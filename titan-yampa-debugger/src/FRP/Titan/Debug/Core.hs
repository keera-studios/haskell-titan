{-# LANGUAGE AllowAmbiguousTypes               #-}
{-# LANGUAGE FlexibleInstances                 #-}
{-# LANGUAGE FunctionalDependencies            #-}
{-# LANGUAGE MultiParamTypeClasses             #-}
{-# LANGUAGE MultiWayIf                        #-}
{-# LANGUAGE ScopedTypeVariables               #-}
{-# LANGUAGE TypeSynonymInstances              #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind -Wall #-}

-- | Replacement of Yampa's @reactimate@ function with more fine-tuned
-- control and debugging capabilities.
module FRP.Titan.Debug.Core
    (
      -- * Debugging
      reactimateControl
      -- ** Debugging commands
    , Command(..)
      -- *** Debugging command queue
    , getCommand
    , pushCommand
      -- ** Debugging preferences
    , Preferences(..)
    , defaultPreferences
      -- ** Debugging predicates
    , Pred(..)
    )
  where

import Control.Applicative       ((<$>))
import Control.Monad
import Control.Monad.IfElse
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Data.Maybe
import Data.Either               (isRight)
import FRP.Yampa                 as Yampa
import FRP.Yampa.InternalCore    (SF(..), SF'(..), sfTF')

import FRP.Titan.Debug.Comm

-- * Interactive reactimation

-- Yampa is based on SFs and SF's. The former is an SF that can be turned on by
-- providing an input signal, the latter is one that is already on, so it also
-- needs to know the time deltas between samples.
--
-- The following two functions implement reactimation (Yampa simulation)
-- based on debug preferences and program configuration.

-- | Start a Yampa program with interactive debugging enabled.
reactimateControl :: forall p a b
                  .  (Read p, Show p, Show a, Read a, Show b, Read b, Pred p a b)
                  => ExternalBridge                 -- ^ Debug: Communication bridge for the interactive GUI
                  -> Preferences                    -- ^ Debug: Debugging preferences
                  -> [Command p]                    -- ^ Debug: List of commands to execute
                  -> IO a                           -- ^ FRP:   Initial sensing action
                  -> (Bool -> IO (DTime, Maybe a))  -- ^ FRP:   Continued sensing action
                  -> (Bool -> b -> IO Bool)         -- ^ FRP:   Rendering/consumption action
                  -> SF a b                         -- ^ FRP:   Signal Function that defines the program
                  -> IO ()
reactimateControl bridge prefs cmds init sense actuate sf =
    evalStateT run simulationState
  where
    simulationState = SimState bridge prefs history cmds yampaIO False
    history         = mkEmptyHistory sf
    yampaIO         = (init, sense, actuate)

-- | Run the debugger continuously until it finishes
run :: (Read p, Show p, Show a, Read a, Show b, Read b, Pred p a b)
    => SimMonad p a b ()
run = get >>= \s -> unless (simFinished s) (reactimateDebugStep >> run)

-- | Process one input command of a Yampa program with interactive debugging enabled.
reactimateDebugStep :: (Read p, Show p, Show a, Read a, Show b, Read b, Pred p a b)
                   => SimMonad p a b ()
reactimateDebugStep = do
  simState <- get
  command  <- simGetCommand
  awhen command $ \command' -> simPrint ("CORE: Executing command " ++ showCommand command')
  case command of

    Nothing   -> return ()

    Just Exit -> modify simFinish

    -- TODO: Print summary information about the history
    Just SummarizeHistory        -> do num <- historyGetNumFrames <$> getSimHistory
                                       simSendMsg ("CurrentHistory " ++ show num)

    -- Jump to a specific frame
    Just (JumpTo n)              -> do running <- (historyIsRunning . simHistory) <$> get
                                       when running $ do
                                         simSendEvent    "CurrentFrameChanged"
                                         simModifyHistory (`historyJumpTo` n)
                                         hPushCommand Redo

    -- Discard all future after a specific frame
    Just (DiscardFuture n)       -> do simSendEvent    "CurrentFrameChanged"
                                       simSendEvent    "HistoryChanged"
                                       nframe <- (historyGetCurrentFrame . simHistory) <$> get
                                       simModifyHistory (`historyDiscardFuture` n)
                                       when (n >= nframe) $ hPushCommand Redo

    -- Jump one step back in the simulation
    Just (TravelToFrame n)       -> do running <- (historyIsRunning . simHistory) <$> get
                                       when running $ do
                                         p0 <- getPos <$> getSimHistory
                                         simPrint ("TravelTo: Traveling to " ++ show n ++ ", current frame is " ++ show p0)
                                         if | p0 == n -> hPushCommand Pause
                                            | p0 <  n -> hPushCommand (TravelToFrame n) >> hPushCommand Step
                                            | p0 >  n -> hPushCommand (TravelToFrame n) >> hPushCommand SkipBack

    Just SkipBack                -> do running <- (historyIsRunning . simHistory) <$> get
                                       when running $ do
                                         p0 <- getPos <$> getSimHistory
                                         simPrint ("SB: The current frame position before modifying history is " ++ show p0)
                                         simModifyHistory historyBack
                                         simSendEvent    "CurrentFrameChanged"
                                         p1 <- getPos <$> getSimHistory
                                         simPrint ("SB: The current frame position after modifying history is " ++ show p1)
                                         l  <- (length.snd.fromJust.getInputHistory) <$> getSimHistory
                                         simPrint ("SB: The number of recorded inputs after modifying history is " ++ show l)
                                         hPushCommand Redo

    -- Re-execute the last step
    Just Redo                    -> do (a0, mdt, sfc) <- historyGetCurFrame <$> getSimHistory
                                       let (sf', b0) = case (mdt, sfc) of
                                                         (_,       Just (Left  sf0)) -> sfTF  sf0 a0
                                                         (Just dt, Just (Right sf1)) -> sfTF' sf1 dt a0

                                       showInput <- (dumpInput . simPrefs) <$> get
                                       when showInput $ simPrint ("CORE: Redo from input " ++ show a0)

                                       last <- simActuate True b0
                                       when last (modify simFinish)


    -- TODO: Skip cycle while sensing the input
    -- Should the input be used as new last input?
    Just SkipSense               -> do running <- (historyIsRunning . simHistory) <$> get
                                       a <- if running then snd <$> simSense1 False else Just <$> simSense

                                       showInput <- (dumpInput . simPrefs) <$> get
                                       when showInput $ simPrint ("CORE: Skip with input " ++ show a)

                                       simSendEvent    "CurrentFrameChanged"

    -- Simulate one step forward
    Just Step                    -> do void stepG

    -- Simulate until a predicate on the input and output holds
    Just (StepUntil p)           -> do (a', dt, b') <- stepG

                                       cond <- checkCond p dt a' b'
                                       unless cond $ hPushCommand (StepUntil p)

    -- Skip steps until a predicate on the input and output holds
    Just (SkipUntil p)           -> do (a', dt, b') <- skipG

                                       cond <- checkCond p dt a' b'
                                       unless cond $ hPushCommand (SkipUntil p)

                                       -- TODO Potential bug here: it could simulate too much!
                                       -- If the condition is not met, it will not "actuate",
                                       -- and so it will not check whether it should have stopped.
                                       last <- if cond then simActuate True b' else return False

                                       -- TODO: Potential bug: should stop, but not exit
                                       when last (modify simFinish)

    -- Simulate indefinitely
    Just Play                    -> do void stepG
                                       commandQ <- getSimCommands
                                       unless (any stopPlayingCommand commandQ) $ hAppendCommand Play

    Just Pause                   -> return ()

    Just DeleteTrace             -> do simEmptyHistory
                                       simSendEvent "CurrentFrameChanged"
                                       simSendEvent "HistoryChanged"

    Just (LoadTraceFromString s) -> do simPrint "CORE: Loading Trace from String"
                                       case maybeRead s of
                                         Nothing -> simPrint "CORE: Could not read a trace"
                                         Just s  -> do simPrint "CORE: Replacing history"
                                                       simReplaceHistory s

    Just (IOSense f)             -> do running <- (historyIsRunning . simHistory) <$> get
                                       if running
                                         then do
                                           (dt, ma') <- simSense1  False
                                           history   <- getSimHistory
                                           -- Unsafe fromJust use
                                           let a' = fromMaybe (fromJust $ getLastInput history) ma'

                                           showInput <- (dumpInput . simPrefs) <$> get
                                           when showInput $ simPrint $ "CORE: IOSense " ++ show a'

                                           simModifyHistory (\h -> historyReplaceInputDTimeAt h f dt a')
                                         else do
                                           a         <- simSense

                                           showInput <- (dumpInput . simPrefs) <$> get
                                           when showInput $ simPrint $ "CORE: IOSense " ++ show a

                                           simModifyHistory (\h -> historyReplaceInputAt h f a)

    Just (GetTrace)              -> do simTrace <- simGetTrace
                                       simPrint (show simTrace)
                                       simSendMsg (show (show <$> simTrace))

    Just (GetInput f)            -> do running <- (historyIsRunning . simHistory) <$> get
                                       if running
                                         then do e <- (`historyGetInput` f) <$> getSimHistory
                                                 simSendMsg (show (show <$> e))
                                         else simSendMsg "Nothing"

    Just (SetInput f i)          -> do case maybeRead i of
                                         Nothing -> return ()
                                         Just a  -> simModifyHistory (\h -> historyReplaceInputAt h f a)

    Just (GetGTime f)            -> do e <- (`historyGetGTime` f) <$> getSimHistory
                                       simPrint $ "CORE: Want to send GTime for frame " ++ show f ++ ", which is " ++ show e
                                       simSendMsg (show e)

    Just (GetDTime f)            -> do e <- (`historyGetDTime` f) <$> getSimHistory
                                       simSendMsg (show e)

    Just (GetMaxTime)            -> do e <- historyGetMaxTime <$> getSimHistory
                                       simPrint $ "CORE: Want to send Max time, which is " ++ show e
                                       simSendMsg $ "MaxTime " ++ show e

    Just (SetDTime f dtS)        -> do case maybeRead dtS of
                                         Nothing -> return ()
                                         Just dt -> simModifyHistory (\h -> historyReplaceDTimeAt h f dt)

    Just GetCurrentTime          -> do num <- historyGetCurrentTime <$> getSimHistory
                                       simSendMsg  ("CurrentTime " ++ show num)
                                       simPrint ("CORE: Sending current time " ++ show num)

    Just GetCurrentFrame         -> do num <- ((\x -> x - 1) . historyGetCurrentFrame) <$> getSimHistory
                                       simSendMsg  ("CurrentFrame " ++ show num)
                                       simPrint ("CORE: Sending current frame " ++ show num)

    Just (SetPrefDumpInput b)    -> do modify (\s -> s { simPrefs = (simPrefs s) { dumpInput = b } })

    Just GetPrefDumpInput        -> do dump <- (dumpInput . simPrefs) <$> get
                                       simSendMsg ("DumpInput " ++ show dump)

    Just Ping                    -> do simSendMsg "Pong"
                                       simSendEvent "PingSent"

    Just c                       -> do simSendEvent ("Got " ++ show c ++ ", dunno what to do with it")
  where

    -- step0 :: IO (a, SF' a b, b)
    step0 = do
      -- Step
      simState <- get
      history  <- getSimHistory
      a0 <- simSense
      when (dumpInput (simPrefs simState)) $ simPrint $ "CORE: Input: " ++ show a0

      let sf       = fromLeft (getCurSF history)
          tf0      = sfTF sf
          (sf',b0) = tf0 a0
      _ <- simActuate  True b0
      -- TODO Potential bug here: it could simulate too much!
      simSendEvent "CurrentFrameChanged"
      simSendEvent "HistoryChanged"
      simModifyHistory (const (mkHistory (a0, sf) sf'))

      return (a0, b0)

    -- skip0 :: IO (a, SF' a b, b)
    skip0 = do
      simState <- get
      history  <- getSimHistory
      a0 <- simSense
      when (dumpInput (simPrefs simState)) $ simPrint $ "CORE: Input: " ++ show a0

      let sf   = fromLeft (getCurSF history)
          tf0  = sfTF sf
          (sf',b0) = tf0 a0
      -- TODO Potential bug here: it could simulate too much!
      simSendEvent "CurrentFrameChanged"
      simSendEvent "HistoryChanged"
      simModifyHistory (const (mkHistory (a0, sf) sf'))
      return (a0, b0)

    stepRR stF = do
      simState <- get
      (a', dt, sf', b') <- stF
      p0 <- getPos <$> getSimHistory
      simPrint ("The current frame position before modifying history is " ++ show p0)
      simModifyHistory (`historyRecordFrame1` (a', dt, sf'))
      p1 <- getPos <$> getSimHistory
      simPrint ("The current frame position after modifying history is " ++ show p1)
      l  <- (length.snd.fromJust.getInputHistory) <$> getSimHistory
      simPrint ("The number of recorded inputs after modifying history is " ++ show l)

      when (dumpInput (simPrefs simState)) $ simPrint $ "CORE: Input " ++ show a'
      simSendEvent "CurrentFrameChanged"
      simSendEvent "HistoryChanged"
      return (a', Just dt, b')

    step1 = do
      (dt, ma')    <- simSense1  False

      history      <- getSimHistory
      let a'       = fromMaybe (fromJust $ getLastInput history) ma' -- unsafe fromJust
          sf       = fromRight $ getCurSF history
          (sf',b') = (sfTF' sf) dt a'
      last         <- simActuate  True b'

      when last (modify simFinish)
      return (a', dt, sf', b')

    skip1 = do
      (dt, ma')    <- simSense1 False

      history      <- getSimHistory
      let a'       = fromMaybe (fromJust $ getLastInput history) ma' -- unsafe fromJust
          sf       = fromRight $ getCurSF history
          (sf',b') = (sfTF' sf) dt a'

      return (a', dt, sf', b')

    stepG = do running <- (historyIsRunning . simHistory) <$> get
               r <- if running then stepRR step1 else (\(a,b) -> (a, Nothing, b)) <$> step0
               -- simSendMsg "StepDone"
               return r

    skipG = do running <- (historyIsRunning . simHistory) <$> get
               r <- if running then stepRR skip1 else (\(a,b) -> (a, Nothing, b)) <$> skip0
               -- simSendMsg "SkipDone"
               return r

    checkCond p dt a0 b0 = do
      simState <- get
      -- Check condition
      let cond = evalPred p dt a0 b0
      when cond $ do
        simPrint ("CORE: Condition became true, with " ++ show (dt, a0) ++ " (" ++ show b0 ++ ")")
        simSendEvent  "ConditionMet"
      return cond

-- * Simulation State

data SimState p a b = SimState
  { simBridge   :: ExternalBridge
  , simPrefs    :: Preferences
  , simHistory  :: History a b
  , simCommands :: [Command p]
  , simOps      :: SimOps a b
  , simFinished :: Bool
  }

getSimHistory :: SimMonad p a b (History a b)
getSimHistory = simHistory <$> get

getSimCommands :: SimMonad p a b [Command p]
getSimCommands = simCommands <$> get

simPrint :: String -> SimMonad p a b ()
simPrint msg = get >>= \simState -> lift $ ebPrint (simBridge simState) msg

simSendMsg :: String -> SimMonad p a b ()
simSendMsg msg = get >>= \simState -> lift $ ebSendMsg (simBridge simState) msg

simSendEvent :: String -> SimMonad p a b ()
simSendEvent msg = get >>= \simState -> lift $ ebSendEvent (simBridge simState) msg

-- | SimOps represents the sensing and consumption actions used to animate/reactimate
--   a Yampa program:
--
--   * Initial sensing action
--
--   * Continued sensing action
--
--   * Rendering/consumption action

type SimOps a b = (IO a, Bool -> IO (DTime, Maybe a), Bool -> b -> IO Bool)

simSense :: SimMonad p a b a
simSense = get >>= \s -> let (op, _, _) = simOps s in lift op

simSense1 :: Bool -> SimMonad p a b (DTime, Maybe a)
simSense1 b = get >>= \s -> let (_, op, _) = simOps s in lift (op b)

simActuate :: Bool -> b -> SimMonad p a b Bool
simActuate c b = get >>= \s -> let (_, _, op) = simOps s in lift (op c b)

simFinish :: SimState p a b -> SimState p a b
simFinish simState = simState { simFinished = True }

type SimMonad p a b = StateT (SimState p a b) IO

-- | Obtain a command from the command queue, polling the communication
--   bridge if the queue is empty.
simGetCommand :: (Read p, Show p, Show a, Read a, Show b, Read b, Pred p a b)
              => SimMonad p a b (Maybe (Command p))
simGetCommand = do
  simState <- get
  (c, cms) <- lift $ getCommand (simBridge simState) (simCommands simState)
  put (simState { simCommands = cms })
  return c

simEmptyHistory :: SimMonad p a b ()
simEmptyHistory = do
  sf0 <- historyGetSF0
  modify $ \simState -> simState { simHistory = mkEmptyHistory sf0 }

simReplaceHistory :: (a, [(DTime, a)]) -> SimMonad p a b ()
simReplaceHistory (a0, as) = do
  sf0     <- historyGetSF0
  let history = History (Just (a0, as))
                        (sf0, [])
                        (-1) (Left sf0) Nothing
  modify $ \simState -> simState { simHistory = history }

simGetTrace :: SimMonad p a b (Maybe (a, [(DTime, a)]))
simGetTrace = getInputHistory <$> getSimHistory

historyGetSF0 :: SimMonad p a b (SF a b)
historyGetSF0 = (fst . getSFHistory) <$> getSimHistory

data History a b = History
  -- { getHistory :: (Maybe (a, Maybe (SF a b)), [(a, DTime, Maybe (SF' a b))])
  { getInputHistory :: Maybe (Stream a (DTime, a))
  , getSFHistory    :: Stream (SF a b) (SF' a b)
  , getPos          :: Int
  , getCurSF        :: Either (SF a b) (SF' a b)
  , getLastInput    :: Maybe a
  }

type Stream a a' = (a, [a'])

simModifyHistory :: (History a b -> History a b) -> SimMonad p a b ()
simModifyHistory f = do
  history <- f <$> getSimHistory
  modify $ \simState -> simState { simHistory = history }


-- * Commands

-- | An interactive, debugging command.
data Command p = Step                       -- ^ Control: Execute a complete simulation cycle
               | StepUntil p                -- ^ Control: Execute cycles until a predicate holds
               | SkipUntil p                -- ^ Control: Skip cycles until a predicate holds
               | SkipSense                  -- ^ Control: Skip cycle while sensing the input
               | Redo                       -- ^ Control: Re-execute the last step
               | SkipBack                   -- ^ Control: Jump one step back in the simulation
               | JumpTo Int                 -- ^ Control: Jump to a specific frame
               | TravelToFrame Int          -- ^ Control: Simulate up to a particular frame   (not implemented yet)
               | DiscardFuture Int          -- ^ Control: Simulate up to a particular frame   (not implemented yet)
               | Exit                       -- ^ Control: Stop the simulation and exit the program
               | Play                       -- ^ Control: Start executing normally
               | Pause                      -- ^ Control: Pause the simulation
               | Stop                       -- ^ Control: Stop the simulation
               | LoadTraceFromFile String   -- ^ Control: Load the Trace from a file (not implemented yet)
               | LoadTraceFromString String -- ^ Control: Load the Trace from a string (not implemented yet)
               | DeleteTrace                -- ^ Control: Discard the whole history
               | IOSense Int                -- ^ Control: Sense input                  (not implemented yet)
               | GetInput Int               -- ^ Info: Obtain input at a particular frame
               | SetInput Int String        -- ^ Info: Change input at a particular frame
               | GetGTime Int               -- ^ Info: Obtain dtime at a particular frame
               | GetDTime Int               -- ^ Info: Obtain dtime at a particular frame
               | SetDTime Int String        -- ^ Info: Change dtime at a particular frame
               | GetCurrentFrame            -- ^ Info: Obtain the current frame
               | GetCurrentTime             -- ^ Info: Obtain the current time
               | GetMaxTime                 -- ^ Info: Obtain the last time that has been explored
               | GetTrace                   -- ^ Info: Obtain input at a particular frame
               | SummarizeHistory           -- ^ Info: Print summary information about the history
               | SetPrefDumpInput Bool      -- ^ Preferences: Alter simulation preferences
               | GetPrefDumpInput           -- ^ Preferences: Obtain simulation preferences
               | Ping                       -- ^ Debugging: send a pong back to the GUI
 deriving (Eq, Read, Show)

-- | Convenince to show commands in the debug log
showCommand (LoadTraceFromString s) = "LoadTraceFromString <length: "  ++ show (length s) ++ " chars>"
showCommand c                       = show c

-- True if the command should make the simulator stop playing
stopPlayingCommand :: Command p -> Bool
stopPlayingCommand (Step)                  = True
stopPlayingCommand (StepUntil p)           = True
stopPlayingCommand (SkipUntil p)           = True
stopPlayingCommand (SkipSense)             = True
stopPlayingCommand (Redo)                  = True
stopPlayingCommand (SkipBack)              = True
stopPlayingCommand (JumpTo _)              = True
stopPlayingCommand (TravelToFrame _)       = True
stopPlayingCommand (DiscardFuture _)       = True
stopPlayingCommand (Exit)                  = True
stopPlayingCommand (Play)                  = False
stopPlayingCommand (Pause)                 = True
stopPlayingCommand (Stop)                  = True
stopPlayingCommand (LoadTraceFromFile _)   = True
stopPlayingCommand (LoadTraceFromString _) = True
stopPlayingCommand (DeleteTrace)           = True
stopPlayingCommand (IOSense _)             = True
stopPlayingCommand (GetInput _ )           = False
stopPlayingCommand (SetInput _ _)          = False
stopPlayingCommand (GetGTime _ )           = False
stopPlayingCommand (GetDTime _ )           = False
stopPlayingCommand (SetDTime _ _)          = False
stopPlayingCommand (GetCurrentFrame)       = False
stopPlayingCommand (GetCurrentTime)        = False
stopPlayingCommand (GetTrace)              = False
stopPlayingCommand (SummarizeHistory)      = False
stopPlayingCommand (SetPrefDumpInput _)    = False
stopPlayingCommand (GetPrefDumpInput)      = False
stopPlayingCommand (Ping)                  = False
stopPlayingCommand (GetMaxTime)            = False

hPushCommand :: Command p -> SimMonad p a b ()
hPushCommand cmd = modify
  (\simState -> simState { simCommands = pushCommand (simCommands simState) cmd })

hAppendCommand :: Command p -> SimMonad p a b ()
hAppendCommand cmd = modify
  (\simState -> simState { simCommands = appendCommand (simCommands simState) cmd })


-- ** Command Queue

-- | Obtain a command from the command queue, polling the communication
--   bridge if the queue is empty.
getCommand :: (Read a, Show a) => ExternalBridge -> [a] -> IO (Maybe a, [a])
-- getCommand bridge (c:cs) = return (Just c, cs)
getCommand bridge cmds = do
  mLines <- filter (not . null) <$> getAllMessages bridge
  let cmLines = map maybeRead mLines
      cLines  = catMaybes cmLines
  unless (null mLines) $ do
    ebPrint bridge (show mLines)
    ebPrint bridge (show cmLines)
  case cmds ++ cLines of
    []     -> return (Nothing, [])
    (c:cs) -> return (Just c, cs)

-- | Place one command on the top of the queue.
pushCommand :: [a] -> a -> [a]
pushCommand cs c = c:cs

-- | Place one command on the top of the queue.
appendCommand :: [a] -> a -> [a]
appendCommand cs c = cs ++ [c]

-- * Execution History

-- INV: forall h . isNothing (fst (getHistory h)) \/ isNothing (fst (getFuture h))
-- INV: forall h . not (null (getHistory h)) ==> isNothing (fst (getFuture h))

-- ** Construction

-- | Create empty history pending to run a signal function
mkEmptyHistory :: SF a b -> History a b
mkEmptyHistory sf = History Nothing (sf, []) 0 (Left  sf) Nothing

-- | Create empty history with an initial sample and sf, and a next SF'
mkHistory :: (a, SF a b) -> SF' a b -> History a b
mkHistory (a0, sf0) sf' =
  History (Just (a0, [])) (sf0, [sf']) 1 (Right sf') (Just a0)

-- | Determine if history is currently pointing to a running SF.
historyIsRunning :: History a b -> Bool
historyIsRunning history = ((>0) . getPos) (history)

-- | Replace the input for a given frame/sample
historyReplaceInputAt :: History a b -> Int -> a -> History a b
historyReplaceInputAt history f a
    | ns < f    = history
    | f == 0    = if isNothing hs
                    then history
                    else history { getInputHistory = Just (a, ps)
                                 , getSFHistory    = ((\(x,y) -> (x, [])) $ getSFHistory history)
                                 }
    | otherwise = history { getInputHistory = Just (a0, appAt (f-1) (\(dt, _) -> (dt, a)) ps)
                          , getSFHistory    = ((\(x,y) -> (x, take f y)) $ getSFHistory history)
                          }
  where
    hs = getInputHistory history
    Just (a0, ps) = hs
    ns = maybe 0 (length.snd) hs

-- | Replace the time for a given frame/sample
historyReplaceDTimeAt :: History a b -> Int -> DTime -> History a b
historyReplaceDTimeAt history f dt =
  let Just (a0, ps) = getInputHistory history
      dts                  = 0 : map (\(dt,_) -> dt) ps
  in if length dts >= f
       then history
       else if f == 0
              then history
              else history { getInputHistory = Just (a0, appAt (f-1) (\(_,a) -> (dt, a)) ps)
                           , getSFHistory    = ((\(x,y) -> (x, take f y)) $ getSFHistory history)
                           }

-- | Replace the input and the time for a given frame/sample
historyReplaceInputDTimeAt :: History a b -> Int -> DTime -> a -> History a b
historyReplaceInputDTimeAt history f dt a =
  let (Just (a0, ps)) = getInputHistory history
      as              = a0 : map (\(_, a) -> a) ps
  in if length as >= f
       then history
       else if f == 0
              then history { getInputHistory = Just (a, ps)
                           , getSFHistory    = ((\(x,y) -> (x, [])) $ getSFHistory history)}
              else history { getInputHistory = Just (a0, appAt (f-1) (\(_,_) -> (dt, a)) ps)
                           , getSFHistory    = ((\(x,y) -> (x, take f y)) $ getSFHistory history)}

-- | Get the total time at a given point/frame
historyGetMaxTime :: History a b -> DTime
historyGetMaxTime history =
  case getInputHistory history of
    Nothing       -> 0
    Just (a0, ps) -> sum $ map (\(dt,_) -> dt) ps

-- | Get the total time at a given point/frame
historyGetGTime :: History a b -> Int -> Maybe DTime
historyGetGTime history f =
  case getInputHistory history of
    Nothing       -> Nothing
    Just (a0, ps) -> let dts = 0 : map fst ps
                         l   = length dts
                         e   = if l < f then Nothing else Just (sum (drop (l-f) dts))
                     in e

-- | Get the time delta for a given frame
historyGetDTime :: History a b -> Int -> Maybe DTime
historyGetDTime history f =
  case getInputHistory history of
    Nothing       -> Nothing
    Just (a0, ps) -> let dts = 0 : map fst ps
                         e   = if length dts < f || f < 0 then Nothing else Just (dts !! f)
                     in e

-- | Get the input for a given frame
historyGetInput :: History a b -> Int -> Maybe a
historyGetInput history f =
  case getInputHistory history of
    Nothing       -> Nothing
    Just (a0, ps) -> let as = a0 : map snd ps
                         e  = if length as < f  || f < 0then Nothing else Just (as !! f)
                     in e

-- | Get the time for the current frame
historyGetCurrentTime :: History t b -> DTime
historyGetCurrentTime history =
  case getInputHistory history of
    Just (a0, ps)  -> sum $ map (\(dt,_) -> dt) (take (getPos history) ps)
    Nothing        -> 0

-- | Get the current frame number.
historyGetCurrentFrame :: History a b -> Int
historyGetCurrentFrame history =  getPos history

-- | Record a running frame
historyRecordFrame1 :: History a b -> (a, DTime, SF' a b) -> History a b
historyRecordFrame1 history (a', dt, sf') = historySF
 where
   historyInput = case getInputHistory history of
                    Nothing       -> history
                    Just (a0, ps) -> if | pos > 0 && pos < length ps -> history { getInputHistory = Just (a0, appAt pos (const (dt, a')) ps) }
                                        | pos > 0                    -> history { getInputHistory = Just (a0, (dt, a'):ps) }
                                        | otherwise                  -> history

   historySF = let (s0, ss) = getSFHistory historyInput
               in if getPos history <= 0
                    then historyInput
                    else historyInput { getSFHistory = (s0, take (getPos history) ss ++ [sf'])
                                      , getPos       = pos + 1
                                      }
   pos = getPos history

-- | Get the total number of frames
historyGetNumFrames :: History t b -> Int
historyGetNumFrames history =
  case getInputHistory history of
    Just (a0, ps) -> length ps + 1
    Nothing       -> 0

-- | Get the current frame info
--
-- TODO: Partial function
historyGetCurFrame :: History a b -> (a, Maybe DTime, Maybe (Either (SF a b) (SF' a b)))
historyGetCurFrame history =
  case curInput of
    Just (Left a0)       -> (a0, Nothing, curSF)
    Just (Right (dt, a)) -> (a,  Just dt, curSF)
    Nothing              -> error "No current frame"
 where
   curInput = (`getSampleAt` (getPos history)) =<< getInputHistory history
   curSF    = (`getSampleAt` (getPos history)) (getSFHistory history)

getSampleAt :: Stream a a' -> Int -> Maybe (Either a a')
getSampleAt (s0, ss) 0 = Just (Left s0)
getSampleAt (s0, ss) n
  | n <= length ss = Just (Right (ss!!(n-1)))
  | otherwise      = Nothing

-- | Move one step back in history
historyBack :: History a b -> History a b
historyBack history = history { getPos = max 0 (getPos history - 1)}

  -- case getHistory history of
  --   (Just (a0, sf0), _:(_a,_dt, sf'):prevs@((lastInput, _, _):_)) -> (Just $ History (Just (a0, sf0), prevs) (Right sf') (Just lastInput), Right (sf', lastInput))
  --   (Just (a0, sf0), _:(_a,_dt, sf'):[])                          -> (Just $ History (Just (a0, sf0), [])    (Right sf') (Just a0),        Right (sf', a0))
  --   (Just (a0, sf0), _:[])                                        -> (Just $ History (Just (a0, sf0), [])    (Left sf0)  Nothing,          Left sf0)
  --   (Just (a0, sf0), [])                                          -> (Just $ History (Nothing, [])           (Left sf0)  Nothing,          Left sf0)
    -- TODO: undefined
    -- (Nothing, [])                                                 -> (Just $ history,                                                      getCurSF history)

-- | Jump to a specific frame number.
-- historyJumpTo :: History a b -> Int -> History a b
-- historyJumpTo history n =
--   case getHistory history of
--     (Nothing,_)          -> history
--     (Just (a0, sf0), ps) ->
--       if length ps + 1 > n
--         then if n > 0
--                then let ((_a,_dt, sf'):prevs@((lastInput, _, _):_)) = takeLast n ps
--                     in History (Just (a0, sf0), prevs) n (Right (fromJust sf')) (Just lastInput)
--                else mkEmptyHistory (fromMaybe (fromLeft (getCurSF history)) sf0)
--         else history

historyJumpTo :: History a b -> Int -> History a b
historyJumpTo history n =
  case getInputHistory history of
    Nothing       -> history
    Just (a0, ps) ->
      if length ps + 1 > n
        then history { getPos = n }
        else history

-- | Discard the future after a given frame.
historyDiscardFuture :: History a b -> Int -> History a b
historyDiscardFuture history n =
  case getInputHistory history of
    Nothing       -> history
    Just (a0, ps) ->
      if length ps + 1 > n
        then if n > 0
               then history { getInputHistory = Just (a0, take n ps)
                            , getSFHistory    = (\(s0, ss) -> (s0, take n ss)) (getSFHistory history)
                            , getPos          = min n (getPos history)
                            }
               else History { getInputHistory = Just (a0, [])
                            , getSFHistory    = (\(s0, ss) -> (s0, [])) (getSFHistory history)
                            , getPos          = 0
                            }
        else history


-- * Simulation preferences

-- | Debugging preferences.
data Preferences = Preferences
  { dumpInput :: Bool -- ^ Dump inputs to local log at every cycle (on simulation machine)
  }

-- | Default simulation preferences that do not dump the input to the log
--   every cycle.
defaultPreferences :: Preferences
defaultPreferences = Preferences
  { dumpInput = False }

-- * Debugging predicates

-- | A notion of temporal point-wise (time-wise) predicate to be tested
-- during a simulation point. It needs to be something we can read
-- from the GUI bridge so that we can interactively read commands
-- from the user and test them.

-- TODO: Possibly use this:
-- https://hackage.haskell.org/package/hint

class Read p => Pred p i o | p -> i, p -> o where
  -- | Evaluate a predicate for a given input sample and a given output.
  evalPred :: p -> Maybe DTime -> i -> o -> Bool

-- ** Utility functions

-- *** Lists

-- takeLast n l = reverse $ take n $ reverse l

appAt :: Int -> (a -> a) -> [a] -> [a]
appAt _ f [] = []
appAt 0 f (x:xs) = f x : xs
appAt n f (x:xs) = x : appAt (n-1) f xs

-- *** Maybe

maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads

--- *** Either

-- TODO: Remove
fromLeft :: Either a b -> a
fromLeft (Left a) = a

-- TODO: Remove
fromRight :: Either a b -> b
fromRight (Right b) = b
