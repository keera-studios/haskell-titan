{-# LANGUAGE FlexibleInstances                 #-}
{-# LANGUAGE TypeSynonymInstances              #-}
{-# LANGUAGE MultiParamTypeClasses             #-}
{-# LANGUAGE FunctionalDependencies            #-}
{-# LANGUAGE ScopedTypeVariables               #-}
{-# LANGUAGE AllowAmbiguousTypes               #-}
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
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Data.Maybe
import Data.Either               (isRight)
import FRP.Yampa                 as Yampa
import FRP.Yampa.InternalCore    (SF(..), SF'(..), sfTF', DTime)

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
  let history = mkEmptyHistory sf
  in evalStateT run (SimState bridge prefs history cmds (init, sense, actuate) False)

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

type SimOps a b = (IO a, (Bool -> IO (DTime, Maybe a)), (Bool -> b -> IO Bool))

  -- IO a                                      -- ^ FRP:   Initial sensing action
  -- (Bool -> IO (DTime, Maybe a))             -- ^ FRP:   Continued sensing action
  -- (Bool -> b -> IO Bool)                    -- ^ FRP:   Rendering/consumption action

simSense :: SimOps a b -> IO a
simSense (op, _, _) = op

simSense1 :: SimOps a b -> Bool -> IO (DTime, Maybe a)
simSense1 (_, op, _) = op

simActuate :: SimOps a b -> Bool -> b -> IO Bool
simActuate (_, _, op) = op

simFinish :: SimState p a b -> SimState p a b
simFinish simState = simState { simFinished = True }

type SimMonad p a b = StateT (SimState p a b) IO 

run :: (Read p, Show p, Show a, Read a, Show b, Read b, Pred p a b)
    => SimMonad p a b ()
run = get >>= \s -> unless (simFinished s) (dispatchCommand >> run)

dispatchCommand :: (Read p, Show p, Show a, Read a, Show b, Read b, Pred p a b)
                => SimMonad p a b ()
dispatchCommand =  do
  running <- (historyIsRunning . simHistory) <$> get
  if running
    then reactimateControl1
    else reactimateControl0

-- | Start a Yampa program with interactive debugging enabled.
reactimateControl0 :: (Read p, Show p, Show a, Read a, Show b, Read b, Pred p a b)
                   => SimMonad p a b ()
reactimateControl0 = do
  simState <- get
  command <- simGetCommand

  -- Process one command and loop
  case command of

    Nothing                  -> return ()
    Just Exit                -> modify simFinish

    -- Jump one step back in the simulation
    Just SkipBack            -> return ()

    -- Re-execute the last step
    Just Redo                -> return ()

    -- TODO: Skip cycle while sensing the input
    Just SkipSense           -> do a0 <- lift $ simSense (simOps simState)
                                   when (dumpInput (simPrefs simState)) $ simPrint $ show a0

                                   let myInit = do (_,ma') <- simSense1 (simOps simState) False
                                                   return $ fromMaybe a0 ma'

                                   simSendEvent  "CurrentFrameChanged"

                                   put (simState { simOps = (myInit, simSense1 (simOps simState), simActuate (simOps simState)) })

    -- TODO: Jump to a specific frame
    Just (JumpTo n)          -> do simSendEvent  "CurrentFrameChanged"

    -- Simulate indefinitely
    Just Play                 -> do (a0, sf', _) <- step0
                                    commandQ <- getSimCommands
                                    unless (any stopPlayingCommand commandQ) (hAppendCommand Play)
                                    sf <- (fromLeft . getCurSF) <$> getSimHistory
                                    let history'   = mkHistory (a0, sf) sf' a0
                                    put (simState { simHistory = history' })

    Just Pause                -> return () 

    -- Simulate one step forward
    Just Step                 -> do (a0, sf', _) <- step0
                                    sf <- (fromLeft . getCurSF) <$> getSimHistory
                                    let history'   = mkHistory (a0, sf) sf' a0
                                    put (simState { simHistory = history' })

    -- Simulate until a predicate on the input and output holds
    Just (StepUntil p)        -> do (a0, sf', b0) <- step0
                                    cond          <- checkCond p Nothing a0 b0
                                    unless cond $ hPushCommand (StepUntil p)
                                    sf <- (fromLeft . getCurSF) <$> getSimHistory
                                    let history'   = mkHistory (a0, sf) sf' a0

                                    -- Continue
                                    -- TODO Potential bug here: it could simulate too much!
                                    modify (\simState'' -> simState'' { simHistory = history' })

    -- Skip steps until a predicate on the input and output holds
    Just (SkipUntil p)        -> do (a0, sf', b0) <- skip0
                                    cond          <- checkCond p Nothing a0 b0
                                    unless cond $ hPushCommand (SkipUntil p)
                                    sf <- (fromLeft . getCurSF) <$> getSimHistory
                                    let history'   = mkHistory (a0, sf) sf' a0

                                    -- TODO Potential bug here: it could simulate too much!
                                    modify (\simState'' -> simState'' { simHistory = history' })

    Just (GetInput _)         -> do simSendMsg  ("Nothing")

    Just GetCurrentTime       -> do simSendMsg  ("CurrentTime " ++ show 0)

    Just GetCurrentFrame      -> do simSendMsg  ("CurrentFrame " ++ show 0)

    Just SummarizeHistory     -> do simPrint  ("CurrentHistory 0")

    Just (SetPrefDumpInput b) -> do modify (\s -> s { simPrefs = (simPrefs s) { dumpInput = b } })

    Just GetPrefDumpInput     -> do simSendMsg ("DumpInput " ++ show (dumpInput (simPrefs simState)))

    Just Ping                 -> do simSendMsg  "Pong"
                                    simSendEvent    "PingSent"

    Just c                    -> do simSendEvent  ("Got " ++ show c ++ ", dunno what to do with it")
  where
    -- step0 :: IO (a, SF' a b, b)
    step0 = do
      -- Step
      simState <- get
      history  <- getSimHistory
      a0 <- lift $ simSense (simOps simState)
      when (dumpInput (simPrefs simState)) $ simPrint $ show a0

      let sf       = fromLeft (getCurSF history)
          tf0      = sfTF sf
          (sf',b0) = tf0 a0
      _ <- lift $ simActuate (simOps simState) True b0
      simSendEvent   "CurrentFrameChanged"
      return (a0, sf', b0)

    -- skip0 :: IO (a, SF' a b, b)
    skip0 = do
      simState <- get
      history  <- getSimHistory
      a0 <- lift $ simSense (simOps simState)
      when (dumpInput (simPrefs simState)) $ simPrint $ show a0

      let sf   = fromLeft (getCurSF history)
          tf0  = sfTF sf
          (sf',b0) = tf0 a0
      simSendEvent  "CurrentFrameChanged"
      return (a0, sf', b0)

    -- checkCond :: (Show a, Show b, Pred p a b)
    --           => p -> Maybe DTime -> a -> b -> IO Bool
    checkCond p dt a0 b0 = do
      simState <- get
      -- Check condition
      let cond = evalPred p dt a0 b0
      when cond $ do
        simPrint ("Condition became true, with " ++ show a0 ++ " (" ++ show b0 ++ ")")
        simSendEvent  "ConditionMet"
      return cond

-- | Continue simulating a Yampa program with interactive debugging enabled.
reactimateControl1 :: (Read p, Show p, Show a, Read a, Show b, Read b, Pred p a b)
                   => SimMonad p a b ()
reactimateControl1 = do
  simState <- get
  command <- simGetCommand

  case command of

    Nothing   -> return ()

    Just Exit -> modify simFinish

    -- TODO: Print summary information about the history
    Just SummarizeHistory     -> do num <- historyGetNumFrames <$> getSimHistory
                                    simPrint  ("CurrentHistory " ++ show num)

    -- Jump to a specific frame
    Just (JumpTo n)           -> do simSendEvent    "CurrentFrameChanged"
                                    history <- getSimHistory
                                    case historyJumpTo history n of
                                      (Just history', Nothing) -> do
                                        put (simState { simHistory = history' })
                                      (Just history', Just (Left _)) -> do
                                        hPushCommand Redo
                                        modify (\simState' -> simState' { simHistory = history' })
                                      (Nothing, Just (Right sf0)) ->
                                        put (simState { simHistory = (mkEmptyHistory sf0) })

    -- Discard all future after a specific frame
    Just (DiscardFuture n)    -> do simSendEvent    "CurrentFrameChanged"
                                    history <- getSimHistory
                                    case historyDiscardFuture history n of
                                      (Just history', Nothing) -> do
                                        put (simState { simHistory = history' })
                                      (Just history', Just (Left _)) -> do
                                        hPushCommand Redo
                                        modify (\simState' -> simState' { simHistory = history' })
                                      (Nothing, Just (Right sf0)) ->
                                        put (simState { simHistory = (mkEmptyHistory sf0) })

    -- Jump one step back in the simulation
    Just SkipBack             -> do simSendEvent    "CurrentFrameChanged"
                                    history <- getSimHistory
                                    case historyBack history of
                                      (Just history', Right _) -> do
                                        hPushCommand Redo
                                        modify (\simState' -> simState' { simHistory = history' })
                                      (Nothing, Left sf0) ->
                                        put (simState { simHistory = (mkEmptyHistory sf0) })

    -- Re-execute the last step
    Just Redo                 -> -- put ((simBridge simState) (simPrefs simState) history commandQ' sense actuate sf lastInput)
                                 do (a0, mdt, sfc) <- historyGetCurFrame <$> getSimHistory
                                    let (sf', b0) = case (mdt, sfc) of
                                                      (_,       Left sf0)  -> sfTF  sf0 a0
                                                      (Just dt, Right sf1) -> sfTF' sf1 dt a0

                                    when (dumpInput (simPrefs simState)) $ simPrint $ show a0
                                    last <- lift $ simActuate (simOps simState) True b0
                                    when last (modify simFinish)


    -- TODO: Skip cycle while sensing the input
    -- Should the input be used as new last input?
    Just SkipSense            -> do (_,a)  <- lift $ simSense1 (simOps simState) False
                                    when (dumpInput (simPrefs simState)) $ simPrint $ show a
                                    simSendEvent    "CurrentFrameChanged"

    -- Simulate one step forward
    Just Step                 -> do (a', dt, sf', b', last) <- step1
                                    history <- getSimHistory
                                    let history' = historyRecordFrame1 history (a', dt, sf')

                                    let simState'' = (simState { simHistory = history' })
                                    put simState''
                                    when last (modify simFinish)

    -- Simulate until a predicate on the input and output holds
    Just (StepUntil p)        -> do (a', dt, sf', b', last) <- step1
                                    history <- getSimHistory
                                    let history' = historyRecordFrame1 history (a', dt, sf')

                                    cond <- checkCond p (Just dt) a' b'
                                    unless cond $ hPushCommand (StepUntil p)

                                    let simState'' = (simState { simHistory = history' })
                                    put simState''

                                    when last (modify simFinish)

    -- Skip steps until a predicate on the input and output holds
    Just (SkipUntil p)        -> do (a', dt, sf', b') <- skip1
                                    history <- getSimHistory
                                    let history' = historyRecordFrame1 history (a', dt, sf')

                                    cond <- checkCond p (Just dt) a' b'
                                    unless cond $ hPushCommand (SkipUntil p)

                                    -- TODO Potential bug here: it could simulate too much! If the condition is not
                                    -- met, it will not "actuate", and so it will not check whether it should have stopped.
                                    last <- if cond then lift (simActuate (simOps simState) True b') else return False

                                    let simState'' = (simState { simHistory = history' })
                                    put simState''
                                    when last (modify simFinish)

    -- Simulate indefinitely
    Just Play                 -> do (a', dt, sf', b', last) <- step1
                                    history <- getSimHistory
                                    let history' = historyRecordFrame1 history (a', dt, sf')

                                    commandQ <- getSimCommands
                                    unless (any stopPlayingCommand commandQ) $ hAppendCommand Play

                                    let simState'' = (simState { simHistory = history' })
                                    put simState''
                                    when last (modify simFinish)

    Just Pause                -> return ()

    Just (IOSense f)          -> do (dt, ma') <- lift $ simSense1 (simOps simState) False
                                    -- Unsafe fromJust use
                                    history <- getSimHistory
                                    let a' = fromMaybe (fromJust $ getLastInput history) ma'
                                    when (dumpInput (simPrefs simState)) $ simPrint $ show a'
                                    let history'' = historyReplaceInputDTimeAt history f dt a'
                                    put (simState { simHistory = history'' })

    Just (GetInput f)         -> do e <- (`historyGetInput` f) <$> getSimHistory
                                    simSendMsg  (show e)

    Just (SetInput f i)       -> do case maybeRead i of
                                      Nothing -> return ()
                                      Just a  -> do h' <- (\h -> historyReplaceInputAt h f a) <$> getSimHistory
                                                    put (simState { simHistory = h' })

    Just (GetGTime f)         -> do e <- (`historyGetGTime` f) <$> getSimHistory
                                    simSendMsg (show e)

    Just (GetDTime f)         -> do e <- (`historyGetDTime` f) <$> getSimHistory
                                    simSendMsg (show e)

    Just (SetDTime f dtS)     -> do case maybeRead dtS of
                                      Nothing -> return ()
                                      Just dt -> do h' <- (\h -> historyReplaceDTimeAt h f dt) <$> getSimHistory
                                                    put (simState { simHistory = h' })

    Just GetCurrentTime       -> do num <- historyGetCurrentTime <$> getSimHistory
                                    simSendMsg  ("CurrentTime " ++ show num)

    Just GetCurrentFrame      -> do num <- historyGetCurrentFrame <$> getSimHistory
                                    simSendMsg  ("CurrentFrame " ++ show num)

    Just (SetPrefDumpInput b) -> do modify (\s -> s { simPrefs = (simPrefs s) { dumpInput = b } })

    Just GetPrefDumpInput     -> do simSendMsg ("DumpInput " ++ show (dumpInput (simPrefs simState)))

    Just Ping                 -> do simSendMsg "Pong"
                                    simSendEvent "PingSent"

    other                     -> do lift $ putStrLn $ show other
  where
    step1 = do
      simState <- get
      history  <- getSimHistory
      (dt, ma') <- lift $ simSense1 (simOps simState) False
      let a'       = fromMaybe (fromJust $ getLastInput history) ma' -- unsafe fromJust
          sf       = fromRight $ getCurSF history
          (sf',b') = (sfTF' sf) dt a'
      when (dumpInput (simPrefs simState)) $ simPrint $ show a'

      last <- lift $ simActuate (simOps simState) True b'
      simSendEvent     "CurrentFrameChanged"
      return (a', dt, sf', b', last)

    skip1 = do
      simState <- get
      history  <- getSimHistory
      (dt, ma') <- lift $ simSense1 (simOps simState) False
      let a'       = fromMaybe (fromJust $ getLastInput history) ma' -- unsafe fromJust
          sf       = fromRight $ getCurSF history
          (sf',b') = (sfTF' sf) dt a'

      when (dumpInput (simPrefs simState)) $ simPrint $ show a'
      simSendEvent     "CurrentFrameChanged"
      return (a', dt, sf', b')

    checkCond p dt a0 b0 = do
      simState <- get
      -- Check condition
      let cond = evalPred p dt a0 b0
      when cond $ do
        simPrint ("Condition became true, with " ++ show (dt, a0) ++ " (" ++ show b0 ++ ")")
        simSendEvent  "ConditionMet"
      return cond

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
               | IOSense Int                -- ^ Control: Sense input                  (not implemented yet)
               | GetInput Int               -- ^ Info: Obtain input at a particular frame
               | SetInput Int String        -- ^ Info: Change input at a particular frame
               | GetGTime Int               -- ^ Info: Obtain dtime at a particular frame
               | GetDTime Int               -- ^ Info: Obtain dtime at a particular frame
               | SetDTime Int String        -- ^ Info: Change dtime at a particular frame
               | GetCurrentFrame            -- ^ Info: Obtain the current frame
               | GetCurrentTime             -- ^ Info: Obtain the current time
               | SummarizeHistory           -- ^ Info: Print summary information about the history
               | SetPrefDumpInput Bool      -- ^ Preferences: Alter simulation preferences
               | GetPrefDumpInput           -- ^ Preferences: Obtain simulation preferences
               | Ping                       -- ^ Debugging: send a pong back to the GUI
 deriving (Eq, Read, Show)

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
stopPlayingCommand (IOSense _)             = True
stopPlayingCommand (GetInput _ )           = False
stopPlayingCommand (SetInput _ _)          = False
stopPlayingCommand (GetGTime _ )           = False
stopPlayingCommand (GetDTime _ )           = False
stopPlayingCommand (SetDTime _ _)          = False
stopPlayingCommand (GetCurrentFrame)       = False
stopPlayingCommand (GetCurrentTime)        = False
stopPlayingCommand (SummarizeHistory)      = False
stopPlayingCommand (SetPrefDumpInput _)    = False
stopPlayingCommand (GetPrefDumpInput)      = False
stopPlayingCommand (Ping)                  = False

hPushCommand :: Command p -> SimMonad p a b ()
hPushCommand cmd = modify (\simState -> simState { simCommands = pushCommand (simCommands simState) cmd })

hAppendCommand :: Command p -> SimMonad p a b ()
hAppendCommand cmd = modify (\simState -> simState { simCommands = appendCommand (simCommands simState) cmd })

-- | Obtain a command from the command queue, polling the communication
--   bridge if the queue is empty.
simGetCommand :: (Read p, Show p, Show a, Read a, Show b, Read b, Pred p a b)
              => SimMonad p a b (Maybe (Command p))
simGetCommand = do
  simState <- get
  (c, cms) <- lift $ getCommand (simBridge simState) (simCommands simState)
  put (simState { simCommands = cms })
  return c

-- ** Command Queue

-- | Obtain a command from the command queue, polling the communication
--   bridge if the queue is empty.
getCommand :: (Read a, Show a) => ExternalBridge -> [a] -> IO (Maybe a, [a])
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
data History a b = History
  { getHistory   :: (Maybe (a, SF a b), [(a, DTime, SF' a b)])
  , getCurSF     :: Either (SF a b) (SF' a b)
  , getLastInput :: Maybe a
  }

historyIsRunning :: History a b -> Bool
historyIsRunning history = isRight (getCurSF history)

historyReplaceInputAt history f a =
  let (Just (a0, sf0), ps) = getHistory history
      as              = a0 : map (\(a,_,_) -> a) ps
  in if length as >= f
       then history
       else if f == 0
              then History { getHistory = (Just (a, sf0), ps) }
              else History { getHistory = (Just (a0, sf0), appAt (f-1) (\(_,dt,sf) -> (a, dt, sf)) ps) }

historyReplaceDTimeAt history f dt =
  let (Just (a0, sf0), ps) = getHistory history
      dts             = 0 : map (\(_,dt,_) -> dt) ps
  in if length dts >= f
       then history
       else if f == 0
              then History { getHistory = (Just (a0, sf0), ps) }
              else History { getHistory = (Just (a0, sf0), appAt (f-1) (\(a,_,sf) -> (a, dt, sf)) ps) }

historyReplaceInputDTimeAt history f dt a =
  let (Just (a0, sf0), ps) = getHistory history
      as              = a0 : map (\(a,_,_) -> a) ps
  in if length as >= f
       then history
       else if f == 0
              then History { getHistory = (Just (a, sf0), ps) }
              else History { getHistory = (Just (a0, sf0), appAt (f-1) (\(_,_,sf) -> (a, dt, sf)) ps) }

historyGetGTime history f =
  let (Just (a0, sf0), ps) = getHistory history
      dts             = 0 : map (\(_,dt,_) -> dt) ps
      e               = if length dts >= f then Nothing else Just (sum (take f dts))
  in e

historyGetDTime history f =
  let (Just (a0, sf0), ps) = getHistory history
      dts             = 0 : map (\(_,dt,_) -> dt) ps
      e               = if length dts >= f then Nothing else Just (dts !! f)
  in e

historyGetInput history f =
  let (Just (a0, sf0), ps) = getHistory history
      as = a0 : map (\(a,_,_) -> a) ps
      e  = if length as >= f then Nothing else Just (as !! f)
  in e

historyGetCurrentTime history =
  let (Just (a0, sf0), ps) = getHistory history
      num             = sum $ map (\(_,dt,_) -> dt) ps
  in num

historyGetCurrentFrame history =
  let (Just (a0, sf0), ps) = getHistory history
      num             = length ps
  in num

mkEmptyHistory sf = History (Nothing,[]) (Left  sf) Nothing
mkHistory (a0, sf0) sf' a = History (Just (a0, sf0),[]) (Right sf') (Just a)

historyRecordFrame1 history (a', dt, sf') =
  let (Just (a0, sf0), ps) = getHistory history
  in History (Just (a0, sf0), (a', dt, sf'):ps) (Right sf') (Just a')

historyGetNumFrames history =
  let (Just (a0, sf0), ps) = getHistory history
  in length ps

historyGetCurFrame history =
  case getHistory history of
    (Just (a0, sf0), (an, dt, sfn):prevs) -> (an, Just dt, Right sfn)
    (Just (a0, sf0), [])                  -> (a0, Nothing, Left  sf0)

historyBack history =
  case getHistory history of
    (Just (a0, sf0), _:(_a,_dt, sf'):prevs@((lastInput, _, _):_)) -> (Just $ History (Just (a0, sf0), prevs) (Right sf') (Just lastInput), Right (sf', lastInput))
    (Just (a0, sf0), _:(_a,_dt, sf'):[])                          -> (Just $ History (Just (a0, sf0), [])    (Right sf') (Just a0),        Right (sf', a0))
    (Just (a0, sf0), _:[])                                        -> (Just $ History (Just (a0, sf0), [])    (Left sf0)  Nothing,          Left sf0)
    (Just (a0, sf0), [])                                          -> (Just $ History (Nothing, [])           (Left sf0)  Nothing,          Left sf0)
    -- TODO: undefined
    -- (Nothing, [])                                                 -> (Just $ history,                                                      getCurSF history)

historyJumpTo :: History a b -> Int -> (Maybe (History a b), Maybe (Either (SF' a b, a) (SF a b)))
historyJumpTo history n =
  case getHistory history of
    (Nothing,_)          -> (Just history, Nothing)
    (Just (a0, sf0), ps) -> 
      if (length ps + 1 > n)
        then if n > 0
               then let ((_a,_dt, sf'):prevs@((lastInput, _, _):_)) = takeLast n ps
                    in (Just $ History (Just (a0, sf0), prevs) (Right sf') (Just lastInput), Just $ Left (sf', lastInput))
               else (Nothing, Just $ Right sf0)
        else (Just history, Nothing)

historyDiscardFuture :: History a b -> Int -> (Maybe (History a b), Maybe (Either (SF' a b, a) (SF a b)))
historyDiscardFuture history n =
  case getHistory history of
    (Nothing,_)          -> (Just history, Nothing)
    (Just (a0, sf0), ps) -> 
      if (length ps + 1 > n)
        then if n > 0
               then let ((_a,_dt, sf'):prevs@((lastInput, _, _):_)) = takeLast n ps
                    in (Just $ History (Just (a0, sf0), prevs) (Right sf') (Just lastInput), Just $ Left (sf', lastInput))
               else (Nothing, Just $ Right sf0)
        else (Just history, Nothing)


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

takeLast n l = reverse $ take n $ reverse l

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
