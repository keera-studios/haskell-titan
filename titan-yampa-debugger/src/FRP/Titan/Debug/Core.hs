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

import Control.Applicative ((<$>))
import Control.Monad
import Data.Maybe
import FRP.Yampa        as Yampa
import FRP.Yampa.InternalCore (SF(..), SF'(..), sfTF', DTime)

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
  reactimateControl0 bridge prefs cmds (init, sense, actuate) sf

-- | Start a Yampa program with interactive debugging enabled.
reactimateControl0 :: (Read p, Show p, Show a, Read a, Show b, Read b, Pred p a b)
                   => ExternalBridge                 -- ^ Debug: Communication bridge for the interactive GUI
                   -> Preferences                    -- ^ Debug: Debugging preferences
                   -> [Command p]                    -- ^ Debug: List of commands to execute
                   -> SimOps a b                     -- ^ FRP:   Simulation (sensing, actuating) actions
                   -> SF a b                         -- ^ FRP:   Signal Function that defines the program
                   -> IO ()
reactimateControl0 bridge prefs commandQ simOps sf = do

  (command,commandQ') <- getCommand bridge commandQ

  -- Process one command and loop
  case command of

    Nothing                  -> reactimateControl0 bridge prefs commandQ' simOps sf
    Just Exit                -> return ()

    -- Jump one step back in the simulation
    Just SkipBack            -> reactimateControl0 bridge prefs commandQ' simOps sf

    -- Re-execute the last step
    Just Redo                -> reactimateControl0 bridge prefs commandQ' simOps sf

    -- TODO: Skip cycle while sensing the input
    Just SkipSense           -> do a0 <- simSense simOps
                                   when (dumpInput prefs) $ print a0

                                   let myInit = do (_,ma') <- simSense1 simOps False
                                                   return $ fromMaybe a0 ma'

                                   ebSendEvent bridge "CurrentFrameChanged"

                                   reactimateControl0 bridge prefs commandQ' (myInit, simSense1 simOps, simActuate simOps) sf

    -- TODO: Jump to a specific frame
    Just (JumpTo n)          -> do ebSendEvent bridge "CurrentFrameChanged"
                                   reactimateControl0 bridge prefs commandQ' simOps sf

    -- Simulate indefinitely
    Just Play                 -> do (a0, sf', _) <- step0
                                    let commandQ'' = if any stopPlayingCommand commandQ'
                                                       then commandQ'
                                                       else appendCommand commandQ' Play
                                        history    = mkHistory (a0, sf) sf' a0
                                    reactimateControl1 bridge prefs history (commandQ'') simOps

    Just Pause                -> reactimateControl0 bridge prefs commandQ' simOps sf

    -- Simulate one step forward
    Just Step                 -> do (a0, sf', _) <- step0
                                    let history    = mkHistory (a0, sf) sf' a0
                                    reactimateControl1 bridge prefs history commandQ' simOps

    -- Simulate until a predicate on the input and output holds
    Just (StepUntil p)        -> do (a0, sf', b0) <- step0
                                    cond          <- checkCond p Nothing a0 b0
                                    let commandQ'' = if cond then commandQ' else pushCommand commandQ' (StepUntil p)
                                    let history    = mkHistory (a0, sf) sf' a0

                                    -- Continue
                                    -- TODO Potential bug here: it could simulate too much!
                                    reactimateControl1 bridge prefs history commandQ'' simOps

    -- Skip steps until a predicate on the input and output holds
    Just (SkipUntil p)        -> do (a0, sf', b0) <- skip0
                                    cond          <- checkCond p Nothing a0 b0
                                    let commandQ'' = if cond then commandQ' else pushCommand commandQ' (SkipUntil p)
                                    let history    = mkHistory (a0, sf) sf' a0

                                    -- TODO Potential bug here: it could simulate too much!
                                    reactimateControl1 bridge prefs history commandQ'' simOps

    Just (GetInput _)         -> do ebSendMsg bridge ("Nothing")
                                    reactimateControl0 bridge prefs commandQ' simOps sf

    Just GetCurrentTime       -> do ebSendMsg bridge ("CurrentTime " ++ show 0)
                                    reactimateControl0 bridge prefs commandQ' simOps sf

    Just GetCurrentFrame      -> do ebSendMsg bridge ("CurrentFrame " ++ show 0)
                                    reactimateControl0 bridge prefs commandQ' simOps sf

    -- TODO: Print summary information about the history
    Just SummarizeHistory    -> do ebPrint bridge ("CurrentHistory 0")
                                   reactimateControl0 bridge prefs commandQ' simOps sf

    Just (SetPrefDumpInput b) -> do let prefs' = prefs { dumpInput = b }
                                    reactimateControl0 bridge prefs' commandQ' simOps sf

    Just GetPrefDumpInput     -> do print (dumpInput prefs)
                                    reactimateControl0 bridge prefs commandQ' simOps sf

    Just Ping                 -> do ebSendMsg bridge "Pong"
                                    ebSendEvent bridge   "PingSent"
                                    reactimateControl0 bridge prefs commandQ' simOps sf

    Just c                    -> do ebSendEvent bridge ("Got " ++ show c ++ ", dunno what to do with it")
                                    reactimateControl0 bridge prefs commandQ' simOps sf
  where
    -- step0 :: IO (a, SF' a b, b)
    step0 = do
      -- Step
      a0 <- simSense simOps
      when (dumpInput prefs) $ print a0

      let tf0  = sfTF sf
          (sf',b0) = tf0 a0
      _ <- simActuate simOps True b0
      ebSendEvent bridge "CurrentFrameChanged"
      return (a0, sf', b0)

    -- skip0 :: IO (a, SF' a b, b)
    skip0 = do
      a0 <- simSense simOps
      when (dumpInput prefs) $ print a0

      let tf0  = sfTF sf
          (sf',b0) = tf0 a0
      ebSendEvent bridge "CurrentFrameChanged"
      return (a0, sf', b0)

    -- checkCond :: (Show a, Show b, Pred p a b)
    --           => p -> Maybe DTime -> a -> b -> IO Bool
    checkCond p dt a0 b0 = do
      -- Check condition
      let cond = evalPred p dt a0 b0
      when cond $ do
        ebPrint bridge ("Condition became true, with " ++ show a0 ++ " (" ++ show b0 ++ ")")
        ebSendEvent bridge "ConditionMet"
      return cond

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

-- | Continue simulating a Yampa program with interactive debugging enabled.
reactimateControl1 :: (Read p, Show p, Show a, Read a, Show b, Read b, Pred p a b)
                   => ExternalBridge                            -- ^ Communication bridge for the interactive GUI
                   -> Preferences                               -- ^ Debugging preferences
                   -> History a b                               -- ^ Execution History: list of inputs and SF continuations, cur SF, inputs
                   -> [Command p]                               -- ^ List of pending commands execute
                   -> SimOps a b
                   -> IO ()
reactimateControl1 bridge prefs history commandQ simOps  = do
  (command,commandQ') <- getCommand bridge commandQ

  case command of

    Nothing   -> reactimateControl1 bridge prefs history commandQ' simOps

    Just Exit -> return ()

    -- TODO: Print summary information about the history
    Just SummarizeHistory     -> do let num = historyGetNumFrames history
                                    ebPrint bridge ("CurrentHistory " ++ show num)
                                    reactimateControl1 bridge prefs history  commandQ' simOps

    -- Jump to a specific frame
    Just (JumpTo n)           -> do ebSendEvent bridge   "CurrentFrameChanged"
                                    case historyJumpTo history n of
                                      (Just history', Nothing) -> do
                                        reactimateControl1 bridge prefs history' commandQ' simOps
                                      (Just history', Just (Left _)) -> do
                                        let commandQ'' = pushCommand commandQ' Redo
                                        reactimateControl1 bridge prefs history' commandQ'' simOps
                                      (Nothing, Just (Right sf0)) ->
                                        reactimateControl0 bridge prefs commandQ' simOps sf0

    -- Discard all future after a specific frame
    Just (DiscardFuture n)    -> do ebSendEvent bridge   "CurrentFrameChanged"
                                    case historyDiscardFuture history n of
                                      (Just history', Nothing) -> do
                                        reactimateControl1 bridge prefs history' commandQ' simOps
                                      (Just history', Just (Left _)) -> do
                                        let commandQ'' = pushCommand commandQ' Redo
                                        reactimateControl1 bridge prefs history' commandQ'' simOps
                                      (Nothing, Just (Right sf0)) ->
                                        reactimateControl0 bridge prefs commandQ' simOps sf0

    -- Jump one step back in the simulation
    Just SkipBack             -> do ebSendEvent bridge   "CurrentFrameChanged"
                                    case historyBack history of
                                      (Just history', Right _) -> do
                                        let commandQ'' = pushCommand commandQ' Redo
                                        reactimateControl1 bridge prefs history' commandQ'' simOps
                                      (Nothing, Left sf0) ->
                                        reactimateControl0 bridge prefs commandQ' simOps sf0

    -- Re-execute the last step
    Just Redo                 -> -- reactimateControl1 bridge prefs history commandQ' sense actuate sf lastInput
                                 do let (a0, mdt, sfc) = historyGetCurFrame history
                                        (sf', b0) = case (mdt, sfc) of
                                                      (_,       Left sf0)  -> sfTF  sf0 a0
                                                      (Just dt, Right sf1) -> sfTF' sf1 dt a0

                                    when (dumpInput prefs) $ print a0
                                    last <- simActuate simOps True b0
                                    unless last $
                                      reactimateControl1 bridge prefs history commandQ' simOps


    -- TODO: Skip cycle while sensing the input
    -- Should the input be used as new last input?
    Just SkipSense            -> do (_,a)  <- simSense1 simOps False
                                    when (dumpInput prefs) $ print a
                                    ebSendEvent bridge   "CurrentFrameChanged"

                                    reactimateControl1 bridge prefs history commandQ' simOps

    -- Simulate one step forward
    Just Step                 -> do (a', dt, sf', b', last) <- step1 history
                                    let history' = historyRecordFrame1 history (a', dt, sf')

                                    unless last $
                                      reactimateControl1 bridge prefs history' commandQ' simOps

    -- Simulate until a predicate on the input and output holds
    Just (StepUntil p)        -> do (a', dt, sf', b', last) <- step1 history
                                    let history' = historyRecordFrame1 history (a', dt, sf')

                                    cond <- checkCond p (Just dt) a' b'
                                    let commandQ'' = if cond then commandQ' else pushCommand commandQ' (StepUntil p)

                                    unless last $
                                      reactimateControl1 bridge prefs history' commandQ'' simOps

    -- Skip steps until a predicate on the input and output holds
    Just (SkipUntil p)        -> do (a', dt, sf', b') <- skip1 history
                                    let history' = historyRecordFrame1 history (a', dt, sf')

                                    cond <- checkCond p (Just dt) a' b'
                                    let commandQ'' = if cond then commandQ' else pushCommand commandQ' (SkipUntil p)

                                    -- TODO Potential bug here: it could simulate too much! If the condition is not
                                    -- met, it will not "actuate", and so it will not check whether it should have stopped.
                                    last <- if cond then simActuate simOps True b' else return False

                                    unless last $
                                      reactimateControl1 bridge prefs history' commandQ'' simOps

    -- Simulate indefinitely
    Just Play                 -> do (a', dt, sf', b', last) <- step1 history
                                    let history' = historyRecordFrame1 history (a', dt, sf')

                                    let commandQ'' = if any stopPlayingCommand commandQ' then commandQ' else commandQ' ++ [Play]

                                    unless last $
                                      reactimateControl1 bridge prefs history' commandQ'' simOps

    Just Pause                -> reactimateControl1 bridge prefs history commandQ' simOps

    Just (IOSense f)          -> do (dt, ma') <- simSense1 simOps False
                                    -- Unsafe fromJust use
                                    let a' = fromMaybe (fromJust $ getLastInput history) ma'
                                    when (dumpInput prefs) $ print a'
                                    let history'' = historyReplaceInputDTimeAt history f dt a'
                                    reactimateControl1 bridge prefs history'' commandQ' simOps

    Just (GetInput f)         -> do let e = historyGetInput history f
                                    ebSendMsg bridge (show e)
                                    reactimateControl1 bridge prefs history commandQ' simOps

    Just (SetInput f i)       -> do history'' <- case maybeRead i of
                                                   Nothing -> return history
                                                   Just a  -> return (historyReplaceInputAt history f a)
                                    reactimateControl1 bridge prefs history'' commandQ' simOps

    Just (GetGTime f)         -> do let e = historyGetGTime history f
                                    ebSendMsg bridge (show e)
                                    reactimateControl1 bridge prefs history commandQ' simOps

    Just (GetDTime f)         -> do let e = historyGetDTime history f
                                    ebSendMsg bridge (show e)
                                    reactimateControl1 bridge prefs history commandQ' simOps

    Just (SetDTime f dtS)     -> do history'' <- case maybeRead dtS of
                                                   Nothing -> return history
                                                   Just dt -> return (historyReplaceDTimeAt history f dt)
                                    reactimateControl1 bridge prefs history'' commandQ' simOps

    Just GetCurrentTime       -> do let num = historyGetCurrentTime history
                                    ebSendMsg bridge ("CurrentTime " ++ show num)
                                    reactimateControl1 bridge prefs history commandQ' simOps

    Just GetCurrentFrame      -> do let num = historyGetCurrentFrame history
                                    ebSendMsg bridge ("CurrentFrame " ++ show num)
                                    reactimateControl1 bridge prefs history commandQ' simOps

    Just (SetPrefDumpInput b) -> do let prefs' = prefs { dumpInput = b }
                                    reactimateControl1 bridge prefs' history commandQ' simOps

    Just GetPrefDumpInput     -> do print (dumpInput prefs)
                                    reactimateControl1 bridge prefs history commandQ' simOps

    Just Ping                 -> do ebSendMsg bridge "Pong"
                                    ebSendEvent bridge "PingSent"
                                    reactimateControl1 bridge prefs history commandQ' simOps

    other                     -> putStrLn $ show other
  where
    step1 history = do
      (dt, ma') <- simSense1 simOps False
      let a'       = fromMaybe (fromJust $ getLastInput history) ma' -- unsafe fromJust
          sf       = fromRight $ getCurSF history
          (sf',b') = (sfTF' sf) dt a'
      when (dumpInput prefs) $ print a'

      last <- simActuate simOps True b'
      ebSendEvent bridge   "CurrentFrameChanged"
      return (a', dt, sf', b', last)

    skip1 history = do
      (dt, ma') <- simSense1 simOps False
      let a'       = fromMaybe (fromJust $ getLastInput history) ma' -- unsafe fromJust
          sf       = fromRight $ getCurSF history
          (sf',b') = (sfTF' sf) dt a'

      when (dumpInput prefs) $ print a'
      ebSendEvent bridge   "CurrentFrameChanged"
      return (a', dt, sf', b')

    checkCond p dt a0 b0 = do
      -- Check condition
      let cond = evalPred p dt a0 b0
      when cond $ do
        ebPrint bridge ("Condition became true, with " ++ show (dt, a0) ++ " (" ++ show b0 ++ ")")
        ebSendEvent bridge "ConditionMet"
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
