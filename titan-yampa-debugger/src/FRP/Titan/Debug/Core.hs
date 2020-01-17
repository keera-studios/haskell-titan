{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE ScopedTypeVariables #-}

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

import Control.Monad
import Control.Monad.IfElse
import Control.Monad.Trans.State
import Data.Maybe
import FRP.Yampa                 (SF, DTime, evalAtZero, evalAt)

import Data.Extra
import FRP.Titan.Debug.Comm
import FRP.Titan.Debug.Command
import FRP.Titan.Debug.Preferences
import FRP.Titan.Debug.Predicates
import FRP.Titan.Debug.History
import FRP.Titan.Debug.SimMonad

-- * Interactive reactimation

-- Yampa is based on SFs and FutureSFs. The former is an SF that can be turned on by
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
                                       let (b0, sf') = case (mdt, sfc) of
                                                         (_,       Just (Left  sf0)) -> evalAtZero  sf0 a0
                                                         (Just dt, Just (Right sf1)) -> evalAt sf1 dt a0

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

    -- step0 :: IO (a, FutureSF a b, b)
    step0 = do
      -- Step
      simState <- get
      history  <- getSimHistory
      a0 <- simSense
      when (dumpInput (simPrefs simState)) $ simPrint $ "CORE: Input: " ++ show a0

      let sf       = fromLeft (getCurSF' history)
          tf0      = evalAtZero sf
          (b0, sf') = tf0 a0
      _ <- simActuate  True b0
      -- TODO Potential bug here: it could simulate too much!
      simSendEvent "CurrentFrameChanged"
      simSendEvent "HistoryChanged"
      simModifyHistory (const (mkHistory (a0, sf) sf'))

      return (a0, b0)

    -- skip0 :: IO (a, FutureSF a b, b)
    skip0 = do
      simState <- get
      history  <- getSimHistory
      a0 <- simSense
      when (dumpInput (simPrefs simState)) $ simPrint $ "CORE: Input: " ++ show a0

      let sf   = fromLeft (getCurSF' history)
          tf0  = evalAtZero sf
          (b0, sf') = tf0 a0
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
          sf       = fromRight $ getCurSF' history
          (b', sf') = (evalAt sf) dt a'
      last         <- simActuate  True b'

      when last (modify simFinish)
      return (a', dt, sf', b')

    skip1 = do
      (dt, ma')    <- simSense1 False

      history      <- getSimHistory
      let a'       = fromMaybe (fromJust $ getLastInput history) ma' -- unsafe fromJust
          sf       = fromRight $ getCurSF' history
          (b',sf') = (evalAt sf) dt a'

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

