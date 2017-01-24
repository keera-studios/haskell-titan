{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE TypeSynonymInstances        #-}
{-# LANGUAGE MultiParamTypeClasses       #-}
{-# LANGUAGE FunctionalDependencies      #-}
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
reactimateControl :: (Read p, Show p, Show a, Read a, Show b, Read b, Pred p a b)
                  => ExternalBridge                 -- ^ Debug: Communication bridge for the interactive GUI
                  -> Preferences                    -- ^ Debug: Debugging preferences
                  -> [Command p]                    -- ^ Debug: List of initial commands execute
                  -> IO a                           -- ^ FRP:   Initial sensing action 
                  -> (Bool -> IO (DTime, Maybe a))  -- ^ FRP:   Continued sensing action
                  -> (Bool -> b -> IO Bool)         -- ^ FRP:   Rendering/consumption action
                  -> SF a b                         -- ^ FRP:   Signal Function that defines the program
                  -> IO ()
reactimateControl bridge prefs commandQ init sense actuate sf = do

  (command,commandQ') <- getCommand bridge commandQ

  -- Process one command and loop
  case command of

    Nothing   -> reactimateControl bridge prefs commandQ' init sense actuate sf

    Just Pause -> reactimateControl bridge prefs commandQ' init sense actuate sf
    Just Exit -> return ()

    -- Jump one step back in the simulation
    Just SkipBack -> reactimateControl bridge prefs commandQ' init sense actuate sf

    -- Re-execute the last step
    Just Redo -> reactimateControl bridge prefs commandQ' init sense actuate sf

    -- TODO: Print summary information about the history
    Just SummarizeHistory -> reactimateControl bridge prefs commandQ' init sense actuate sf

    -- TODO: Skip cycle while sensing the input
    Just SkipSense -> do
      a0 <- init
      when (dumpInput prefs) $ print a0

      let myInit = do (_,ma') <- sense False
                      return $ fromMaybe a0 ma'

      ebSendEvent bridge "CurrentFrameChanged"

      reactimateControl bridge prefs commandQ' myInit sense actuate sf

    -- TODO: Jump to a specific frame
    Just (JumpTo n) -> do
      ebSendEvent bridge "CurrentFrameChanged"
      reactimateControl bridge prefs commandQ' init sense actuate sf

    -- TODO: Step one simulation frame
    Just Step -> do
      a0 <- init
      when (dumpInput prefs) $ print a0

      let tf0      = sfTF sf
          (sf',b0) = tf0 a0
      _ <- actuate True b0
      ebSendEvent bridge "CurrentFrameChanged"

      reactimateControl' bridge prefs ((a0, sf), []) commandQ' init sense actuate sf' a0

    Just (StepUntil p) -> do
      a0 <- init
      when (dumpInput prefs) $ print a0

      let tf0  = sfTF sf
          (sf',b0) = tf0 a0
      _ <- actuate True b0
      ebSendEvent bridge "CurrentFrameChanged"

      let cond = evalPred p Nothing a0 b0
      when cond $ do
        (ebPrint bridge) ("Condition became true, with " ++ show a0 ++ " (" ++ show b0 ++ ")")
        ebSendEvent bridge "ConditionMet"

      let commandQ'' = if cond then commandQ' else pushCommand commandQ' (StepUntil p)

      -- TODO Potential bug here: it could simulate too much!
      reactimateControl' bridge prefs ((a0, sf), []) commandQ'' init sense actuate sf' a0

    Just (SkipUntil p) -> do
      a0 <- init
      when (dumpInput prefs) $ print a0

      let tf0  = sfTF sf
          (sf',b0) = tf0 a0
      ebSendEvent bridge "CurrentFrameChanged"

      let cond = evalPred p Nothing a0 b0
      when cond $ do
         _ <- actuate True b0
         (ebPrint bridge) ("Condition became true, with " ++ show a0 ++ " (" ++ show b0 ++ ")")
         ebSendEvent bridge "ConditionMet"

      let commandQ'' = if cond then commandQ' else pushCommand commandQ' (SkipUntil p)

      -- TODO Potential bug here: it could simulate too much!
      reactimateControl' bridge prefs ((a0, sf), []) commandQ'' init sense actuate sf' a0

    Just (SetPrefDumpInput b) -> do
      let prefs' = prefs { dumpInput = b }
      reactimateControl bridge prefs' commandQ' init sense actuate sf

    Just GetPrefDumpInput -> do
      print (dumpInput prefs)
      reactimateControl bridge prefs commandQ' init sense actuate sf

    Just Ping -> do
      ebSendMsg bridge "Pong"
      ebSendEvent bridge   "PingSent"
      reactimateControl bridge prefs commandQ' init sense actuate sf

-- | Continue simulating a Yampa program with interactive debugging enabled.
reactimateControl' :: (Read p, Show p, Show a, Read a, Show b, Read b, Pred p a b)
                   => ExternalBridge                            -- ^ Debug: Communication bridge for the interactive GUI
                   -> Preferences                               -- ^ Debug: Debugging preferences
                   -> ((a, SF a b), [(a, DTime, SF' a b)])      -- ^ Execution History: list of inputs and SF continuations
                   -> [Command p]                               -- ^ Debug: List of initial commands execute
                   -> IO a                                      -- ^ FRP:   Initial sensing action 
                   -> (Bool -> IO (DTime, Maybe a))             -- ^ FRP:   Continued sensing action
                   -> (Bool -> b -> IO Bool)                    -- ^ FRP:   Rendering/consumption action
                   -> SF' a b                                   -- ^ FRP:   Signal Function that defines the program
                   -> a                                         -- ^ Debug: Last known input
                   -> IO ()
reactimateControl' bridge prefs previous commandQ init sense actuate sf lastInput = do
  (command,commandQ') <- getCommand bridge commandQ

  case command of

    Nothing   -> reactimateControl' bridge prefs previous commandQ' init sense actuate sf lastInput

    Just Exit -> return ()

    Just SummarizeHistory -> do
      let ((a0, sf0), ps) = previous
          num             = length ps
      (ebPrint bridge) ("CurrentHistory " ++ show num)

    Just (JumpTo n) -> do
      let ((a0, sf0), ps) = previous
      when (length ps + 1 > n) $ do
          ebSendEvent bridge   "CurrentFrameChanged"
          case n of
            0 -> reactimateControl bridge prefs commandQ' init sense actuate sf0
            _ -> let ps' = reverse $ take n $ reverse ps
                     ((_a,_dt, sf'):prevs@((lastInput, _, _):_)) = ps'
                 in reactimateControl' bridge prefs ((a0, sf0), prevs) (Redo:commandQ') init sense actuate sf' lastInput

    Just SkipBack -> do
      ebSendEvent bridge   "CurrentFrameChanged"
      case previous of
        ((a0, sf0), _:(_a,_dt, sf'):prevs@((lastInput, _, _):_)) ->
          reactimateControl' bridge prefs ((a0, sf0), prevs) (Redo:commandQ') init sense actuate sf' lastInput

        ((a0, sf0), _:(_a,_dt, sf'):[]) ->
          reactimateControl' bridge prefs ((a0, sf0), []) (Redo:commandQ') init sense actuate sf' a0

        ((a0, sf0), _:[]) ->
          reactimateControl bridge prefs commandQ' init sense actuate sf0

        ((a0, sf0), []) ->
          reactimateControl bridge prefs commandQ' init sense actuate sf0

    Just Redo ->
      -- reactimateControl' bridge prefs previous commandQ' sense actuate sf lastInput
      case previous of
        ((a0, sf0), (an, dt, sfn):prevs) -> do
          let (sf',b') = (sfTF' sfn) dt an
          when (dumpInput prefs) $ print an

          last <- actuate True b'

          unless last $
            reactimateControl' bridge prefs previous commandQ' init sense actuate sf' an

        ((a0, sf0), []) -> do
           let tf0      = sfTF sf0
               (sf',b0) = tf0 a0
           _ <- actuate True b0

           reactimateControl' bridge prefs ((a0, sf0), []) commandQ' init sense actuate sf' a0

    -- Should the input be used as new last input?
    Just SkipSense -> do
      (_,a)  <- sense False
      when (dumpInput prefs) $ print a
      ebSendEvent bridge   "CurrentFrameChanged"

      reactimateControl' bridge prefs previous commandQ' init sense actuate sf lastInput

    Just Step -> do
      (dt, ma') <- sense False
      let a'       = fromMaybe lastInput ma'
          (sf',b') = (sfTF' sf) dt a'
      when (dumpInput prefs) $ print a'
      (ebPrint bridge) ("Sample was " ++ show (dt, a') ++ " returned (" ++ show b' ++ ")")

      last <- actuate True b'
      ebSendEvent bridge   "CurrentFrameChanged"

      let ((a0, sf0), prevs) = previous

      unless last $
        reactimateControl' bridge prefs ((a0, sf0), (a', dt, sf'):prevs) commandQ' init sense actuate sf' a'

    Just (StepUntil p) -> do
      (dt, ma') <- sense False
      let a'       = fromMaybe lastInput ma'
          (sf',b') = (sfTF' sf) dt a'
      when (dumpInput prefs) $ print a'

      last <- actuate True b'
      ebSendEvent bridge   "CurrentFrameChanged"

      let cond = evalPred p (Just dt) a' b'
      when cond $ do
        ebSendEvent bridge "ConditionMet"
        (ebPrint bridge) ("Condition became true, with " ++ show (dt, a') ++ " (" ++ show b' ++ ")")

      let commandQ'' = if cond then commandQ' else pushCommand commandQ' (StepUntil p)
      let ((a0, sf0), prevs) = previous

      unless last $
        reactimateControl' bridge prefs ((a0, sf0), (a', dt, sf'):prevs) commandQ'' init sense actuate sf' a'

    Just (SkipUntil p) -> do
      (dt, ma') <- sense False
      let a'       = fromMaybe lastInput ma'
          (sf',b') = (sfTF' sf) dt a'

      when (dumpInput prefs) $ print a'
      ebSendEvent bridge   "CurrentFrameChanged"

      let cond = evalPred p (Just dt) a' b'
      when cond $ do
        ebSendEvent bridge "ConditionMet"
        (ebPrint bridge) ("Condition became true, with " ++ show (dt, a') ++ " (" ++ show b' ++ ")")
      let commandQ'' = if cond then commandQ' else pushCommand commandQ' (SkipUntil p)

      -- TODO Potential bug here: it could simulate too much! If the condition is not
      -- met, it will not "actuate", and so it will not check whether it should have stopped.
      last <- if cond then actuate True b' else return False
      let ((a0, sf0), prevs) = previous

      unless last $
        reactimateControl' bridge prefs ((a0, sf0), (a', dt, sf'):prevs) commandQ'' init sense actuate sf' a'

    Just (SetPrefDumpInput b) -> do
      let prefs' = prefs { dumpInput = b }
      reactimateControl' bridge prefs' previous commandQ' init sense actuate sf lastInput

    Just GetPrefDumpInput -> do
      print (dumpInput prefs)
      reactimateControl' bridge prefs previous commandQ' init sense actuate sf lastInput

    Just Ping -> do
      ebSendMsg bridge "Pong"
      ebSendEvent bridge   "PingSent"
      reactimateControl' bridge prefs previous commandQ' init sense actuate sf lastInput

    Just Pause ->
      reactimateControl' bridge prefs previous commandQ' init sense actuate sf lastInput
-- * Commands

-- | An interactive, debugging command.
data Command p = Step                       -- ^ Execute a complete simulation cycle
               | StepUntil p                -- ^ Execute cycles until a predicate holds
               | SkipUntil p                -- ^ Skip cycles until a predicate holds
               | SkipSense                  -- ^ Skip cycle while sensing the input
               | Redo                       -- ^ Re-execute the last step
               | SkipBack                   -- ^ Jump one step back in the simulation
               | JumpTo Int                 -- ^ Jump to a specific frame
               | Exit                       -- ^ Stop the simulation and exit the program
               | Play                       -- ^ Start executing normally
               | Pause                      -- ^ Pause the simulation
               | Stop                       -- ^ Stop the simulation
               | ReloadTrace      String    -- ^ Reload the Trace from a file (not implemented yet)
               | IOSense                    -- ^ Sense input                  (not implemented yet)
               | GetCurrentFrame            -- ^ Obtain the current frame     (not implemented yet)
               | TravelToFrame    Int       -- ^ Simulate up to a particular frame   (not implemented yet)
               | TeleportToFrame  Int       -- ^ Skip to a particular frame (not implemented)
               | SummarizeHistory           -- ^ Print summary information about the history
               | SetPrefDumpInput Bool      -- ^ Alter simulation preferences
               | GetPrefDumpInput           -- ^ Obtain simulation preferences
               | Ping                       -- ^ Debugging: send a pong back to the GUI
 deriving (Eq, Read, Show)

-- ** Command Queue

-- | Obtain a command from the command queue, polling the communication
--   bridge if the queue is empty.
getCommand :: (Read a, Show a) => ExternalBridge -> [a] -> IO (Maybe a, [a])
getCommand bridge (c:cs) = return (Just c, cs)
getCommand bridge [] = do
  mLines <- filter (not . null) <$> getAllMessages bridge
  let cmLines = map maybeRead mLines
      cLines  = catMaybes cmLines
  unless (null mLines) $ do
    (ebPrint bridge) (show mLines)
    (ebPrint bridge) (show cmLines)
  case cLines of
    [] -> return (Nothing, [])
    (c:cs) -> return (Just c, cs)

-- | Place one command on the top of the queue.
pushCommand :: [a] -> a -> [a]
pushCommand cs c = c:cs

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

maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads
