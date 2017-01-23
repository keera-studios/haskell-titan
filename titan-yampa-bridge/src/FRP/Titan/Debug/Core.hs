{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE TypeSynonymInstances        #-}
{-# LANGUAGE MultiParamTypeClasses       #-}
{-# LANGUAGE FunctionalDependencies      #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind -Wall #-}

module FRP.Titan.Debug.Core where

import Control.Monad
import Data.Maybe
import FRP.Yampa        as Yampa
import FRP.Yampa.InternalCore (SF(..), SF'(..), sfTF', DTime)

import FRP.Titan.Debug.Comm

class Read p => Pred p i o | p -> i, p -> o where
  evalPred :: p -> Maybe DTime -> i -> o -> Bool

-- TODO: Possibly use this:
-- https://hackage.haskell.org/package/hint

-- * Interactive reactimation

reactimateControl :: (Read p, Show p, Show a, Read a, Show b, Read b, Pred p a b)
                  => ExternalBridge 
                  -> Preferences
                  -> [Command p]
                  -> IO a
                  -> (Bool -> IO (DTime, Maybe a))
                  -> (Bool -> b -> IO Bool)
                  -> SF a b
                  -> IO ()
reactimateControl bridge prefs commandQ init sense actuate sf = do

  (command,commandQ') <- getCommand bridge commandQ

  -- Process one command and loop
  case command of

    Nothing   -> reactimateControl bridge prefs commandQ' init sense actuate sf

    Just Exit -> return ()

    Just SkipBack -> reactimateControl bridge prefs commandQ' init sense actuate sf

    Just Redo -> reactimateControl bridge prefs commandQ' init sense actuate sf

    Just SummarizeHistory -> reactimateControl bridge prefs commandQ' init sense actuate sf

    Just SkipSense -> do
      a0 <- init
      when (dumpInput prefs) $ print a0

      let myInit = do (_,ma') <- sense False
                      return $ fromMaybe a0 ma'

      ebSendEvent bridge "CurrentFrameChanged"

      reactimateControl bridge prefs commandQ' myInit sense actuate sf

    Just (JumpTo n) -> do
      ebSendEvent bridge "CurrentFrameChanged"
      reactimateControl bridge prefs commandQ' init sense actuate sf

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

reactimateControl' :: (Read p, Show p, Show a, Read a, Show b, Read b, Pred p a b)
                   => ExternalBridge 
                   -> Preferences
                   -> ((a, SF a b), [(a, DTime, SF' a b)])
                   -> [Command p]
                   -> IO a
                   -> (Bool -> IO (DTime, Maybe a))
                   -> (Bool -> b -> IO Bool)
                   -> SF' a b
                   -> a
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

-- * Command Queue

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
pushCommand cs c = c:cs

-- * Commands

data Command p = Step
               | StepUntil p
               | SkipUntil p
               | SkipSense
               | Redo
               | SkipBack
               | JumpTo Int
               | Exit
               | Play
               | Pause
               | Stop
               | ReloadTrace      String
               | IOSense
               | GetCurrentFrame
               | TravelToFrame    Int
               | TeleportToFrame  Int
               | SummarizeHistory
               | SetPrefDumpInput Bool
               | GetPrefDumpInput
               | Ping
 deriving (Eq, Read, Show)

-- * Simulation preferences
data Preferences = Preferences
  { dumpInput :: Bool }

defaultPreferences :: Preferences
defaultPreferences = Preferences
  { dumpInput = False }

-- ** Utility functions

maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads
