{-# LANGUAGE FlexibleInstances                 #-}
{-# LANGUAGE TypeSynonymInstances              #-}
{-# LANGUAGE MultiParamTypeClasses             #-}
{-# LANGUAGE FunctionalDependencies            #-}
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

    Nothing                  -> reactimateControl bridge prefs commandQ' init sense actuate sf
    Just Exit                -> return ()

    -- Jump one step back in the simulation
    Just SkipBack            -> reactimateControl bridge prefs commandQ' init sense actuate sf

    -- Re-execute the last step
    Just Redo                -> reactimateControl bridge prefs commandQ' init sense actuate sf

    -- TODO: Skip cycle while sensing the input
    Just SkipSense           -> do a0 <- init
                                   when (dumpInput prefs) $ print a0

                                   let myInit = do (_,ma') <- sense False
                                                   return $ fromMaybe a0 ma'

                                   ebSendEvent bridge "CurrentFrameChanged"

                                   reactimateControl bridge prefs commandQ' myInit sense actuate sf

    -- TODO: Jump to a specific frame
    Just (JumpTo n)          -> do ebSendEvent bridge "CurrentFrameChanged"
                                   reactimateControl bridge prefs commandQ' init sense actuate sf

    -- Simulate indefinitely
    Just Play                 -> do (a0, sf', _) <- step0
                                    let commandQ'' = if any stopPlayingCommand commandQ'
                                                       then commandQ'
                                                       else appendCommand commandQ' Play
                                    reactimateControl' bridge prefs ((a0, sf), []) (commandQ'') init sense actuate sf' a0

    Just Pause               -> reactimateControl bridge prefs commandQ' init sense actuate sf

    -- Simulate one step forward
    Just Step                -> do (a0, sf', _) <- step0
                                   reactimateControl' bridge prefs ((a0, sf), []) commandQ' init sense actuate sf' a0

    -- Simulate until a predicate on the input and output holds
    Just (StepUntil p)       -> do (a0, sf', b0) <- step0
                                   cond          <- checkCond p Nothing a0 b0
                                   let commandQ'' = if cond then commandQ' else pushCommand commandQ' (StepUntil p)

                                   -- Continue
                                   -- TODO Potential bug here: it could simulate too much!
                                   reactimateControl' bridge prefs ((a0, sf), []) commandQ'' init sense actuate sf' a0

    -- Skip steps until a predicate on the input and output holds
    Just (SkipUntil p)       -> do (a0, sf', b0) <- skip0

                                   cond <- checkCond p Nothing a0 b0
                                   let commandQ'' = if cond then commandQ' else pushCommand commandQ' (SkipUntil p)

                                   -- TODO Potential bug here: it could simulate too much!
                                   reactimateControl' bridge prefs ((a0, sf), []) commandQ'' init sense actuate sf' a0

    Just (GetInput _)         -> do ebSendMsg bridge ("Nothing")
                                    reactimateControl bridge prefs commandQ' init sense actuate sf


    Just GetCurrentTime       -> do ebSendMsg bridge ("CurrentTime " ++ show 0)
                                    reactimateControl bridge prefs commandQ' init sense actuate sf

    Just GetCurrentFrame      -> do ebSendMsg bridge ("CurrentFrame " ++ show 0)
                                    reactimateControl bridge prefs commandQ' init sense actuate sf

    -- TODO: Print summary information about the history
    Just SummarizeHistory    -> do ebPrint bridge ("CurrentHistory 0")
                                   reactimateControl bridge prefs commandQ' init sense actuate sf

    Just (SetPrefDumpInput b) -> do let prefs' = prefs { dumpInput = b }
                                    reactimateControl bridge prefs' commandQ' init sense actuate sf

    Just GetPrefDumpInput     -> do print (dumpInput prefs)
                                    reactimateControl bridge prefs commandQ' init sense actuate sf

    Just Ping                 -> do ebSendMsg bridge "Pong"
                                    ebSendEvent bridge   "PingSent"
                                    reactimateControl bridge prefs commandQ' init sense actuate sf

    Just c                    -> do ebSendEvent bridge ("Got " ++ show c ++ ", dunno what to do with it")
                                    reactimateControl bridge prefs commandQ' init sense actuate sf
  where
    -- step0 :: IO (a, SF' a b, b)
    step0 = do
      -- Step
      a0 <- init
      when (dumpInput prefs) $ print a0

      let tf0  = sfTF sf
          (sf',b0) = tf0 a0
      _ <- actuate True b0
      ebSendEvent bridge "CurrentFrameChanged"
      return (a0, sf', b0)

    -- skip0 :: IO (a, SF' a b, b)
    skip0 = do
      a0 <- init
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

    -- TODO: Print summary information about the history
    Just SummarizeHistory     -> do let ((a0, sf0), ps) = previous
                                        num             = length ps
                                    ebPrint bridge ("CurrentHistory " ++ show num)
                                    reactimateControl' bridge prefs previous commandQ' init sense actuate sf lastInput

    -- TODO: Jump to a specific frame
    Just (JumpTo n)           -> do let ((a0, sf0), ps) = previous
                                    when (length ps + 1 > n) $ do
                                        ebSendEvent bridge   "CurrentFrameChanged"
                                        case n of
                                          0 -> reactimateControl bridge prefs commandQ' init sense actuate sf0
                                          _ -> let ps' = reverse $ take n $ reverse ps
                                                   ((_a,_dt, sf'):prevs@((lastInput, _, _):_)) = ps'
                                               in reactimateControl' bridge prefs ((a0, sf0), prevs) (Redo:commandQ') init sense actuate sf' lastInput

    -- Jump one step back in the simulation
    Just SkipBack             -> do ebSendEvent bridge   "CurrentFrameChanged"
                                    case previous of
                                      ((a0, sf0), _:(_a,_dt, sf'):prevs@((lastInput, _, _):_)) -> do
                                        let commandQ'' = pushCommand commandQ' Redo
                                        reactimateControl' bridge prefs ((a0, sf0), prevs) commandQ'' init sense actuate sf' lastInput

                                      ((a0, sf0), _:(_a,_dt, sf'):[]) -> do
                                        let commandQ'' = pushCommand commandQ' Redo
                                        reactimateControl' bridge prefs ((a0, sf0), []) commandQ'' init sense actuate sf' a0

                                      ((a0, sf0), _:[]) ->
                                        reactimateControl bridge prefs commandQ' init sense actuate sf0

                                      ((a0, sf0), []) ->
                                        reactimateControl bridge prefs commandQ' init sense actuate sf0

    -- Re-execute the last step
    Just Redo                 -> -- reactimateControl' bridge prefs previous commandQ' sense actuate sf lastInput
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

    -- TODO: Skip cycle while sensing the input
    -- Should the input be used as new last input?
    Just SkipSense            -> do (_,a)  <- sense False
                                    when (dumpInput prefs) $ print a
                                    ebSendEvent bridge   "CurrentFrameChanged"

                                    reactimateControl' bridge prefs previous commandQ' init sense actuate sf lastInput

    -- Simulate one step forward
    Just Step                 -> do (a', dt, sf', b', last) <- step1

                                    let ((a0, sf0), prevs) = previous

                                    unless last $
                                      reactimateControl' bridge prefs ((a0, sf0), (a', dt, sf'):prevs) commandQ' init sense actuate sf' a'

    -- Simulate until a predicate on the input and output holds
    Just (StepUntil p)        -> do (a', dt, sf', b', last) <- step1

                                    cond <- checkCond p (Just dt) a' b'

                                    let commandQ'' = if cond then commandQ' else pushCommand commandQ' (StepUntil p)
                                    let ((a0, sf0), prevs) = previous

                                    unless last $
                                      reactimateControl' bridge prefs ((a0, sf0), (a', dt, sf'):prevs) commandQ'' init sense actuate sf' a'

    -- Skip steps until a predicate on the input and output holds
    Just (SkipUntil p)        -> do (a', dt, sf', b') <- skip1

                                    cond <- checkCond p (Just dt) a' b'
                                    let commandQ'' = if cond then commandQ' else pushCommand commandQ' (SkipUntil p)

                                    -- TODO Potential bug here: it could simulate too much! If the condition is not
                                    -- met, it will not "actuate", and so it will not check whether it should have stopped.
                                    last <- if cond then actuate True b' else return False
                                    let ((a0, sf0), prevs) = previous

                                    unless last $
                                      reactimateControl' bridge prefs ((a0, sf0), (a', dt, sf'):prevs) commandQ'' init sense actuate sf' a'

    -- Simulate indefinitely
    Just Play                 -> do (a', dt, sf', b', last) <- step1

                                    let ((a0, sf0), prevs) = previous

                                    let commandQ'' = if any stopPlayingCommand commandQ' then commandQ' else commandQ' ++ [Play]

                                    unless last $
                                      reactimateControl' bridge prefs ((a0, sf0), (a', dt, sf'):prevs) commandQ'' init sense actuate sf' a'

    Just Pause                -> reactimateControl' bridge prefs previous commandQ' init sense actuate sf lastInput

    Just (GetInput f)         -> do let ((a0, sf0), ps) = previous
                                        as = a0 : map (\(a,_,_) -> a) ps
                                        e  = if length as >= f then Nothing else Just (as !! f)
                                    ebSendMsg bridge (show e)
                                    reactimateControl' bridge prefs previous commandQ' init sense actuate sf lastInput

    Just (SetInput f i)       -> do let ((a0, sf0), ps) = previous
                                        as              = a0 : map (\(a,_,_) -> a) ps
                                        e               = maybeRead i
                                    previous'' <- case e of
                                                    Nothing -> return previous
                                                    Just a  -> if length as >= f
                                                                 then return previous
                                                                 else let previous' = if f == 0
                                                                                        then ((a, sf0), ps)
                                                                                        else ((a0, sf0), appAt (f-1) (\(_,dt,sf) -> (a, dt, sf)) ps)
                                                                      in return previous'
                                    reactimateControl' bridge prefs previous'' commandQ' init sense actuate sf lastInput

    Just (GetGTime f)         -> do let ((a0, sf0), ps) = previous
                                        dts             = 0 : map (\(_,dt,_) -> dt) ps
                                        e               = if length dts >= f then Nothing else Just (sum (take f dts))
                                    ebSendMsg bridge (show e)
                                    reactimateControl' bridge prefs previous commandQ' init sense actuate sf lastInput

    Just (GetDTime f)         -> do let ((a0, sf0), ps) = previous
                                        dts             = 0 : map (\(_,dt,_) -> dt) ps
                                        e               = if length dts >= f then Nothing else Just (dts !! f)
                                    ebSendMsg bridge (show e)
                                    reactimateControl' bridge prefs previous commandQ' init sense actuate sf lastInput

    Just (SetDTime f i)       -> do let ((a0, sf0), ps) = previous
                                        dts             = 0 : map (\(_,dt,_) -> dt) ps
                                        e               = maybeRead i
                                    previous'' <- case e of
                                                    Nothing -> return previous
                                                    Just dt -> if length dts >= f
                                                                 then return previous
                                                                 else let previous' = if f == 0
                                                                                        then ((a0, sf0), ps)
                                                                                        else ((a0, sf0), appAt (f-1) (\(a,_,sf) -> (a, dt, sf)) ps)
                                                                      in return previous'
                                    reactimateControl' bridge prefs previous'' commandQ' init sense actuate sf lastInput

    Just GetCurrentTime       -> do let ((a0, sf0), ps) = previous
                                        num             = sum $ map (\(_,dt,_) -> dt) ps
                                    ebSendMsg bridge ("CurrentTime " ++ show num)
                                    reactimateControl' bridge prefs previous commandQ' init sense actuate sf lastInput

    Just GetCurrentFrame      -> do let ((a0, sf0), ps) = previous
                                        num             = length ps
                                    ebSendMsg bridge ("CurrentFrame " ++ show num)
                                    reactimateControl' bridge prefs previous commandQ' init sense actuate sf lastInput

    Just (SetPrefDumpInput b) -> do let prefs' = prefs { dumpInput = b }
                                    reactimateControl' bridge prefs' previous commandQ' init sense actuate sf lastInput

    Just GetPrefDumpInput     -> do print (dumpInput prefs)
                                    reactimateControl' bridge prefs previous commandQ' init sense actuate sf lastInput

    Just Ping                 -> do ebSendMsg bridge "Pong"
                                    ebSendEvent bridge   "PingSent"
                                    reactimateControl' bridge prefs previous commandQ' init sense actuate sf lastInput
    
    other                     -> putStrLn $ show other
  where
    step1 = do
      (dt, ma') <- sense False
      let a'       = fromMaybe lastInput ma'
          (sf',b') = (sfTF' sf) dt a'
      when (dumpInput prefs) $ print a'
                                                 
      last <- actuate True b'
      ebSendEvent bridge   "CurrentFrameChanged"
      return (a', dt, sf', b', last)

    skip1 = do
      (dt, ma') <- sense False
      let a'       = fromMaybe lastInput ma'
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
               | TravelToFrame    Int       -- ^ Control: Simulate up to a particular frame   (not implemented yet)
               | TeleportToFrame  Int       -- ^ Control: Skip to a particular frame          (not implemented)
               | Exit                       -- ^ Control: Stop the simulation and exit the program
               | Play                       -- ^ Control: Start executing normally
               | Pause                      -- ^ Control: Pause the simulation
               | Stop                       -- ^ Control: Stop the simulation
               | ReloadTrace      String    -- ^ Control: Reload the Trace from a file (not implemented yet)
               | IOSense                    -- ^ Control: Sense input                  (not implemented yet)
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
stopPlayingCommand (Step)               = True
stopPlayingCommand (StepUntil p)        = True
stopPlayingCommand (SkipUntil p)        = True
stopPlayingCommand (SkipSense)          = True
stopPlayingCommand (Redo)               = True
stopPlayingCommand (SkipBack)           = True
stopPlayingCommand (JumpTo _)           = True
stopPlayingCommand (TravelToFrame _)    = True
stopPlayingCommand (TeleportToFrame _)  = True 
stopPlayingCommand (Exit)               = True
stopPlayingCommand (Play)               = False
stopPlayingCommand (Pause)              = True
stopPlayingCommand (Stop)               = True
stopPlayingCommand (ReloadTrace _)      = True
stopPlayingCommand (IOSense)            = True
stopPlayingCommand (GetInput _ )        = False
stopPlayingCommand (SetInput _ _)       = False
stopPlayingCommand (GetGTime _ )        = False
stopPlayingCommand (GetDTime _ )        = False
stopPlayingCommand (SetDTime _ _)       = False
stopPlayingCommand (GetCurrentFrame)    = False
stopPlayingCommand (GetCurrentTime)     = False
stopPlayingCommand (SummarizeHistory)   = False
stopPlayingCommand (SetPrefDumpInput _) = False
stopPlayingCommand (GetPrefDumpInput)   = False
stopPlayingCommand (Ping)               = False


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

appAt :: Int -> (a -> a) -> [a] -> [a]
appAt _ f [] = []
appAt 0 f (x:xs) = f x : xs
appAt n f (x:xs) = x : appAt (n-1) f xs
