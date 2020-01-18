-- | Simulation State
module FRP.Titan.Debug.SimMonad where

import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import FRP.Yampa                 (DTime, SF)

import FRP.Titan.Debug.Comm
import FRP.Titan.Debug.Command
import FRP.Titan.Debug.Preferences
import FRP.Titan.Debug.Predicates
import FRP.Titan.Debug.History

type SimMonad p a b = StateT (SimState p a b) IO

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

simModifyHistory :: (History a b -> History a b) -> SimMonad p a b ()
simModifyHistory f = do
  history <- f <$> getSimHistory
  modify $ \simState -> simState { simHistory = history }

hPushCommand :: Command p -> SimMonad p a b ()
hPushCommand cmd = modify
  (\simState -> simState { simCommands = pushCommand (simCommands simState) cmd })

hAppendCommand :: Command p -> SimMonad p a b ()
hAppendCommand cmd = modify
  (\simState -> simState { simCommands = appendCommand (simCommands simState) cmd })
