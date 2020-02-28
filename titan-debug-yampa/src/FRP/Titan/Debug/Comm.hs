-- | Communication bridges between the debugger running locally attached
--   to a Yampa application and the debugging GUI running remotely on
--   your machine.
--
-- Copyright   : (C) Keera Studios Ltd, 2018
-- License     : GPL-3
-- Maintainer  : support@keera.co.uk
module FRP.Titan.Debug.Comm
    ( ExternalBridge(..)
    , getAllMessages
    )
  where

-- | Configuration layer to communicate the FRP app and the debugging GUI.
data ExternalBridge = ExternalBridge
  { ebPrint     :: String -> IO () -- ^ Print log messages
  , ebSendMsg   :: String -> IO () -- ^ Send a message to the debugger using the sync channel.
  , ebSendEvent :: String -> IO () -- ^ Send a message to the debugger using the async channel.
  , ebGetMsg    :: IO String       -- ^ Obtain a message from the debugger.
  }

-- | Obtain all pending messages from the debugger.
getAllMessages :: ExternalBridge -> IO [String]
getAllMessages bridge = getAllMessages' bridge []

-- | Obtain all pending messages from the debugger and add them to a queue.
getAllMessages' :: ExternalBridge -> [String] -> IO [String]
getAllMessages' bridge xs = do
  msg <- ebGetMsg bridge
  if null msg
    then return xs
    else getAllMessages' bridge (msg:xs)
