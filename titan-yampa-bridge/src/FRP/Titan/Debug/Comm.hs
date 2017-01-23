module FRP.Titan.Debug.Comm where

data ExternalBridge = ExternalBridge
  { ebPrint     :: String -> IO ()
  , ebSendMsg   :: String -> IO ()
  , ebSendEvent :: String -> IO ()
  , ebGetMsg    :: IO String
  }

getAllMessages :: ExternalBridge -> IO [String]
getAllMessages bridge = getAllMessages' bridge []

getAllMessages' bridge xs = do
  msg <- ebGetMsg bridge
  if null msg
    then return xs
    else getAllMessages' bridge (msg:xs)
