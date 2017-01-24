-- | Communicate Yampa game and debugging GUI via TCP
module FRP.Titan.Debug.CommTCP
    ( mkThemisCommTCPBridge
    )
  where

-- External modules
import Control.Concurrent
import Control.Concurrent
import Control.Concurrent.MVar
import Data.Bits
import Data.List
import Network.BSD
import Network.Socket
import System.IO

-- Internal modules
import FRP.Titan.Debug.Comm

-- | Create a communication bridge using a local TCP server.
mkThemisCommTCPBridge :: IO ExternalBridge
mkThemisCommTCPBridge = do
  outChannel   <- newMVar []
  eventChannel <- newMVar []
  getChannel   <- newMVar []
  forkIO $ mkSendMsg outChannel
  forkIO $ mkSendEvent eventChannel
  forkIO $ mkGetChannel getChannel
  let sendMsg msg = do
        msgs <- takeMVar outChannel
        putMVar outChannel (msgs ++ [msg])
      sendEvent msg = do
        msgs <- takeMVar eventChannel
        putMVar eventChannel (msgs ++ [msg])
      getMsg = do
        msgs <- takeMVar getChannel
        case msgs of
          []     -> putMVar getChannel [] >> return ""
          (x:xs) -> putMVar getChannel xs >> return x
  return $ ExternalBridge putStrLn sendMsg sendEvent getMsg

-- | Send communication channel that takes messages from an MVar and pushes
--   them out a socket.
mkSendMsg :: MVar [String] -> IO ()
mkSendMsg outChannel = do
  forkIO $ serveLog "8081" (\_ msg -> putInMVar outChannel msg)
  var <- takeMVar outChannel
  case var of
    [] -> putMVar outChannel []
    (x:xs) -> do putMVar outChannel xs

-- | Event communication channel that takes event messages from an MVar and
--   pushes them out a socket.
mkSendEvent :: MVar [String] -> IO ()
mkSendEvent channel = do
  forkIO $ serveLog "8082" (\_ msg -> putInMVar channel msg)
  var <- takeMVar channel
  case var of
    []     -> putMVar channel []
    (x:xs) -> do putMVar channel xs

mkGetChannel :: MVar [String] -> IO ()
mkGetChannel mvar = do
  undefined

type HandlerFunc = SockAddr -> String -> IO ()

serveLog :: String              -- ^ Port number or name; 514 is default
         -> HandlerFunc         -- ^ Function to handle incoming messages
         -> IO ()
serveLog port handlerfunc = withSocketsDo $
    do -- Look up the port.  Either raises an exception or returns
       -- a nonempty list.  
       addrinfos <- getAddrInfo 
                    (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                    Nothing (Just port)
       let serveraddr = head addrinfos

       -- Create a socket
       sock <- socket (addrFamily serveraddr) Stream defaultProtocol

       -- Bind it to the address we're listening to
       bindSocket sock (addrAddress serveraddr)

       -- Start listening for connection requests.  Maximum queue size
       -- of 5 connection requests waiting to be accepted.
       listen sock 5

       -- Create a lock to use for synchronizing access to the handler
       lock <- newMVar ()

       -- Loop forever waiting for connections.  Ctrl-C to abort.
       procRequests lock sock

    where
          -- | Process incoming connection requests
          procRequests :: MVar () -> Socket -> IO ()
          procRequests lock mastersock = 
              do (connsock, clientaddr) <- accept mastersock
                 handle lock clientaddr
                    "syslogtcpserver.hs: client connnected"
                 forkIO $ procMessages lock connsock clientaddr
                 procRequests lock mastersock

          -- | Process incoming messages
          procMessages :: MVar () -> Socket -> SockAddr -> IO ()
          procMessages lock connsock clientaddr =
              do connhdl <- socketToHandle connsock ReadMode
                 hSetBuffering connhdl LineBuffering
                 messages <- hGetContents connhdl
                 mapM_ (handle lock clientaddr) (lines messages)
                 hClose connhdl
                 handle lock clientaddr 
                    "syslogtcpserver.hs: client disconnected"

          -- Lock the handler before passing data to it.
          handle :: MVar () -> HandlerFunc
          -- This type is the same as
          -- handle :: MVar () -> SockAddr -> String -> IO ()
          handle lock clientaddr msg =
              withMVar lock 
                 (\a -> handlerfunc clientaddr msg >> return a)

-- A simple handler that prints incoming packets
plainHandler :: HandlerFunc
plainHandler addr msg = 
    putStrLn $ "From " ++ show addr ++ ": " ++ msg

-- * Aux

-- | Put a message in an MVar.
putInMVar :: MVar [String] -> String -> IO ()
putInMVar mvar s = do
  ss <- takeMVar mvar
  putMVar mvar (ss ++ [s])
