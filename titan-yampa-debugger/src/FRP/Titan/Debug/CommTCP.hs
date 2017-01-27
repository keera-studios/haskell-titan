{-# LANGUAGE ScopedTypeVariables #-}
-- | Communicate Yampa game and debugging GUI via TCP
module FRP.Titan.Debug.CommTCP
    ( mkThemisCommTCPBridge
    )
  where

-- External modules
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad
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
  forkIO $ mkSendMsg outChannel getChannel
  forkIO $ mkSendEvent eventChannel
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
mkSendMsg :: MVar [String] -> MVar [String] -> IO ()
mkSendMsg outChannel getChannel = void $
  forkIO $ serveSync "8081" $ \_ msg -> do
    putInMVar getChannel msg
    yield
    var <- takeMVar outChannel
    putMVar outChannel []
    return var

-- | Event communication channel that takes event messages from an MVar and
--   pushes them out a socket.
mkSendEvent :: MVar [String] -> IO ()
mkSendEvent channel = void $
  forkIO $ serveAsync "8082" $ \_ handle -> forever $ do
    -- putStrLn "Trying to send outgoing events"
    var <- takeMVar channel
    putMVar channel []
    mapM_ (putStrLn . ("Sending to the event log: " ++) . show) var
    mapM_ (hPutStrLn handle) var

type HandlerFunc = SockAddr -> String -> IO [String]
type HandlerFunc' = SockAddr -> Handle -> IO ()

serveAsync :: String              -- ^ Port number or name; 514 is default
           -> HandlerFunc'        -- ^ Function to handle incoming messages
           -> IO ()
serveAsync port handlerfunc = withSocketsDo $
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
          procRequests lock mastersock = do
            (connsock, clientaddr) <- accept mastersock
            -- handle lock clientaddr
            --    "syslogtcpserver.hs: client connnected"
            forkIO $ procMessages lock connsock clientaddr
            procRequests lock mastersock

          -- | Process incoming messages
          procMessages :: MVar () -> Socket -> SockAddr -> IO ()
          procMessages lock connsock clientaddr = do
            connhdl <- socketToHandle connsock ReadWriteMode
            hSetBuffering connhdl LineBuffering
            hPutStrLn connhdl "DHello 0"
            hFlush connhdl
            handle lock clientaddr connhdl
            hClose connhdl

          -- Lock the handler before passing data to it.
          handle :: MVar () -> HandlerFunc'
          -- This type is the same as
          -- handle :: MVar () -> SockAddr -> String -> IO ()
          handle lock clientaddr handle =
              withMVar lock 
                 (\a -> handlerfunc clientaddr handle >> return a)

serveSync :: String              -- ^ Port number or name; 514 is default
           -> HandlerFunc         -- ^ Function to handle incoming messages
           -> IO ()
serveSync port handlerfunc = withSocketsDo $
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
                 -- handle lock clientaddr
                 --    "syslogtcpserver.hs: client connnected"
                 forkIO $ procMessages lock connsock clientaddr
                 procRequests lock mastersock

          -- | Process incoming messages
          procMessages :: MVar () -> Socket -> SockAddr -> IO ()
          procMessages lock connsock clientaddr =
              do connhdl <- socketToHandle connsock ReadWriteMode
                 putStrLn ("Connected " ++ show clientaddr)
                 hSetBuffering connhdl LineBuffering
                 hPutStrLn connhdl "Hello 0"
                 hFlush connhdl
                 let processMessage = do
                       message   <- hGetLine connhdl
                       responses <- handle lock clientaddr message
                       mapM_ (\msg -> hPutStrLn connhdl msg >> hFlush connhdl) responses
                       processMessage
                 catch processMessage (\(e :: IOException) -> putStrLn "Disconnected")
                 
                 hClose connhdl
                 -- handle lock clientaddr 
                 --    "syslogtcpserver.hs: client disconnected"

          -- Lock the handler before passing data to it.
          handle :: MVar () -> HandlerFunc
          -- This type is the same as
          -- handle :: MVar () -> SockAddr -> String -> IO ()
          handle lock clientaddr msg = do
              a <- takeMVar lock
              responses <- handlerfunc clientaddr msg 
              putMVar lock a
              return responses

-- * Aux

-- | Put a message in an MVar.
putInMVar :: MVar [String] -> String -> IO ()
putInMVar mvar s = do
  ss <- takeMVar mvar
  putMVar mvar (ss ++ [s])
