{-# LANGUAGE ScopedTypeVariables #-}
-- | Communicate Yampa game and debugging GUI via TCP
module FRP.Titan.Debug.CommTCP
    ( mkTitanCommTCPBridge )
  where

-- External modules
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan
import Control.Exception
import Control.Monad
import Data.Bits
import Data.List
import Data.Maybe
import Network.BSD
import Network.Socket
import System.IO

-- Internal modules
import FRP.Titan.Debug.Comm

-- | Create a communication bridge using a local TCP server.
mkTitanCommTCPBridge :: IO ExternalBridge
mkTitanCommTCPBridge = do

  -- The communication bridge is composed of two TCP sockets:
  -- a sync one and an async one.
  --
  -- To control messages sent and received through these sockets,
  -- three mvars are used:
  -- - One for outgoing, sync messages  (out)
  -- - One for outgoing, async messages (event)
  -- - One for incoming, sync message   (get)
  --
  -- Sending and receiving messages is controlled via three MVars.
  --
  outChannel   <- atomically $ newTChan
  eventChannel <- atomically $ newTChan
  getChannel   <- atomically $ newTChan
  forkIO $ mkSendMsg outChannel getChannel
  forkIO $ mkSendEvent eventChannel
  let sendMsg msg = do atomically $ writeTChan outChannel msg
                       putStrLn $ "Wrote the sync message " ++ msg
      sendEvent msg = do atomically $ writeTChan eventChannel msg
                         putStrLn $ "Wrote the async message " ++ msg
      getMsg = do a <- (fromMaybe "") <$> (atomically $ tryReadTChan getChannel)
      -- getMsg = do a <- atomically $ readTChan getChannel
                  when (not (null a)) $ putStrLn $ "Got the sync message " ++ show a
                  return a
  return $ ExternalBridge errPrintLn sendMsg sendEvent getMsg

-- | Send communication channel that takes messages from an MVar and pushes
--   them out a socket.
mkSendMsg :: TChan String -> TChan String -> IO ()
mkSendMsg outChannel getChannel = void $
  forkIO $ serveSync "8081" (\msg -> do atomically $ writeTChan getChannel msg) (atomically $ readTChanAll outChannel)

readTChanAll :: TChan a -> STM [a]
readTChanAll tchan = reverse <$> readTChanAll' tchan []

readTChanAll' :: TChan a -> [a] -> STM [a]
readTChanAll' tchan acc = do
  mval <- tryReadTChan tchan
  case mval of
    Nothing  -> return acc
    Just val -> readTChanAll' tchan (val : acc)

serveSync :: String             -- ^ Port number or name; 514 is default
          -> (String -> IO ())  -- ^ Function to handle incoming messages
          -> (IO [String])      -- ^ Function to obtain outgoing messages
          -> IO ()
serveSync port handlerGet handlerSend = withSocketsDo $
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
    procRequests lock mastersock = forever $ void $ do
      (connsock, _clientaddr) <- accept mastersock
      -- handle lock clientaddr
      --    "syslogtcpserver.hs: client connnected"
      connhdl <- socketToHandle connsock ReadWriteMode
      putStrLn ("Socket connected.")
      hSetBuffering connhdl LineBuffering
      hPutStrLn connhdl "Hello 0"
      hFlush connhdl
      t1 <- forkIO $ procSend lock connhdl
      t2 <- forkIO $ procGet  lock connhdl
      -- hClose connhdl
      return ()

    -- | Process incoming messages
    procGet :: MVar () -> Handle -> IO ()
    procGet lock connhdl = do
      let processMessage = forever $ do
            message   <- hGetLine connhdl
            handleGet lock message
      catch processMessage (\(e :: IOException) -> putStrLn "Disconnected")

    -- | Process incoming messages
    procSend :: MVar () -> Handle -> IO ()
    procSend lock connhdl = do
      let processMessage = forever $ do
            responses  <- handleSend lock
            mapM_ (\msg -> hPutStrLn connhdl msg >> hFlush connhdl) responses
      catch processMessage (\(e :: IOException) -> putStrLn "Disconnected")

      -- handle lock clientaddr
      --    "syslogtcpserver.hs: client disconnected"

    -- Lock the handler before passing data to it.
    handleGet :: MVar () -> String -> IO ()
    handleGet lock msg =
      withMVarLock lock $ handlerGet msg

    -- Lock the handler before passing data to it.
    handleSend :: MVar () -> IO [String]
    handleSend lock =
      withMVarLock lock $ handlerSend

-- | Event communication channel that takes event messages from an MVar and
--   pushes them out a socket.
mkSendEvent :: TChan String -> IO ()
mkSendEvent channel = void $
  forkIO $ serveAsync "8082" $ \handle -> forever $ do
    response <- atomically $ readTChan channel
    putStrLn $ "Sending to the event log: " ++ show response
    hPutStrLn handle response
    hFlush handle

serveAsync :: String                          -- ^ Port number or name; 514 is default
           -> (Handle -> IO ())   -- ^ Function to handle incoming messages
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
    procRequests lock mastersock = forever $ void $ do
      (connsock, _clientaddr) <- accept mastersock
      -- handle lock clientaddr
      --    "syslogtcpserver.hs: client connnected"
      forkIO $ procMessages lock connsock

    -- | Process incoming messages
    procMessages :: MVar () -> Socket -> IO ()
    procMessages lock connsock = do
      connhdl <- socketToHandle connsock ReadWriteMode
      putStrLn ("Socket connected.")
      hSetBuffering connhdl LineBuffering
      hPutStrLn connhdl "DHello 0"
      hFlush connhdl
      handle lock connhdl
      hClose connhdl

    -- Lock the handler before passing data to it.
    handle :: MVar () -> Handle -> IO ()
    -- This type is the same as
    -- handle :: MVar () -> SockAddr -> String -> IO ()
    handle lock handle =
      withMVarLock lock (handlerfunc handle)

-- * Aux

-- | Put a message in an MVar.
putInMVar :: MVar [String] -> String -> IO ()
putInMVar mvar s = modifyMVar_ mvar (return . (++ [s]))

withMVarLock :: MVar a -> IO b -> IO b
withMVarLock lock io = do
  a <- takeMVar lock
  r <- io
  putMVar lock a
  return r

errPrintLn :: String -> IO ()
errPrintLn s = hPutStrLn stderr s >> hFlush stderr
