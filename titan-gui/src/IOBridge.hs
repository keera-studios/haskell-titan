{-# LANGUAGE ScopedTypeVariables #-}
module IOBridge where

import Control.Applicative
import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.IfElse
import Data.Bits
import Data.List
import Data.IORef
import Data.Maybe
import Graphics.UI.Gtk
import Network.BSD
import Network.Socket
import System.IO
import Foreign.Ptr

-- | Bridge between the local debugger and the debugging GUI
type IOBridge = IORef IOBridge'

data IOBridge' = IOBridge'
  { yampaSocket :: Maybe YampaHandle }

mkDefaultIOBridge :: IO IOBridge
mkDefaultIOBridge =
  newIORef $ IOBridge' { yampaSocket = Nothing }

-- | Communication Handles
data YampaHandle = YampaHandle
  { commHandle  :: Handle
  , eventHandle :: Handle
  }

-- | Create communication handles to talk to Yampa simulation
--
-- TODO: Make this safe and may return type Maybe
openYampaHandle :: IO YampaHandle -- ^ Handles to communicate with FRP simulation
openYampaHandle = YampaHandle <$> openYampaCommHandle <*> openYampaEventHandle

-- | Open Sync communication channel with FRP simulation

openYampaCommHandle :: IO Handle -- ^ Handle to send commands and receive
                                 --    results from FRP debugger.
openYampaCommHandle = do
  -- Shamelessly taken from RWH

  let hostname = "localhost"
      port     = "8081"
  -- Look up the hostname and port. Either raises an exception or returns a
  -- nonempty list. First element in that list is supposed to be the best
  -- option.
  addrinfos <- getAddrInfo Nothing (Just hostname) (Just port)
  let serveraddr = head addrinfos

  -- Establish a socket for communication
  sock <- socket (addrFamily serveraddr) Stream defaultProtocol

  -- Mark the socket for keep-alive handling since it may be idle
  -- for long periods of time
  setSocketOption sock KeepAlive 1

  -- Connect to server
  connect sock (addrAddress serveraddr)

  -- Make a Handle out of it for convenience
  h <- socketToHandle sock ReadWriteMode

  -- We're going to set buffering to BlockBuffering and then
  -- explicitly call hFlush after each message, below, so that
  -- messages get logged immediately
  hSetBuffering h LineBuffering

  -- Save off the socket, program name, and server address in a handle
  return h

openYampaEventHandle :: IO Handle -- ^ Handle to use for logging
openYampaEventHandle = do
  let hostname = "localhost"
      port     = "8082"
  -- Look up the hostname and port.  Either raises an exception
  -- or returns a nonempty list.  First element in that list
  -- is supposed to be the best option.
  addrinfos <- getAddrInfo Nothing (Just hostname) (Just port)
  let serveraddr = head addrinfos

  -- Establish a socket for communication
  sock <- socket (addrFamily serveraddr) Stream defaultProtocol

  -- Mark the socket for keep-alive handling since it may be idle
  -- for long periods of time
  setSocketOption sock KeepAlive 1

  -- Connect to server
  connect sock (addrAddress serveraddr)

  -- Make a Handle out of it for convenience
  h <- socketToHandle sock ReadWriteMode

  -- We're going to set buffering to BlockBuffering and then
  -- explicitly call hFlush after each message, below, so that
  -- messages get logged immediately
  hSetBuffering h LineBuffering

  -- Save off the socket, program name, and server address in a handle
  return h

stopYampaSocket ioBridgeRef = do
  ioBridge <- readIORef ioBridgeRef
  let mSocket = yampaSocket ioBridge
  awhen mSocket $ \socket -> do
    hClose (commHandle socket)
    let ioBridge' = ioBridge { yampaSocket = Nothing }
    writeIORef ioBridgeRef ioBridge'

startYampaSocket ioBridgeRef = do
  ioBridge <- readIORef ioBridgeRef
  let mSocket = yampaSocket ioBridge
  when (isNothing mSocket) $ do
    handle <- openYampaHandle
    let mSocket'  = Just handle
        ioBridge' = ioBridge { yampaSocket = mSocket' }
    writeIORef ioBridgeRef ioBridge'

sendToYampaSocketSync ioBridgeRef msg =
  catch (sendToYampaSocketSync' ioBridgeRef msg)
        (\(e :: IOException) -> do hPutStrLn stderr ("Send failed when trying to send " ++ msg ++ " with " ++ show e)
                                   return Nothing)

sendToYampaSocketSync' ioBridgeRef msg = do
  ioBridge <- readIORef ioBridgeRef
  whenMaybe (yampaSocket ioBridge) $ \socket -> do
    hPutStrLn stderr ("Debug: Sending " ++ msg)
    hPutStrLn (commHandle socket) msg
    hFlush (commHandle socket)
    waitForInput (commHandle socket) 10000
    s <- hGetLine (commHandle socket)
    hPutStrLn stderr ("Debug: received " ++ s)
    return (Just s)

getFromYampaSocketSync ioBridgeRef =
  catch (getFromYampaSocketSync' ioBridgeRef)
        (\(e :: IOException) -> do hPutStrLn stderr ("Reading failed")
                                   return Nothing)

getFromYampaSocketSync' ioBridgeRef = do
  ioBridge <- readIORef ioBridgeRef
  let mSocket = yampaSocket ioBridge
  whenMaybe (yampaSocket ioBridge) $ \socket ->
    Just <$> hGetLine (commHandle socket)

getFromEventSocketSync ioBridgeRef =
  catch (getFromEventSocketSync' ioBridgeRef)
        (\(e :: IOException) -> do hPutStrLn stderr ("Reading failed " ++ show e)
                                   return Nothing)

getFromEventSocketSync' ioBridgeRef = do
  ioBridge <- readIORef ioBridgeRef
  whenMaybe (yampaSocket ioBridge) $ \socket -> do
    eof <- hIsEOF (eventHandle socket)
    if eof
      then do putStrLn "Got nothing in the event log"
              return Nothing
      else do s <- hGetLine  (eventHandle socket)
              putStrLn $ "Event log got: " ++ show s
              return (Just s)

sendToYampaSocketAsync ioBridgeRef msg =
  catch (sendToYampaSocketAsync' ioBridgeRef msg)
        (\(e :: IOException) -> do hPutStrLn stderr ("Send failed when trying to send " ++ msg)
                                   return ())

sendToYampaSocketAsync' ioBridgeRef msg = do
  ioBridge <- readIORef ioBridgeRef
  let mSocket = yampaSocket ioBridge
  awhen mSocket $ \socket -> do
    hPutStrLn stderr ("Debug: Sending " ++ msg)
    hFlush stderr
    hPutStrLn (commHandle socket) msg

waitForInput handle n = do
  eof <- hIsEOF handle
  when eof $ do
    threadDelay n
    waitForInput handle n

whenMaybe :: Maybe a -> (a -> IO (Maybe b)) -> IO (Maybe b)
whenMaybe Nothing  _ = return Nothing
whenMaybe (Just x) f = f x
