{-# LANGUAGE ScopedTypeVariables #-}
-- | Replacement of Yampa's reactimate function with recod-and-replay
-- capabilities.
module FRP.Titan.Record.Yampa
    ( reactimateRecord
    , RecordMode(..)
    )
  where

import Control.Exception
import Control.Monad
import Data.IORef
import Data.Maybe
import FRP.Yampa
import System.IO

-- | How to treat the given trace: read (replay) but not write to it,
--   write (record) but not replay it, and readwrite (replay until the end
--   and the continue recording to it).
data RecordMode = RecordReadOnly
                | RecordWriteOnly
                | RecordReadWrite
  deriving (Eq, Show)

recordMustWrite RecordReadOnly = False
recordMustWrite _              = True

recordMustRead RecordWriteOnly = False
recordMustRead _               = True

reactimateRecord :: (Read a, Show a)
                 => Maybe (FilePath, RecordMode)    -- ^ Debug: File onto which the result should be recorded and recording mode
                 -> IO a                            -- ^ FRP: Initial sensing action
                 -> (Bool -> IO (DTime, Maybe a))   -- ^ FRP: Continued sensing action
                 -> (Bool -> b -> IO Bool)          -- ^ FRP: Rendering/consumption action
                 -> SF a b                          -- ^ FRP: Signal Function that defines the program
                 -> IO ()
reactimateRecord Nothing sense0 sense actuate sf = reactimate sense0 sense actuate sf
reactimateRecord (Just (fp, mode)) sense0 sense actuate sf = do
  samples   <- (maybeRead =<<) <$> (catch (Just <$> readFile fp) (\(e :: IOException) -> return Nothing))
  hPutStrLn stderr (show samples)

  -- Read from here
  sample0Ref <- newIORef (fmap fst samples)
  samplesRef <- newIORef (let ss = fromMaybe [] $ fmap snd samples in length ss `seq` ss)

  -- Write into here
  newSample0Ref <- newIORef Nothing
  newSamplesRef <- newIORef []

  let newSense0 = do
        -- Sense from IO
        a  <- sense0

        -- Choose which one to use
        a' <- if recordMustRead mode
                then do sample0' <- readIORef sample0Ref
                        return (fromMaybe a sample0')
                else return a

        -- Update record, if necessary
        when (recordMustWrite mode) $ writeIORef newSample0Ref (Just a')
        return a'

  let newSense b = do
        as  <- sense b

        as' <- if recordMustRead mode
                then do sample <- readIORef samplesRef
                        case sample of
                          []     -> return as
                          (x:xs) -> do writeIORef samplesRef xs
                                       return x
                else return as

        when (recordMustWrite mode) $ do
          curSamples <- readIORef newSamplesRef
          writeIORef newSamplesRef (curSamples ++ [as'])

        return as'

  let newActuate x y = do
        last <- actuate x y
        when (recordMustWrite mode) $ do
          curSample0 <- readIORef newSample0Ref
          curSamples <- readIORef newSamplesRef
          case curSample0 of
            Nothing -> return ()
            Just s0 -> length curSamples `seq` writeFile fp (show (s0, curSamples))
        return last

  (maybe 0 (length.snd) samples) `seq`
    reactimate newSense0 newSense newActuate sf

maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads
