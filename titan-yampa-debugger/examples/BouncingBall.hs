{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE TypeSynonymInstances        #-}
{-# LANGUAGE MultiParamTypeClasses       #-}
{-# LANGUAGE Arrows                      #-}
{-# LANGUAGE ForeignFunctionInterface    #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Primitives as SDL
import FRP.Yampa       as Yampa
import Data.IORef
import FRP.Titan.Debug.Core
import FRP.Titan.Debug.CommTCP

width :: Num a => a
width  = 640
height :: Num a => a
height = 480

main = do
  timeRef <- newIORef (0 :: Int)
  bridge <- mkTitanCommTCPBridge
  reactimateControl
             bridge                         -- Communication channels
             defaultPreferences             -- Simulation preferences
             ([] :: [Command PosLowerThan]) -- Initial command queue
             initGraphs
             (\_ -> do
                dtSecs <- yampaSDLTimeSense timeRef
                return (dtSecs, Nothing))
             (\_ e -> display e >> return False)
             inCircles

display :: Double -> IO()
display angle = do
  -- Obtain surface
  screen <- getVideoSurface

  -- Paint screen green
  let format = surfaceGetPixelFormat screen
  green <- mapRGB format 0 0xFF 0
  fillRect screen Nothing green

  -- Paint small red square, at an angle 'angle' with respect to the center
  red <- mapRGB format 0xFF 0 0
  let radius = 30
      side   = 10
      x = (width  `div` 2) + round (cos angle * radius)
      y = (height `div` 2) + round (sin angle * radius)
  filledCircle screen x y side (Pixel 0xFF0000FF)

  -- Double buffering
  SDL.flip screen


inCircles :: SF () Double
inCircles = localTime -- >>> arr (\time -> time * pi / 4)

initGraphs :: IO ()
initGraphs = do
  -- Initialise SDL
  SDL.init [InitVideo]

  -- Create window
  screen <- setVideoMode width height 16 [SWSurface]
  setCaption "Test" ""
-- | Updates the time in an IO Ref and returns the time difference
updateTime :: IORef Int -> Int -> IO Int
updateTime timeRef newTime = do
  previousTime <- readIORef timeRef
  writeIORef timeRef newTime
  return (newTime - previousTime)

yampaSDLTimeSense :: IORef Int -> IO Yampa.DTime
yampaSDLTimeSense timeRef = do
  -- Get time passed since SDL init
  newTime <- fmap fromIntegral SDL.getTicks

  -- Obtain time difference
  dt <- updateTime timeRef newTime
  let dtSecs = fromIntegral dt / 100
  return dtSecs

newtype PosLowerThan = PosLowerThan { limit :: Double }
 deriving (Read, Show)

instance Pred PosLowerThan () GameOutput where
  evalPred p dt i ey = limit p < ey

-- * Interactive reactimation

type GameOutput = Double -- b (output type of the SF)

myPred dt a ((ey,_),_) = ey < 450

