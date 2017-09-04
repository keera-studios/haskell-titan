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
             bouncingBall

display :: Double -> IO()
display y = do
  -- Obtain surface
  screen <- getVideoSurface

  -- Paint screen green
  let format = surfaceGetPixelFormat screen
  green <- mapRGB format 0xAB 0xA9 0xBF
  fillRect screen Nothing green

  -- Paint small red square, at an angle 'angle' with respect to the center
  red <- mapRGB format 0x34 0x11 0x3F
  let radius = 20
  let x = (width  - radius) `div` 2
  filledCircle screen x (height - round y) radius (Pixel 0xFF3411FF)

  putStrLn (show (x, y, radius))
  -- Double buffering
  SDL.flip screen

bouncingBall :: SF () Double
bouncingBall = bouncingBall' (height / 2) 0 >>> arr fst

bouncingBall' :: Double -> Double -> SF () (Double, Double)
bouncingBall' p0 v0 = switch (fallingBall p0 v0 >>> (arr id &&& hitFloor)) (\(p,v) -> bouncingBall' p (-v))

fallingBall p0 v0 = proc () -> do
                      v <- (v0+) ^<< integral -< -9.8
                      p <- (p0+) ^<< integral -< v
                      returnA -< (p, v)

hitFloor =
  arr (\(p,v) -> if p < 0 && v < 0 then Event (p,v) else Yampa.NoEvent)

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
  let dtSecs = fromIntegral dt / 1000
  return dtSecs

newtype PosLowerThan = PosLowerThan { limit :: Double }
 deriving (Read, Show)

instance Pred PosLowerThan () GameOutput where
  evalPred p dt i ey = limit p < ey

-- * Interactive reactimation

type GameOutput = Double -- b (output type of the SF)

myPred dt a ((ey,_),_) = ey < 450

