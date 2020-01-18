{-# LANGUAGE Arrows #-}
module BouncingBallSF where

import FRP.Yampa as Yampa

width :: Num a => a
width  = 640
height :: Num a => a
height = 480

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


