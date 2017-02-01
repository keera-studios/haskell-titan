-- | The TraceViewer contains the rules that create and keep the
--   GUI stream widget in sync with the trace in the model.
module Controller.Conditions.TraceViewer where

import Data.ReactiveValue
import Graphics.UI.Gtk hiding (Frame)
import Graphics.UI.Gtk.StreamChart
import Hails.MVC.Model.ProtectedModel.Reactive

import CombinedEnvironment
import Model.Model
import View.Objects

installCondition :: CEnv -> IO ()
installCondition cenv = do
  installTraceViewerSelection cenv
  installTraceViewerFrames    cenv

-- | Install Rule that keeps the user selection in the view in sync
-- with the selection in the model.
installTraceViewerSelection :: CEnv -> IO ()
installTraceViewerSelection cenv = do
  let traceViewer = streamChart (view cenv)

  let curFrameField' = mkFieldAccessor selectedFrameField (model cenv)
  let framesField'   = mkFieldAccessor framesField        (model cenv)

  -- TODO: Make streamchart reactive
  streamChartOnButtonEvent traceViewer $ \press p -> do
    fs <- reactiveValueRead (mkFieldAccessor framesField (model cenv))
    if (p >= length fs || p < 0)
      then do putStrLn "Out of range"
              reactiveValueWrite curFrameField' Nothing
      else if press
             then putStrLn $ "Pressed: "  ++ show (fs!!p)
             else do putStrLn $ "Released: " ++ show (fs!!p)
                     reactiveValueWrite curFrameField' (Just p)

  curFrameField' =:>
    wrapMW (\frame -> putStrLn $ "The selection changed " ++ show frame)

-- | Install Rule that keeps the frames model field in sync with the frames in
-- the view.
installTraceViewerFrames :: CEnv -> IO ()
installTraceViewerFrames cenv = do
  let traceViewer = streamChart (view cenv)
  -- Debug
  let framesField'   = mkFieldAccessor framesField        (model cenv)
  let curFrameField' = mkFieldAccessor selectedFrameField (model cenv)
  let curSimFrame'   = mkFieldAccessor curSimFrameField   (model cenv)
  liftR3 markSelectedFrame curFrameField' curSimFrame' framesField'
    =:> wrapMW (onViewAsync . streamChartSetList traceViewer)
  framesField' =:> wrapMW (\fs -> putStrLn $ "Frames changed:" ++ show fs)

markSelectedFrame :: Maybe Int -> Maybe Int -> [Frame] -> [Frame]
markSelectedFrame c s [] = []
markSelectedFrame c s (f:fs) = mark f : markSelectedFrame c' s' fs
  where
    mark = markSelected . markCurrent
    markSelected = if s == Just 0 then frameSelect  else frameDeselect
    markCurrent  = if c == Just 0 then frameCurrent else frameNotCurrent
    c'           = toJust $ maybe (-1) (\x -> x - 1) c
    s'           = toJust $ maybe (-1) (\x -> x - 1) s
    toJust n = if n < 0 then Nothing else Just n
