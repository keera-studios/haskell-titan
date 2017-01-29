{-# LANGUAGE MultiWayIf #-}
module Controller.Conditions.TraceViewer where

import Data.ReactiveValue
import Graphics.UI.Gtk hiding (Frame)
import Graphics.UI.Gtk.StreamChart
import Hails.MVC.Model.ProtectedModel.Reactive

import CombinedEnvironment
import Model.Model
import View.Objects

installCondition :: CEnv -> IO ()
installCondition cenv =
  installTraceViewer cenv

-- TODO: Move this to the View
installTraceViewer :: CEnv -> IO ()
installTraceViewer cenv = do

  -- Initialize stream chart
  streamChart <- streamChartNew :: IO (StreamChart Frame)
  widgetSetSizeRequest streamChart 10000 10000

  -- Add stream chart to view
  -- viewport `onResize` (do (w,h) <- widgetGetSize window
  --                       adjustmentSetPageSize adjustX w
  --                       adjustmentSetPageSize adjustY h)
  -- containerAdd viewport streamChart
  -- containerAdd sw viewport
  sw  <- scrollFrameSelection (uiBuilder (view cenv))
  scrolledWindowAddWithViewport sw streamChart

  -- Set rendering properties
  streamChartSetStyle streamChart defaultRenderSettingsF

  -- Attach reactive rules
  installTraceViewerSelection cenv streamChart
  installTraceViewerFrames    cenv streamChart

-- | Install Rule that keeps the user selection in the view in sync
-- with the selection in the model.
installTraceViewerSelection :: CEnv -> StreamChart Frame -> IO ()
installTraceViewerSelection cenv streamChart = do

  -- TODO: Make streamchart reactive
  streamChartOnButtonEvent streamChart $ \press p -> do
    let curFrameField' = mkFieldAccessor selectedFrameField (model cenv)
        framesField'   = mkFieldAccessor framesField        (model cenv)
    fs <- reactiveValueRead (mkFieldAccessor framesField (model cenv))
    if (p >= length fs || p < 0)
      then do putStrLn "Out of range"
              reactiveValueWrite curFrameField' Nothing
      else if press
             then do putStrLn $ "Pressed: "  ++ show (fs!!p)
             else do putStrLn $ "Released: " ++ show (fs!!p)
                     reactiveValueWrite curFrameField' (Just p)

  curFrameField' =:> wrapMW (\frame -> putStrLn $ "The selection changed " ++ show frame)

-- | Install Rule that keeps the frames model field in sync with the frames in
-- the view.
installTraceViewerFrames :: CEnv -> StreamChart Frame -> IO ()
installTraceViewerFrames cenv streamChart = do
  -- Debug
  let framesField' = mkFieldAccessor framesField (model cenv)
  framesField' =:> wrapMW (onViewAsync . streamChartSetList streamChart)
  framesField' =:> wrapMW (\frames -> putStrLn $ "The frames changed " ++ show frames)

-- | This funciton determines the style of the stream chart based on the kind
-- of frame.
defaultRenderSettingsF :: Frame -> StreamChartStyle
defaultRenderSettingsF = \c ->
  StreamChartStyle
    { renderFGColor = (0.5, 0.6, 0.7, 1.0)
    , renderBGColor = if | fSelected c       -> (0.8, 0.8, 0.8, 0.9)
                         | fCurrent c        -> (1.0, 0.6, 0.0, 0.9)
                         | fError c          -> (0.9, 0.2, 0.1, 0.9)
                         | (not $ fCached c) -> (0.9, 0.9, 0.9, 1.0)
                         | otherwise         -> (0.1, 0.9, 0.1, 0.9)
    , renderDot     = if | fBreakpoint c     -> True
                         | otherwise         -> False
    , renderLetter  = if | fError c          -> Just 'X'
                         | otherwise         -> Nothing
    }
