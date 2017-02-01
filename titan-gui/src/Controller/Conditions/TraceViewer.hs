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

  let curFrameField' = mkFieldAccessor selectedFrameField (model cenv)
  let framesField'   = mkFieldAccessor framesField        (model cenv)

  -- TODO: Make streamchart reactive
  streamChartOnButtonEvent streamChart $ \press p -> do
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
installTraceViewerFrames :: CEnv -> StreamChart Frame -> IO ()
installTraceViewerFrames cenv streamChart = do
  -- Debug
  let framesField'   = mkFieldAccessor framesField        (model cenv)
  let curFrameField' = mkFieldAccessor selectedFrameField (model cenv)
  let curSimFrame'   = mkFieldAccessor curSimFrameField   (model cenv)
  liftR3 markSelectedFrame curFrameField' curSimFrame' framesField'
    =:> wrapMW (onViewAsync . streamChartSetList streamChart)
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
