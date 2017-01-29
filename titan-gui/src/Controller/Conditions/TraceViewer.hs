module Controller.Conditions.TraceViewer where

import Graphics.UI.Gtk hiding (Frame)
import Graphics.UI.Gtk.StreamChart
import Hails.MVC.Model.ProtectedModel.Reactive
import Data.ReactiveValue

import View.Objects
import CombinedEnvironment
import Model.Model

defaultRenderSettingsF :: Frame -> StreamChartStyle
defaultRenderSettingsF = \c ->
        StreamChartStyle
          { renderFGColor = (0.5, 0.6, 0.7, 1.0)
          , renderBGColor = case () of
                              _ | fSelected c       -> (0.8, 0.8, 0.8, 0.9)
                                | fCurrent c        -> (1.0, 0.6, 0.0, 0.9)
                                | fError c          -> (0.9, 0.2, 0.1, 0.9)
                                | (not $ fCached c) -> (0.9, 0.9, 0.9, 1.0)
                                | otherwise         -> (0.1, 0.9, 0.1, 0.9)
          , renderDot     = case () of
                              _ | fBreakpoint c   -> True
                                | otherwise       -> False
          , renderLetter  = case () of
                              _ | fError c        -> Just 'X'
                                | otherwise       -> Nothing
          }

installCondition cenv = do
  installTraceViewer cenv

installTraceViewer cenv = do
  sw <- scrollFrameSelection (uiBuilder (view cenv))

  streamChart <- streamChartNew :: IO (StreamChart Frame)
  widgetSetSizeRequest streamChart 10000 10000

  -- viewport `onResize` (do (w,h) <- widgetGetSize window
  --                       adjustmentSetPageSize adjustX w
  --                       adjustmentSetPageSize adjustY h)

  -- containerAdd viewport streamChart
  -- containerAdd sw viewport
  scrolledWindowAddWithViewport sw streamChart

  -- let myList = [ 'a', 'a', 'a', 'a', 'a', 'a', 'a', 'a', 'a', 'a', 'a', 'a', 'a', 'a'
  --              , 'a', 'a', 'a', 'a', 'a', 'a', 'a', 'a', 'a', 'a', 'a', 'a', 'a', 'a'
  --              , 'a', 'a', 'a', 'a', 'a', 'a', 'a', 'a', 'a', 'a', 'a', 'a', 'a', 'a'
  --              , 'a', 'a', 'a', 'a', 'c', 'a', 'a', 'a', 'b', 'a', 'd', 'a', 'a', 'a'
  --              ]

  -- let defaultRenderSettingsF = \c ->
  --       StreamChartStyle
  --         { renderFGColor = (0.5, 0.6, 0.7, 1.0)
  --         , renderBGColor = case c of
  --                             'a' -> (0.1, 0.9, 0.1, 0.9)
  --                             'b' -> (0.1, 0.2, 0.9, 0.9)
  --                             'c' -> (0.8, 0.8, 0.8, 0.9)
  --                             'd' -> (0.9, 0.2, 0.1, 0.9)
  --                             _   -> (0.9, 0.9, 0.9, 1.0)
  --         , renderDot     = if (c `notElem` ['b', 'd']) then True else False
  --         , renderLetter  = if (c /= 'd') then Nothing else Just 'X'
  --         }

  streamChartSetStyle streamChart defaultRenderSettingsF

  let m              = model cenv
  let curFrameField' = mkFieldAccessor selectedFrameField      (model cenv)

  -- TODO: Make streamchart reactive
  streamChartOnButtonEvent streamChart $ \press p -> do
    let framesField' = mkFieldAccessor framesField      (model cenv)
    fs <- reactiveValueRead (mkFieldAccessor framesField (model cenv))
    if (p >= length fs || p < 0)
      then do putStrLn "Out of range"
              reactiveValueWrite curFrameField' Nothing
      else if press
             then do putStrLn $ "Pressed: "  ++ show (fs!!p)
             else do putStrLn $ "Released: " ++ show (fs!!p)
                     reactiveValueWrite curFrameField' (Just p)

  -- Debug
  curFrameField' =:> wrapMW (\frame -> putStrLn $ "The selection changed " ++ show frame)

  let framesField' = mkFieldAccessor framesField (model cenv)
  framesField' =:> (conditionVMFrames streamChart cenv)
  framesField' =:> wrapMW (\frames -> putStrLn $ "The frames changed " ++ show frames)

conditionVMFrames streamChart cenv fs = onViewAsync $ do
  print fs
  streamChartSetList streamChart fs
