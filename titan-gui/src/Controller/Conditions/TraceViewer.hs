module Controller.Conditions.TraceViewer where

import Graphics.UI.Gtk hiding (Frame)
import Graphics.UI.Gtk.StreamChart

import View.Objects
import CombinedEnvironment

data Frame = Frame
  { fSelected   :: Bool
  , fCached     :: Bool
  , fCurrent    :: Bool
  , fBreakpoint :: Bool
  , fError      :: Bool
  , fNumber     :: Int
  }
 deriving Show

defaultSelectedFrame   = Frame True  True False False False 
defaultFrame           = Frame False True False False False
defaultCurrentFrame    = Frame False False True False False
defaultBreakpointFrame = Frame False True False True  False
defaultErrorFrame      = Frame False True False False True

frames = zipWith (\f x -> f x)
         [ defaultFrame, defaultFrame, defaultFrame, defaultFrame, defaultFrame, defaultFrame
         , defaultFrame, defaultFrame, defaultFrame, defaultFrame, defaultFrame, defaultFrame
         , defaultFrame, defaultFrame, defaultFrame, defaultFrame, defaultFrame, defaultFrame
         , defaultFrame, defaultFrame, defaultFrame, defaultFrame, defaultFrame, defaultFrame
         , defaultSelectedFrame, defaultFrame, defaultFrame, defaultFrame, defaultFrame, defaultFrame
         , defaultErrorFrame, defaultFrame, defaultCurrentFrame, defaultFrame, defaultBreakpointFrame 
         , defaultFrame, defaultFrame, defaultFrame, defaultFrame, defaultFrame, defaultFrame
         ]
         [0..]

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

  streamChartSetList streamChart frames

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

  -- streamChartOnButtonEvent streamChart (\press p ->
  --   if (p >= length frames || p < 0)
  --     then putStrLn "Out of range"
  --     else if press
  --            then putStrLn $ "Pressed: "  ++ show (frames!!p)
  --            else putStrLn $ "Released: " ++ show (frames!!p))
