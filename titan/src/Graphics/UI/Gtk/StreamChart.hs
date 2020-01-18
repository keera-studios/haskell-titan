module Graphics.UI.Gtk.StreamChart where

import Control.Monad
import Data.IORef
import Data.Maybe
import Graphics.Rendering.Cairo as Cairo
import Graphics.UI.Gtk
import System.Glib.Types

data StreamChart a = StreamChart DrawingArea (IORef ([a], StreamChartParams a))
type StreamChartParams a = a -> StreamChartStyle
data StreamChartStyle =
  StreamChartStyle
    { renderFGColor :: (Double, Double, Double, Double)
    , renderBGColor :: (Double, Double, Double, Double)
    , renderDot     :: Bool
    , renderLetter  :: Maybe Char
    }

defaultStreamChartStyle = StreamChartStyle
  { renderFGColor = (0.5, 0.6, 0.7, 1.0)
  , renderBGColor = (0.9, 0.2, 0.1, 0.9)
  , renderDot     = True
  , renderLetter  = Nothing
  }

streamChartNew :: IO (StreamChart a)
streamChartNew = do
  da <- drawingAreaNew
  defaultParamsRef <- newIORef ([], \_ -> defaultStreamChartStyle)

  let sc = StreamChart da defaultParamsRef

  da `on` exposeEvent $ liftIO $ redrawStreamChart sc

  widgetAddEvents da [PointerMotionMask, Button1MotionMask, KeyPressMask, KeyReleaseMask]

  return sc

instance GObjectClass (StreamChart a) where
  toGObject (StreamChart drawingArea _) = toGObject drawingArea
  unsafeCastGObject o = StreamChart (unsafeCastGObject o) undefined
instance ObjectClass (StreamChart a)
instance WidgetClass (StreamChart a)
instance DrawingAreaClass (StreamChart a)

redrawStreamChart :: StreamChart a -> IO Bool
redrawStreamChart (StreamChart drawingArea scRef) = do
  (es, styleF) <- readIORef scRef
  dw           <- widgetGetDrawWindow drawingArea

  (w, h) <- widgetGetSize drawingArea
  regio  <- regionRectangle $ Rectangle 0 0 w h
  drawWindowBeginPaintRegion dw regio
  renderWithDrawable dw $ streamChartRenderBlocksCairo styleF es
  drawWindowEndPaint dw

  return True

streamChartSetList :: StreamChart a -> [a] -> IO ()
streamChartSetList sc@(StreamChart da scRef) ls' = do
  modifyIORef scRef (\(_,styleF) -> (ls',styleF))
  invalidateStreamChart sc

streamChartSetStyle :: StreamChart a -> StreamChartParams a -> IO ()
streamChartSetStyle sc@(StreamChart da scRef) styleF' = do
  modifyIORef scRef (\(ls,styleF) -> (ls,styleF'))
  invalidateStreamChart sc
  -- redrawStreamChart sc

invalidateStreamChart :: StreamChart a -> IO ()
invalidateStreamChart (StreamChart da _) = do
  (w, h) <- widgetGetSize da
  let rect = Rectangle 0 0 w h
  dw <- widgetGetWindow da
  case dw of
    Nothing -> return ()
    Just dw' -> drawWindowInvalidateRect dw' rect True

streamChartRenderBlocksCairo :: (a -> StreamChartStyle) -> [a] -> Render ()
streamChartRenderBlocksCairo rSettingsF bs = streamChartRenderBlocksCairo' rSettingsF bs 0

streamChartRenderBlocksCairo' :: (a -> StreamChartStyle) -> [a] -> Double -> Render ()
streamChartRenderBlocksCairo' rSettingsF []      baseX = return ()
streamChartRenderBlocksCairo' rSettingsF (bk:bs) baseX = do
  let rSettings = rSettingsF bk
      (rF,gF,bF,aF) = renderFGColor rSettings
      (rB,gB,bB,aB) = renderBGColor rSettings

  setSourceRGBA rB gB bB aB
  Cairo.rectangle baseX 0 20 80
  fill

  setSourceRGBA rF gF bF aF
  Cairo.rectangle baseX 0 20 80
  stroke

  when (renderDot rSettings) $ do
    setSourceRGBA 0 0 0 1.0
    Cairo.arc (baseX + 10) 40 3 0 (2 * pi)
    fill

  when (isJust $ renderLetter rSettings) $ do
    let x = fromJust $ renderLetter rSettings
    te <- textExtents [x]
    let textW = textExtentsWidth te
        textH = textExtentsHeight te
    setSourceRGBA 0.0 0.0 0.0 1.0
    moveTo (baseX + 10 - (textW / 2)) (40 - (textExtentsYbearing te / 2)) -- (40 - textH / 2)
    showText [x]

    liftIO (print $ textExtentsXbearing te)
    liftIO (print $ textExtentsYbearing te)

  streamChartRenderBlocksCairo' rSettingsF bs (baseX + 20)

streamChartOnButtonEvent :: StreamChart a -> (Bool -> Int -> IO ()) -> IO ()
streamChartOnButtonEvent (StreamChart da scRef) handler = do
    da `on` buttonPressEvent   $ myHandler (handler True)
    da `on` buttonReleaseEvent $ myHandler (handler False)
    return ()
  where
    myHandler :: (Int -> IO ()) -> EventM EButton Bool
    myHandler handler = do
      (x,y) <- eventCoordinates
      let x' = round x
          y' = round y
      liftIO $ postGUIAsync $ handler (x' `div` 20)
      return False
