import Graphics.UI.Gtk
import Graphics.UI.Gtk.StreamChart

main :: IO ()
main = do
  _      <- initGUI
  window <- windowNew

  -- (w,h) <- widgetGetSize window
  --
  -- adjustX  <- adjustmentNew 1 0 10000000 1 (fromIntegral w) (fromIntegral w)
  -- adjustY  <- adjustmentNew 1 0 10000    1 (fromIntegral h) (fromIntegral h)
  -- viewport <- viewportNew   adjustX adjustY
  -- sw       <- scrolledWindowNew (Just adjustX) (Just adjustY)
  -- scrolledWindowSetPolicy sw PolicyAlways PolicyAlways

  sw       <- scrolledWindowNew Nothing Nothing
  streamChart <- streamChartNew 
  widgetSetSizeRequest streamChart 10000 10000

  -- viewport `onResize` (do (w,h) <- widgetGetSize window
  --                       adjustmentSetPageSize adjustX w
  --                       adjustmentSetPageSize adjustY h)

  -- containerAdd viewport streamChart
  -- containerAdd sw viewport
  scrolledWindowAddWithViewport sw streamChart
  containerAdd window sw

  let myList = [ 'a', 'b', 'c', 'b', 'c', 'b', 'c', 'b', 'c', 'b', 'c', 'b', 'c', 'b'
               , 'c', 'b', 'c', 'b', 'c', 'b', 'c', 'b', 'c', 'b', 'c', 'b', 'c', 'b'
               , 'c', 'b', 'c', 'b', 'c', 'b', 'c', 'b', 'c', 'b', 'c', 'b', 'c', 'b'
               , 'c', 'b', 'c', 'b', 'c', 'b', 'c', 'b', 'c', 'b', 'c', 'b', 'c', 'b'
               , 'c', 'b', 'c', 'b', 'c', 'b', 'c', 'b', 'c', 'b', 'c', 'b', 'c', 'b'
               , 'c', 'b', 'c', 'b', 'c', 'b', 'c', 'b', 'c', 'b', 'c', 'b', 'c', 'b'
               , 'c', 'b', 'c', 'b', 'c', 'b', 'c', 'b', 'c', 'b', 'c', 'b', 'c', 'b'
               , 'c', 'b', 'c', 'b', 'c', 'b', 'c', 'b', 'c', 'b', 'c', 'b', 'c', 'b'
               , 'd', 'd', 'd', 'd', 'd', 'd', 'd', 'd', 'd', 'd', 'd', 'd', 'd', 'd'
               ]

  streamChartSetList streamChart myList

  let defaultRenderSettingsF = \c ->
        StreamChartStyle
          { renderFGColor = (0.5, 0.6, 0.7, 1.0)
          , renderBGColor = if (c == 'a') then (0.9, 0.2, 0.1, 0.9) else (0.1, 0.9, 0.1, 0.9)
          , renderDot     = if (c /= 'b') then True else False
          , renderLetter  = if (c /= 'b') then Nothing else Just 'X'
          }

  streamChartSetStyle streamChart defaultRenderSettingsF

  streamChartOnButtonEvent streamChart (\press p -> 
    if (p >= length myList || p < 0)
      then putStrLn "Out of range"
      else if press
             then putStrLn $ "Pressed: "  ++ [ (myList!!p) ]
             else putStrLn $ "Released: " ++ [ (myList!!p) ])
   
  window `onDestroy` mainQuit

  windowSetDefaultSize window 640 480
  widgetShowAll window
  mainGUI
