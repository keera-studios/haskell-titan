{-# LANGUAGE MultiWayIf #-}
-- | Contains basic operations related to the GUI
--
-- Copyright   : (C) Keera Studios Ltd, 2018
-- License     : GPL-3
-- Maintainer  : support@keera.co.uk
module View (module Exported) where

-- External libraries
import Graphics.UI.Gtk
import Graphics.UI.Gtk.GtkView (GtkGUI(..))
import Graphics.UI.Gtk.StreamChart

-- Internal libraries
import Hails.MVC.View.GtkView     as Exported
import Hails.MVC.View.DefaultView as Exported
import View.Objects

import Model.Model                as Model

-- | Add all initialisers to the initialise operation and store
-- everything we'll need in the view. We need this operation here
-- because the URL to the glade file depends on the application
-- name.
instance GtkGUI View where
  initialise = do
    ui <- loadInterface

    -- Initialize stream chart
    streamChart <- streamChartNew :: IO (StreamChart Model.Frame)
    widgetSetSizeRequest streamChart 10000 10000

    -- Add stream chart to view
    -- viewport `onResize` (do (w,h) <- widgetGetSize window
    --                       adjustmentSetPageSize adjustX w
    --                       adjustmentSetPageSize adjustY h)
    -- containerAdd viewport streamChart
    -- containerAdd sw viewport
    sw  <- scrollFrameSelection ui
    scrolledWindowAddWithViewport sw streamChart

    -- Set rendering properties
    streamChartSetStyle streamChart defaultRenderSettingsF

    return $ View ui streamChart

-- | This funciton determines the style of the stream chart based on the kind
-- of frame.
defaultRenderSettingsF :: Model.Frame -> StreamChartStyle
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
