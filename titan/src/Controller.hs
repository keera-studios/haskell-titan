-- | This contains the main controller. Many operations will be
-- implemented in the Controller.* subsystem. This module simply
-- initialises program.
--
-- Copyright   : (C) Keera Studios Ltd, 2018
-- License     : GPL-3
-- Maintainer  : support@keera.co.uk

-- FIXME: A debug version could be included as a separate controller.

module Controller where

-- Uncomment the following line if you need to capture errors
-- import System.Glib.GError
import Graphics.UI.Gtk

-- Internal imports
import CombinedEnvironment
import Controller.Conditions
import Model.Model
import IOBridge

-- | Starts the program by creating the model,
-- the view, starting all the concurrent threads,
-- installing the handlers for all the conditions
-- and starting the view.
startController :: IO ()
startController = do
  -- Uncomment the following line if you need to debug errors
  -- handleGError (\(GError _ _ em) -> putStrLn em) $ do

    -- Initialise the visual layer
    initView

    -- Create an empty model
    ioBridge <- mkDefaultIOBridge
    cenv <- createCEnv emptyBM ioBridge

    -- Install the model and view handlers
    installHandlers cenv

    -- Notify the system's initialisation
    initialiseSystem $ model cenv

    mainWindow (uiBuilder (view cenv)) >>= widgetShowAll

    -- Run the view
    startView
