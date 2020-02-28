-- | Condition: The program ends when the main window is closed
--
-- Copyright   : (C) Keera Studios Ltd, 2018
-- License     : GPL-3
-- Maintainer  : support@keera.co.uk
module Controller.Conditions.CloseIDE
    (installCondition)
  where

-- External libraries
import Control.Arrow
import Control.Monad
import Control.Monad.Reader (liftIO)
import Graphics.UI.Gtk

-- Internal libraries
import CombinedEnvironment hiding (installCondition)
import View.Objects

-- TODO:
-- filter :: (a -> Bool) -> RV a -> RV a
-- only changes when f a == True
--
-- edge :: (a -> Bool) -> RV a -> RV a
-- only changes when f a becomes True after being False
--
-- also filterM and edgeM
--
-- liftIO :: m a -> RO a m
--
-- wrapIO :: m a -> (a -> m ()) -> RV a m -- Passive
--
-- The following probably exists already:
-- wrapWO :: (a -> m b) -> RV a
--

installCondition :: CEnv -> IO()
installCondition cenv = void $ do
  mw <- mainWindow $ uiBuilder $ view cenv
  mw `on` deleteEvent $ liftIO $ conditionVM cenv

-- | Enforces the condition in View to Model direction
conditionVM :: CEnv -> IO Bool
conditionVM cenv = do
  b <- checkExit cenv
  when b $ onViewAsync destroyView
  return (not b)

-- Returns true if the operation can continue, false otherwise
checkExit :: CEnv -> IO Bool
checkExit cenv = do
  let (v,m) = (view &&& model) cenv
  let ui = uiBuilder v
  win <- mainWindow ui
  dialog <- messageDialogNew (Just win) [DialogModal] MessageQuestion ButtonsNone
              "Exit?"
  dialogAddButton dialog "Cancel"               ResponseCancel
  dialogAddButton dialog "Exit"                 ResponseYes

  dialogSetDefaultResponse dialog ResponseCancel

  -- Run the dialog and process the result
  widgetShowAll dialog
  r <- dialogRun dialog
  res <- case r of
           ResponseYes -> return True
           _           -> return False -- Cancel or close the dialog without answering
  widgetDestroy dialog
  return res
