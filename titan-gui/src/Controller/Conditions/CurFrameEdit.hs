{-# LANGUAGE ScopedTypeVariables #-}
module Controller.Conditions.CurFrameEdit where

import Control.Applicative
import Control.Exception
import Control.Monad
import Data.ReactiveValue
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Reactive
import Graphics.UI.Gtk.Reactive.Gtk2
import Hails.MVC.Model.ProtectedModel.Reactive
import System.IO

import CombinedEnvironment
import IOBridge


installCondition :: CEnv -> IO ()
installCondition cenv = do
--  installConditionInput       cenv
  installConditionModifyInput cenv

installConditionModifyInput cenv = do
  let curFrameField'      = mkFieldAccessor selectedFrameField      (model cenv)
      curFrameInputField' = mkFieldAccessor selectedFrameInputField (model cenv)
      curFrameData        = liftR2 (,) curFrameField' curFrameInputField'

  btn <- toolButtonActivateField <$> toolBtnModifyInput (uiBuilder (view cenv))
  (btn `governingR` curFrameData) =:> conditionVMModifyInput cenv

conditionVMModifyInput :: CEnv -> (Maybe Int, Maybe String) -> IO ()
conditionVMModifyInput cenv (mn, mi) = do
  case (mn, mi) of
    (Just ix, Just info) -> do
       let command = "UPDATE FRAME INPUT " ++ show ix ++ " " ++ show info
       sendToYampaSocketAsync (extra cenv) command
       print command

    otherwise -> return ()

-- installConditionInput cenv = do
--   let curFrameInputField' = mkFieldAccessor selectedFrameInputField (model cenv)
--   curFrameInputTxt <- entryTextReactive <$> txtFrameInput (uiBuilder (view cenv))
--   curFrameInputField' =:= curFrameInputTxt
