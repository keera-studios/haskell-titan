{-# LANGUAGE ScopedTypeVariables #-}
module Controller.Conditions.CurFrameInfo where

import Control.Applicative
import Control.Exception
import Control.Monad
import Control.Monad.IfElse
import Data.Maybe
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
  installConditionInput       cenv
  installConditionModifyInput cenv

  installConditionShowFrame   cenv
  installConditionShowTime    cenv
  installConditionShowDTime   cenv

installConditionModifyInput cenv = do
  let curFrameField'      = mkFieldAccessor selectedFrameField      (model cenv)
      curFrameInputField' = mkFieldAccessor selectedFrameInputField (model cenv)
      curFrameData        = liftR2 (,) curFrameField' curFrameInputField'

  btn <- toolButtonActivateField <$> toolBtnModifyInput (uiBuilder (view cenv))
  (btn `governingR` curFrameData) =:> conditionVMModifyInput cenv

installConditionInput cenv = do
  let curFrameField'      = mkFieldAccessor selectedFrameField (model cenv)
  let curFrameInputField' = mkFieldAccessor selectedFrameInputField (model cenv)
  txtFrameInput' <- entryTextReactive <$> txtFrameInput (uiBuilder (view cenv))

  curFrameField' =:> wrapMW (\f ->
    case f of
      Just ix -> do
        let command = "GetInput " ++ show (ix :: Int)
        n <- sendToYampaSocketSync (extra cenv) command
        case n >>= maybeRead of
          Just (Just x) -> do putStrLn ("Want to show " ++ show x)
                              postGUIAsync $ do reactiveValueWrite txtFrameInput' x
                                                reactiveValueWrite curFrameInputField' (Just x)
          _      -> postGUIAsync $ do reactiveValueWrite txtFrameInput' ""
                                      reactiveValueWrite curFrameInputField' Nothing
      Nothing -> do reactiveValueWrite txtFrameInput' ""
                    reactiveValueWrite curFrameInputField' Nothing
   )

conditionVMModifyInput :: CEnv -> (Maybe Int, Maybe String) -> IO ()
conditionVMModifyInput cenv (Just ix, Just info) = do
    let command = "ModifyInputAt " ++ show ix ++ " " ++ show info
    sendToYampaSocketAsync (extra cenv) command
    print command
conditionVMModifyInput _ _ = return ()

installConditionShowFrame cenv = do
  let curFrameField' = mkFieldAccessor selectedFrameField (model cenv)
  txtFrameNumber' <- entryTextReactive <$> txtFrameNumber      (uiBuilder (view cenv))

  liftR (maybe "" show) curFrameField' =:> txtFrameNumber'

installConditionShowTime cenv = do
  let curFrameField' = mkFieldAccessor selectedFrameField (model cenv)
  txtFrameTime' <- entryTextReactive <$> txtFrameTime (uiBuilder (view cenv))

  curFrameField' =:> wrapMW (\f ->
       case f of
        Just ix -> do
          let command = "GetGTime " ++ show (ix :: Int)
          n <- sendToYampaSocketSync (extra cenv) command
          case n >>= maybeRead of
            Just (Just x) -> do putStrLn ("Want to show " ++ show x)
                                postGUIAsync $ reactiveValueWrite txtFrameTime' (show (x :: Double))
            _      -> postGUIAsync $ reactiveValueWrite txtFrameTime' ""
        Nothing -> reactiveValueWrite txtFrameTime' ""
    )

installConditionShowDTime cenv = do
  let curFrameField' = mkFieldAccessor selectedFrameField (model cenv)
  txtFrameDTime' <- entryTextReactive <$> txtFrameDTime (uiBuilder (view cenv))

  curFrameField' =:> wrapMW (\f ->
      case f of
       Just ix -> do
         let command = "GetDTime " ++ show (ix :: Int)
         n <- sendToYampaSocketSync (extra cenv) command
         case n >>= maybeRead of
           Just (Just x) -> do putStrLn ("Want to show " ++ show x)
                               postGUIAsync $ reactiveValueWrite txtFrameDTime' (show (x :: Double))
           _      -> postGUIAsync $ reactiveValueWrite txtFrameDTime' ""
       Nothing -> reactiveValueWrite txtFrameDTime' ""
    )

maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads
