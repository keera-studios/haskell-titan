{-# LANGUAGE ScopedTypeVariables #-}
module Controller.Conditions.Buttons where

import Control.Applicative
import Control.Exception
import Control.Monad
import Data.Maybe
import Data.ReactiveValue
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Reactive
import Graphics.UI.Gtk.Reactive.Gtk2
import Hails.MVC.Model.ProtectedModel.Reactive
import Hails.Polling
import System.IO

import CombinedEnvironment
import IOBridge
import Model.Model (defaultFrame)

installCondition :: CEnv -> IO ()
installCondition cenv = do
  installConditionConnect            cenv
  installConditionDisconnect         cenv
  installConditionStep               cenv
  installConditionSkip               cenv
  installConditionStepUntil          cenv
  installConditionSkipBack           cenv
  installConditionRedo               cenv
  installConditionPlay               cenv
  installConditionStop               cenv
  installConditionPause              cenv
  installConditionDeleteTrace        cenv
  installConditionReplayTrace        cenv
  installConditionSaveTrace          cenv
  installConditionLoadTrace          cenv
  installConditionRefineTrace        cenv
  installConditionDiscardFuture      cenv
  installConditionSaveTraceUpToFrame cenv
  installConditionTravelToFrame      cenv
  installConditionTeleportToFrame    cenv
  installConditionIOSenseFrame       cenv
  installConditionModifyTime         cenv

-- gtkBuilderAccessor "toolBtnSaveTrace"          "Button"
installConditionSaveTrace cenv = void $ do
  btn <- toolButtonActivateField <$> toolBtnSaveTrace (uiBuilder (view cenv))
  btn =:> conditionVMSaveTrace cenv

conditionVMSaveTrace :: CEnv -> IO ()
conditionVMSaveTrace cenv = onViewAsync $ do
  window <- mainWindow (uiBuilder (view cenv))
  fch <- fileChooserDialogNew (Just "Save Yampa trace") Nothing
                              FileChooserActionSave
                              [("Cancel", ResponseCancel),
                               ("Save", ResponseAccept)]

  fileChooserSetDoOverwriteConfirmation fch True

  ytrfilt <- fileFilterNew
  fileFilterAddPattern ytrfilt "*.ytr"
  fileFilterSetName ytrfilt "Yampa Trace"
  fileChooserAddFilter fch ytrfilt

  nofilt <- fileFilterNew
  fileFilterAddPattern nofilt "*.*"
  fileFilterSetName nofilt "All Files"
  fileChooserAddFilter fch nofilt

  widgetShow fch
  response <- dialogRun fch
  case response of
       ResponseCancel -> putStrLn "You cancelled..."
       ResponseAccept -> do nwf <- fileChooserGetFilename fch
                            case nwf of
                                 Nothing -> putStrLn "Nothing"
                                 Just path -> do putStrLn ("New file path is:\n" ++ path)
                                                 n <- sendToYampaSocketSync (extra cenv) "GetTrace"
                                                 case n >>= maybeRead of
                                                   Nothing -> return ()
                                                   Just Nothing  -> return ()
                                                   Just (Just s) -> writeFile path s
       ResponseDeleteEvent -> putStrLn "You closed the dialog window..."

  widgetDestroy fch

-- gtkBuilderAccessor "toolBtnLoadTrace"          "Button"
installConditionLoadTrace cenv = void $ do
  btn <- toolButtonActivateField <$> toolBtnLoadTrace (uiBuilder (view cenv))
  btn =:> conditionVMLoadTrace cenv
conditionVMLoadTrace :: CEnv -> IO ()
conditionVMLoadTrace cenv = do
  window <- mainWindow (uiBuilder (view cenv))
  fch <- fileChooserDialogNew (Just "Open Yampa trace") Nothing
                              FileChooserActionOpen
                              [("Cancel", ResponseCancel),
                               ("Load", ResponseAccept)]

  ytrfilt <- fileFilterNew
  fileFilterAddPattern ytrfilt "*.ytr"
  fileFilterSetName ytrfilt "Yampa Trace"
  fileChooserAddFilter fch ytrfilt

  nofilt <- fileFilterNew
  fileFilterAddPattern nofilt "*.*"
  fileFilterSetName nofilt "All Files"
  fileChooserAddFilter fch nofilt

  widgetShow fch
  response <- dialogRun fch
  fp <- case response of
          ResponseCancel -> putStrLn "You cancelled..." >> return Nothing
          ResponseAccept -> do nwf <- fileChooserGetFilename fch
                               case nwf of
                                    Nothing -> putStrLn "Nothing" >> return Nothing
                                    Just path -> putStrLn ("New file path is:\n" ++ path) >> return (Just path)
          ResponseDeleteEvent -> putStrLn "You closed the dialog window..." >> return Nothing

  widgetDestroy fch
  case fp of
    Nothing -> return ()
    (Just p) -> do
      contents <- readFile p
      sendToYampaSocketAsync (extra cenv) ("LoadTraceFromString " ++ show contents)

-- gtkBuilderAccessor "toolBtnRefineTrace"        "Button"
installConditionRefineTrace cenv = void $ do
  btn <- toolButtonActivateField <$> toolBtnRefineTrace (uiBuilder (view cenv))
  btn =:> conditionVMRefineTrace cenv
conditionVMRefineTrace :: CEnv -> IO ()
conditionVMRefineTrace cenv = return ()

-- gtkBuilderAccessor "toolBtnDiscardFuture"      "Button"
installConditionDiscardFuture cenv = void $ do
  let curFrameField' = mkFieldAccessor selectedFrameField      (model cenv)

  btn <- toolButtonActivateField <$> toolBtnDiscardFuture (uiBuilder (view cenv))
  (btn `governingR` curFrameField') =:> conditionVMDiscardFuture cenv

conditionVMDiscardFuture :: CEnv -> Maybe Int -> IO ()
conditionVMDiscardFuture cenv Nothing = return ()
conditionVMDiscardFuture cenv (Just i) = do
  sendToYampaSocketAsync (extra cenv) ("DiscardFuture " ++ show i)

-- gtkBuilderAccessor "toolBtnSaveTraceUpToFrame" "Button"
installConditionSaveTraceUpToFrame cenv = void $ do
  btn <- toolButtonActivateField <$> toolBtnSaveTraceUpToFrame (uiBuilder (view cenv))
  btn =:> conditionVMSaveTraceUpToFrame cenv
conditionVMSaveTraceUpToFrame :: CEnv -> IO ()
conditionVMSaveTraceUpToFrame cenv = do
  window <- mainWindow (uiBuilder (view cenv))
  fch <- fileChooserDialogNew (Just "Save Yampa trace") Nothing
                              FileChooserActionSave
                              [("Cancel", ResponseCancel),
                               ("Save", ResponseAccept)]

  fileChooserSetDoOverwriteConfirmation fch True

  ytrfilt <- fileFilterNew
  fileFilterAddPattern ytrfilt "*.ytr"
  fileFilterSetName ytrfilt "Yampa Trace"
  fileChooserAddFilter fch ytrfilt

  nofilt <- fileFilterNew
  fileFilterAddPattern nofilt "*.*"
  fileFilterSetName nofilt "All Files"
  fileChooserAddFilter fch nofilt

  widgetShow fch
  response <- dialogRun fch
  case response of
       ResponseCancel -> putStrLn "You cancelled..."
       ResponseAccept -> do nwf <- fileChooserGetFilename fch
                            case nwf of
                                 Nothing -> putStrLn "Nothing"
                                 Just path -> putStrLn ("New file path is:\n" ++ path)
       ResponseDeleteEvent -> putStrLn "You closed the dialog window..."

  widgetDestroy fch

-- gtkBuilderAccessor "toolBtnTravelToFrame"      "Button"
installConditionTravelToFrame cenv = void $ do
  let curFrameField' = mkFieldAccessor selectedFrameField      (model cenv)

  btn <- toolButtonActivateField <$> toolBtnTravelToFrame (uiBuilder (view cenv))
  (btn `governingR` curFrameField') =:> conditionVMTravelToFrame cenv

conditionVMTravelToFrame :: CEnv -> Maybe Int -> IO ()
conditionVMTravelToFrame cenv Nothing = return ()
conditionVMTravelToFrame cenv (Just i) = do
  sendToYampaSocketAsync (extra cenv) ("TravelToFrame " ++ show i)

-- gtkBuilderAccessor "toolBtnTeleportToFrame"    "Button"
installConditionTeleportToFrame cenv = void $ do
  let curFrameField' = mkFieldAccessor selectedFrameField      (model cenv)

  btn <- toolButtonActivateField <$> toolBtnTeleportToFrame (uiBuilder (view cenv))
  (btn `governingR` curFrameField') =:> conditionVMTeleportToFrame cenv

conditionVMTeleportToFrame :: CEnv -> Maybe Int -> IO ()
conditionVMTeleportToFrame cenv Nothing = return ()
conditionVMTeleportToFrame cenv (Just i) = do
  sendToYampaSocketAsync (extra cenv) ("JumpTo " ++ show i)

-- gtkBuilderAccessor "toolBtnIOSenseFrame"       "Button"
installConditionIOSenseFrame cenv = void $ do
  let curFrameField' = mkFieldAccessor selectedFrameField      (model cenv)

  btn <- toolButtonActivateField <$> toolBtnIOSenseFrame (uiBuilder (view cenv))
  (btn `governingR` curFrameField') =:> conditionVMIOSenseFrame cenv

conditionVMIOSenseFrame :: CEnv -> Maybe Int -> IO ()
conditionVMIOSenseFrame cenv Nothing = return ()
conditionVMIOSenseFrame cenv (Just i) = do
  sendToYampaSocketAsync (extra cenv) ("IOSense " ++ show i)

-- gtkBuilderAccessor "toolBtnModifyTime"         "Button"
installConditionModifyTime cenv = void $ do
  btn <- toolButtonActivateField <$> toolBtnModifyTime (uiBuilder (view cenv))
  btn =:> conditionVMModifyTime cenv
conditionVMModifyTime :: CEnv -> IO ()
conditionVMModifyTime cenv = return ()

installConditionConnect cenv = void $ do
  btn <- toolButtonActivateField <$> toolBtnConnect (uiBuilder (view cenv))
  btn =:> conditionVMConnect cenv

installConditionDisconnect cenv = void $ do
  btn <- toolButtonActivateField <$> toolBtnDisconnect (uiBuilder (view cenv))
  btn =:> conditionVMDisconnect cenv

installConditionStep cenv = void $ do
  btn <- toolButtonActivateField <$> toolBtnStep (uiBuilder (view cenv))
  btn =:> conditionVMStep cenv

installConditionStepUntil cenv = void $ do
  btn <- toolButtonActivateField <$> toolBtnStepUntil (uiBuilder (view cenv))
  btn =:> conditionVMStepUntil cenv

installConditionSkip cenv = void $ do
  btn <- toolButtonActivateField <$> toolBtnSkip (uiBuilder (view cenv))
  btn =:> conditionVMSkip cenv

installConditionSkipBack cenv = void $ do
  btn <- toolButtonActivateField <$> toolBtnSkipBack (uiBuilder (view cenv))
  btn =:> conditionVMSkipBack cenv

installConditionRedo cenv = void $ do
  btn <- toolButtonActivateField <$> toolBtnRedo (uiBuilder (view cenv))
  btn =:> conditionVMRedo cenv

installConditionPlay cenv = void $ do
  btn <- toolButtonActivateField <$> toolBtnPlay (uiBuilder (view cenv))
  btn =:> conditionVMPlay cenv

installConditionStop cenv = void $ do
  btn <- toolButtonActivateField <$> toolBtnStop (uiBuilder (view cenv))
  btn =:> conditionVMStop cenv

installConditionPause cenv = void $ do
  btn <- toolButtonActivateField <$> toolBtnPause (uiBuilder (view cenv))
  btn =:> conditionVMPause cenv

installConditionDeleteTrace cenv = void $ do
  btn <- toolButtonActivateField <$> toolBtnDeleteTrace (uiBuilder (view cenv))
  btn =:> conditionVMDeleteTrace cenv

installConditionReplayTrace cenv = void $ do
  btn <- toolButtonActivateField <$> toolBtnReplayTrace (uiBuilder (view cenv))
  btn =:> conditionVMReplayTrace cenv

conditionVMConnect cenv =
  catch (do startYampaSocket (extra cenv)
            r <- getFromYampaSocketSync (extra cenv)
            print r
            eventField <- pollingReactive (getFromEventSocketSync (extra cenv)) (Just 10)
            debugEntry <- txtDebug (uiBuilder (view cenv))
            let debugEntrySetter v = postGUIAsync (get debugEntry textViewBuffer >>= (\b -> set b [textBufferText := v]))
            liftR show eventField =:> debugEntrySetter
            ((const ()) <^> (guardRO' eventField (== Just "CurrentFrameChanged"))) =:> conditionVMTimeChanged cenv
            ((const ()) <^> (guardRO' eventField (== Just "CurrentFrameChanged"))) =:> conditionVMFrameChanged cenv
            ((const ()) <^> (guardRO' eventField (== Just "HistoryChanged")))      =:> conditionVMHistoryChanged cenv

        )
        (\(e :: IOException) -> hPutStrLn stderr "Cannot connect to Yampa socket")

-- | Make this reactive
conditionVMTimeChanged cenv = do
  entryGT <- txtGlobalTime (uiBuilder (view cenv))
  n <- sendToYampaSocketSync (extra cenv) "GetCurrentTime"
  putStrLn $ "Received " ++ show n
  case maybe [] words n of
    ["CurrentTime", m] -> postGUIAsync $ entrySetText entryGT m
    _                  -> return ()

-- | Make this reactive
conditionVMFrameChanged cenv = do
  let curSimFrame' = mkFieldAccessor curSimFrameField (model cenv)
  entryGT <- txtGlobalTime (uiBuilder (view cenv))
  n <- sendToYampaSocketSync (extra cenv) "GetCurrentFrame"
  putStrLn $ "Received " ++ show n
  case maybe [] words n of
    ["CurrentFrame", m] -> do case maybeRead m of
                                Just m' -> do putStrLn $ "Current Frame is " ++ show m'
                                              reactiveValueWrite curSimFrame' (Just m')
                                Nothing -> do reactiveValueWrite curSimFrame' Nothing
    _                   -> do reactiveValueWrite curSimFrame' Nothing

-- | Make this reactive
conditionVMHistoryChanged cenv = do
  let fs = mkFieldAccessor framesField (model cenv)
  n <- sendToYampaSocketSync (extra cenv) "SummarizeHistory"
  putStrLn $ "Received " ++ show n
  case maybe [] words n of
    ["CurrentHistory", m] -> case maybeRead m of
                               Just m' -> do putStrLn $ "Show have now " ++ show m' ++ " frames"
                                             reactiveValueWrite fs $ map defaultFrame [0..(m'-1)]
                               Nothing -> do putStrLn "Could not read any number of frames"
                                             reactiveValueWrite fs []
    _                      ->  do putStrLn "Could not read any number of frames"
                                  reactiveValueWrite fs []


conditionVMDisconnect cenv =
  catch (stopYampaSocket (extra cenv))
        (\(e :: IOException) -> hPutStrLn stderr "Failure trying to disconnect from Yampa socket")

conditionVMStep cenv =
  void $ sendToYampaSocketSync (extra cenv) "Step"

conditionVMSkip cenv =
  sendToYampaSocketAsync (extra cenv) "Skip"

conditionVMStepUntil cenv =
  sendToYampaSocketAsync (extra cenv) "StepUntil"

conditionVMSkipBack cenv =
  sendToYampaSocketAsync (extra cenv) "SkipBack"

conditionVMRedo cenv =
  sendToYampaSocketAsync (extra cenv) "Redo"

conditionVMPlay cenv =
  sendToYampaSocketAsync (extra cenv) "Play"

conditionVMStop cenv =
  sendToYampaSocketAsync (extra cenv) "Stop"

conditionVMPause cenv =
  sendToYampaSocketAsync (extra cenv) "Pause"

conditionVMDeleteTrace cenv =
  sendToYampaSocketAsync (extra cenv) "DeleteTrace"

conditionVMReplayTrace cenv =
  sendToYampaSocketAsync (extra cenv) "ReplayTrace"

maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads
