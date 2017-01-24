{-# LANGUAGE ScopedTypeVariables #-}
module Controller.Conditions.BtnStep where

import Control.Applicative
import Control.Exception
import Control.Monad
import Data.ReactiveValue
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Reactive
import Graphics.UI.Gtk.Reactive.Gtk2
import Hails.MVC.Model.ProtectedModel.Reactive
import Hails.Polling 
import System.IO

import CombinedEnvironment
import IOBridge

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
conditionVMSaveTrace cenv = do
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
  case response of
       ResponseCancel -> putStrLn "You cancelled..."
       ResponseAccept -> do nwf <- fileChooserGetFilename fch
                            case nwf of
                                 Nothing -> putStrLn "Nothing"
                                 Just path -> putStrLn ("New file path is:\n" ++ path)
       ResponseDeleteEvent -> putStrLn "You closed the dialog window..."

  widgetDestroy fch

-- gtkBuilderAccessor "toolBtnRefineTrace"        "Button"
installConditionRefineTrace cenv = void $ do
  btn <- toolButtonActivateField <$> toolBtnRefineTrace (uiBuilder (view cenv))
  btn =:> conditionVMRefineTrace cenv
conditionVMRefineTrace :: CEnv -> IO ()
conditionVMRefineTrace cenv = return ()

-- gtkBuilderAccessor "toolBtnDiscardFuture"      "Button"
installConditionDiscardFuture cenv = void $ do
  btn <- toolButtonActivateField <$> toolBtnDiscardFuture (uiBuilder (view cenv))
  btn =:> conditionVMDiscardFuture cenv
conditionVMDiscardFuture :: CEnv -> IO ()
conditionVMDiscardFuture cenv = return ()

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
  sendToYampaSocketAsync (extra cenv) ("TRAVEL TO " ++ show i)

-- gtkBuilderAccessor "toolBtnTeleportToFrame"    "Button"
installConditionTeleportToFrame cenv = void $ do
  let curFrameField' = mkFieldAccessor selectedFrameField      (model cenv)

  btn <- toolButtonActivateField <$> toolBtnTeleportToFrame (uiBuilder (view cenv))
  (btn `governingR` curFrameField') =:> conditionVMTeleportToFrame cenv

conditionVMTeleportToFrame :: CEnv -> Maybe Int -> IO ()
conditionVMTeleportToFrame cenv Nothing = return ()
conditionVMTeleportToFrame cenv (Just i) = do
  sendToYampaSocketAsync (extra cenv) ("JumpTo" ++ show i)

-- gtkBuilderAccessor "toolBtnIOSenseFrame"       "Button"
installConditionIOSenseFrame cenv = void $ do
  btn <- toolButtonActivateField <$> toolBtnIOSenseFrame (uiBuilder (view cenv))
  btn =:> conditionVMIOSenseFrame cenv
conditionVMIOSenseFrame :: CEnv -> IO ()
conditionVMIOSenseFrame cenv = do
  x <- sendToYampaSocketSync (extra cenv) "Ping"
  print x

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
              
        )
        (\(e :: IOException) -> hPutStrLn stderr "Cannot connect to Yampa socket")

conditionVMDisconnect cenv =
  catch (stopYampaSocket (extra cenv))
        (\(e :: IOException) -> hPutStrLn stderr "Failure trying to disconnect from Yampa socket")

conditionVMStep cenv =
  sendToYampaSocketAsync (extra cenv) "Step"

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
