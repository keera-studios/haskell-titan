{-# LANGUAGE ScopedTypeVariables #-}
module Controller.Conditions.Buttons where

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
import Hails.Polling
import System.IO

import CombinedEnvironment
import IOBridge
import Model.Model (defaultFrame)
import FRP.Titan.Protocol

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
                                                   Just (Just s) -> writeFile path s
                                                   _             -> return ()
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
                                    Nothing   -> putStrLn "Nothing" >> return Nothing
                                    Just path -> putStrLn ("New file path is:\n" ++ path) >> return (Just path)
          ResponseDeleteEvent -> putStrLn "You closed the dialog window..." >> return Nothing

  widgetDestroy fch
  awhen fp $ \p -> do
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
  let curFrameField' = mkFieldAccessor selectedFrameField (model cenv)

  btn <- toolButtonActivateField <$> toolBtnDiscardFuture (uiBuilder (view cenv))
  (btn `governingR` curFrameField') =:> conditionVMDiscardFuture cenv

conditionVMDiscardFuture :: CEnv -> Maybe Int -> IO ()
conditionVMDiscardFuture cenv Nothing  = return ()
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
  let curFrameField' = mkFieldAccessor selectedFrameField (model cenv)

  btn <- toolButtonActivateField <$> toolBtnTravelToFrame (uiBuilder (view cenv))
  (btn `governingR` curFrameField') =:> conditionVMTravelToFrame cenv

conditionVMTravelToFrame :: CEnv -> Maybe Int -> IO ()
conditionVMTravelToFrame cenv Nothing  = return ()
conditionVMTravelToFrame cenv (Just i) =
  sendToYampaSocketAsync (extra cenv) ("TravelToFrame " ++ show i)

-- gtkBuilderAccessor "toolBtnTeleportToFrame"    "Button"
installConditionTeleportToFrame cenv = void $ do
  let curFrameField' = mkFieldAccessor selectedFrameField (model cenv)

  btn <- toolButtonActivateField <$> toolBtnTeleportToFrame (uiBuilder (view cenv))
  (btn `governingR` curFrameField') =:> conditionVMTeleportToFrame cenv

conditionVMTeleportToFrame :: CEnv -> Maybe Int -> IO ()
conditionVMTeleportToFrame cenv Nothing  = return ()
conditionVMTeleportToFrame cenv (Just i) =
  sendToYampaSocketAsync (extra cenv) ("JumpTo " ++ show i)

-- gtkBuilderAccessor "toolBtnIOSenseFrame"       "Button"
installConditionIOSenseFrame cenv = void $ do
  let curFrameField' = mkFieldAccessor selectedFrameField (model cenv)

  btn <- toolButtonActivateField <$> toolBtnIOSenseFrame (uiBuilder (view cenv))
  (btn `governingR` curFrameField') =:> conditionVMIOSenseFrame cenv

conditionVMIOSenseFrame :: CEnv -> Maybe Int -> IO ()
conditionVMIOSenseFrame cenv Nothing  = return ()
conditionVMIOSenseFrame cenv (Just i) =
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

installConditionSkip cenv = do
  btn <- toolButtonActivateField <$> toolBtnSkip (uiBuilder (view cenv))
  btn =:> conditionVMSkip cenv

installConditionSkipBack cenv = do
  btn <- toolButtonActivateField <$> toolBtnSkipBack (uiBuilder (view cenv))
  btn =:> conditionVMSkipBack cenv

installConditionRedo cenv = do
  btn <- toolButtonActivateField <$> toolBtnRedo (uiBuilder (view cenv))
  btn =:> conditionVMRedo cenv

installConditionPlay cenv = do
  btn <- toolButtonActivateField <$> toolBtnPlay (uiBuilder (view cenv))
  btn =:> conditionVMPlay cenv

installConditionStop cenv = do
  btn <- toolButtonActivateField <$> toolBtnStop (uiBuilder (view cenv))
  btn =:> conditionVMStop cenv

installConditionPause cenv = do
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
            ((const ()) <^> (guardRO' eventField (== Just "HistoryChanged")))      =:> conditionVMMaxTimeChanged cenv

        )
        (\(e :: IOException) -> hPutStrLn stderr "Cannot connect to Yampa socket")

-- | Make this reactive
conditionVMMaxTimeChanged cenv = do
  entryGT <- txtMaxTime (uiBuilder (view cenv))
  maxTime <- sendToYampaSocketSync (extra cenv) (show GetMaxTime)
  putStrLn $ "Received " ++ show maxTime
  case maxTime >>= maybeRead of
    Just (MaxTime time) -> postGUIAsync $ entrySetText entryGT $ show time
    _                   -> return ()

-- | Make this reactive
conditionVMTimeChanged cenv = do
  entryGT <- txtGlobalTime (uiBuilder (view cenv))
  curTime <- sendToYampaSocketSync (extra cenv) (show GetCurrentTime)
  putStrLn $ "Received " ++ show curTime
  case curTime >>= maybeRead of
    Just (CurrentTime time) -> postGUIAsync $ entrySetText entryGT $ show time
    _                       -> return ()

-- | Make this reactive
conditionVMFrameChanged cenv = do
  let curSimFrame' = mkFieldAccessor curSimFrameField (model cenv)
  entryGT <- txtGlobalTime (uiBuilder (view cenv))
  n       <- sendToYampaSocketSync (extra cenv) (show GetCurrentFrame)
  case n >>= maybeRead of
    Just (CurrentFrame m') -> do putStrLn $ "Current Frame is " ++ show m'
                                 reactiveValueWrite curSimFrame' (Just m')
    _                      -> reactiveValueWrite curSimFrame' Nothing

-- | Make this reactive
conditionVMHistoryChanged cenv = do
  let fs = mkFieldAccessor framesField (model cenv)
  n <- sendToYampaSocketSync (extra cenv) (show SummarizeHistory)
  putStrLn $ "Received " ++ show n
  case n >>= maybeRead of
    Just (CurrentHistory m') -> do putStrLn $ "Show have now " ++ show m' ++ " frames"
                                   reactiveValueWrite fs $ map defaultFrame [0..(m'-1)]
    _                        -> do putStrLn "Could not read any number of frames"
                                   reactiveValueWrite fs []


conditionVMDisconnect cenv =
  catch (stopYampaSocket (extra cenv))
        (\(e :: IOException) -> hPutStrLn stderr "Failure trying to disconnect from Yampa socket")

conditionVMStep cenv =
  sendToYampaSocketAsync (extra cenv) (show Step)

conditionVMSkip cenv =
  sendToYampaSocketAsync (extra cenv) (show Skip)

conditionVMStepUntil cenv =
  sendToYampaSocketAsync (extra cenv) (show StepUntil)

conditionVMSkipBack cenv =
  sendToYampaSocketAsync (extra cenv) (show SkipBack)

conditionVMRedo cenv =
  sendToYampaSocketAsync (extra cenv) (show Redo)

conditionVMPlay cenv =
  sendToYampaSocketAsync (extra cenv) (show Play)

conditionVMStop cenv =
  sendToYampaSocketAsync (extra cenv) (show Stop)

conditionVMPause cenv =
  sendToYampaSocketAsync (extra cenv) (show Pause)

conditionVMDeleteTrace cenv =
  sendToYampaSocketAsync (extra cenv) (show DeleteTrace)

conditionVMReplayTrace cenv =
  sendToYampaSocketAsync (extra cenv) (show ReplayTrace)

maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads
