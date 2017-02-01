module Controller.Conditions where

import CombinedEnvironment
import Controller.Conditions.Buttons      as Buttons
import Controller.Conditions.CurFrameEdit as CurFrameEdit
import Controller.Conditions.TraceViewer  as TraceViewer
import Controller.Conditions.CloseIDE     as CloseIDE

installHandlers :: CEnv -> IO ()
installHandlers cenv = do
  Buttons.installCondition      cenv
  CurFrameEdit.installCondition cenv
  TraceViewer.installCondition  cenv
  CloseIDE.installCondition     cenv
