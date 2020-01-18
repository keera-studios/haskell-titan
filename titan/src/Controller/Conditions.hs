module Controller.Conditions where

import CombinedEnvironment
import Controller.Conditions.Buttons      as Buttons
import Controller.Conditions.CloseIDE     as CloseIDE
import Controller.Conditions.CurFrameInfo as CurFrameInfo
import Controller.Conditions.TraceViewer  as TraceViewer

installHandlers :: CEnv -> IO ()
installHandlers cenv = do
  Buttons.installCondition      cenv
  CloseIDE.installCondition     cenv
  CurFrameInfo.installCondition cenv
  TraceViewer.installCondition  cenv
