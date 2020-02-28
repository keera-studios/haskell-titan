{-# LANGUAGE TemplateHaskell #-}
-- |
--
-- Copyright   : (C) Keera Studios Ltd, 2018
-- License     : GPL-3
-- Maintainer  : support@keera.co.uk
module View.Objects where

-- External imports
import Graphics.UI.Gtk
import Hails.Graphics.UI.Gtk.Builder
import Hails.Graphics.UI.Gtk.THBuilderAccessor

-- Internal imports
import Paths

loadInterface :: IO Builder
loadInterface = loadDefaultInterface getDataFileName

-- gtkBuilderAccessor element name type name
-- gtkBuilderAccessor "mainMenu"   "Menu"
gtkBuilderAccessor "mainWindow"                "Window"
gtkBuilderAccessor "toolBtnConnect"            "ToolButton"
gtkBuilderAccessor "toolBtnDisconnect"         "ToolButton"
gtkBuilderAccessor "toolBtnStep"               "ToolButton"
gtkBuilderAccessor "toolBtnSkip"               "ToolButton"
gtkBuilderAccessor "toolBtnStepUntil"          "ToolButton"
gtkBuilderAccessor "toolBtnSkipBack"           "ToolButton"
gtkBuilderAccessor "toolBtnRedo"               "ToolButton"
gtkBuilderAccessor "toolBtnPlay"               "ToolButton"
gtkBuilderAccessor "toolBtnStop"               "ToolButton"
gtkBuilderAccessor "toolBtnPause"              "ToolButton"
gtkBuilderAccessor "toolBtnDeleteTrace"        "ToolButton"
gtkBuilderAccessor "toolBtnReplayTrace"        "ToolButton"
gtkBuilderAccessor "toolBtnSaveTrace"          "ToolButton"
gtkBuilderAccessor "toolBtnLoadTrace"          "ToolButton"
gtkBuilderAccessor "toolBtnRefineTrace"        "ToolButton"
gtkBuilderAccessor "toolBtnDiscardFuture"      "ToolButton"
gtkBuilderAccessor "toolBtnSaveTraceUpToFrame" "ToolButton"
gtkBuilderAccessor "toolBtnTravelToFrame"      "ToolButton"
gtkBuilderAccessor "toolBtnTeleportToFrame"    "ToolButton"
gtkBuilderAccessor "toolBtnIOSenseFrame"       "ToolButton"
gtkBuilderAccessor "toolBtnModifyInput"        "ToolButton"
gtkBuilderAccessor "toolBtnModifyTime"         "ToolButton"
gtkBuilderAccessor "txtFrameNumber"            "Entry"
gtkBuilderAccessor "txtFrameTime"              "Entry"
gtkBuilderAccessor "txtFrameDTime"             "Entry"
gtkBuilderAccessor "txtFrameInput"             "Entry"
gtkBuilderAccessor "txtFrameLast"              "Entry"
gtkBuilderAccessor "txtGlobalTime"             "Entry"
gtkBuilderAccessor "txtMaxTime"                "Entry"
gtkBuilderAccessor "txtFinished"               "Entry"
gtkBuilderAccessor "txtDebug"                  "TextView"
gtkBuilderAccessor "scrollFrameSelection"      "ScrolledWindow"
