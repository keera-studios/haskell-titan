{-# LANGUAGE CPP #-}
module Paths.CustomPaths
  (module Paths_yampa_debugging_gui
#ifndef linux_HOST_OS
  , module Paths.CustomPaths
#endif
  )
 where

import Paths_yampa_debugging_gui

#ifndef linux_HOST_OS
vendorKey :: String
vendorKey = ""

programKey :: String
programKey = ""
#endif
