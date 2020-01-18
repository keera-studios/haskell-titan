-- | Contains basic operations related to the GUI
--
module Hails.MVC.View.DefaultView where

-- External libraries
import Graphics.UI.Gtk
import Graphics.UI.Gtk.StreamChart

import Model.Model as Model

import Hails.MVC.View.GladeView

-- | This datatype should hold the elements that we must track in the
-- future (for instance, treeview models)
data View = View
  { uiBuilder   :: Builder
  , streamChart :: StreamChart Model.Frame
  }

instance GladeView View where
  ui = uiBuilder
