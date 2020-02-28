-- | The environment that contains both the view and the model.
--
-- Copyright   : (C) Keera Studios Ltd, 2018
-- License     : GPL-3
-- Maintainer  : support@keera.co.uk
module Hails.MVC.View.GladeView where

-- Internal libraries
import Graphics.UI.Gtk.Builder

class GladeView a where
   ui :: a -> Builder
