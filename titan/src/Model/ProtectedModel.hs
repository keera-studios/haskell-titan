-- |
--
-- Copyright   : (C) Keera Studios Ltd, 2018
-- License     : GPL-3
-- Maintainer  : support@keera.co.uk
module Model.ProtectedModel
   ( ProtectedModel
   , onEvent
   , waitFor
   , module Exported
   )
  where

import Model.ProtectedModel.ProtectedModelInternals
import Model.ReactiveModel.ModelEvents               as Exported
import Model.ProtectedModel.ProtectedFields          as Exported
import Hails.MVC.Model.ProtectedModel.Initialisation as Exported
