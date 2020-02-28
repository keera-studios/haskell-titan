-- | This module holds the reactive program model. It holds a program
-- model, but includes events that other threads can listen to, so
-- that a change in a part of the model is notified to other parts of
-- the program. The reactive model is not necessarily concurrent (it
-- doesn't have its own thread), although a facility is included to
-- make it also concurrent (so that event handlers can be called as
-- soon as they are present).
--
-- Copyright   : (C) Keera Studios Ltd, 2018
-- License     : GPL-3
-- Maintainer  : support@keera.co.uk
module Model.ReactiveModel
   ( ReactiveModel
   -- * Construction
   , emptyRM
   -- * Access
   , pendingEvents
   , pendingHandlers
   -- * Modification
   , getPendingHandler
   , onEvent
   , module Exported
   )
  where

import Model.ReactiveModel.ReactiveModelInternals
import Hails.MVC.Model.ReactiveModel.Initialisation as Exported
import Model.ReactiveModel.ReactiveFields           as Exported
import Model.ReactiveModel.ModelEvents              as Exported
