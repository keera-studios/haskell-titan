-- | This module has been generated by hails.
--
-- This module holds the reactive program model. It holds a program model,
-- but includes events that other threads can listen to, so that a change
-- in a part of the model is notified to another part of the program. The
-- reactive model is not necessarily concurrent (it doesn't have its own thread),
-- although a facility is included to make it also concurrent (so that
-- event handlers can be called as soon as they are present).
module Model.ReactiveModel.ReactiveModelInternals
   ( ReactiveModel
   , GRM.basicModel
   -- * Construction
   , GRM.emptyRM
   -- * Access
   , GRM.pendingEvents
   , GRM.pendingHandlers
   -- * Modification
   , GRM.getPendingHandler
   , GRM.onEvent
   , GRM.onEvents
   , GRM.onBasicModel
   , GRM.triggerEvent
   )
  where

-- Internal imports
-- import GenericModel.GenericReactiveModel
import Model.Model
import Model.ReactiveModel.ModelEvents
import qualified Hails.MVC.Model.ReactiveModel as GRM

type ReactiveModel = GRM.ReactiveModel Model ModelEvent (IO ())