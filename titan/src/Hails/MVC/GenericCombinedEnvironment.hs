-- | This module contains the necessary functions to manipulate a global
-- environment that gives access to the view and the model in a MVC-structured
-- program with a Gtk View.
--
-- It contains the necessary functions to create an environment
-- that holds a View and a Protected Reactive Model.
--
-- Although this code is stable, the design is experimental. Usage in real
-- applications should give way to better implementations.
--
-- Copyright   : (C) Keera Studios Ltd, 2018
-- License     : GPL-3
-- Maintainer  : support@keera.co.uk
module Hails.MVC.GenericCombinedEnvironment
   ( CEnv (view, model, extra)
   , createCEnv
   , installConditions
   , installCondition
   )
  where

-- Internal libraries
import Graphics.UI.View
import Graphics.UI.Gtk.GtkView
import Hails.MVC.Model.ProtectedModel
import Hails.MVC.Model.ReactiveModel (Event)

-- | Given a GUI and a Type for the events, a CEnv contains a View and a
-- Protected Model.
data (GtkGUI a, Event c) => CEnv a b c d = CEnv
  { view  :: GtkView a
  , model :: ProtectedModel b c
  , extra :: d
  }

-- | To create an Environment, we just need to provide the
-- default internal model. The initialisation operations
-- for the view and the protected model are called internally.
createCEnv :: (GtkGUI a, Event c) => b -> d -> IO (CEnv a b c d)
createCEnv emptyBM extraData = do
  m <- startProtectedModel emptyBM
  v <- createView
  return CEnv { view = v, model = m, extra = extraData }

-- | Installs a condition in the Combined Environment.
--
-- NOTE: This is an experimental function and might be removed in the future.
installCondition :: (GtkGUI a, Event c) => CEnv a b c d -> (CEnv a b c d -> IO()) -> IO()
installCondition cenv cond = cond cenv

-- | Installs several conditions in the Combined Environment.
--
-- FIXME: I really don't like the syntax
--   installConditions cenv
--      [ rv1 =:= rf1
--      , ...
--      ]
--
--   I'd rather use
--   installConditions cenv $ do
--     rv1 =:= rf1
--     rv2 =:= rf2
--     ...
--   Which means that I would have to define a monad.
--
-- NOTE: This is an experimental function and might be removed in the future.
installConditions :: (GtkGUI a, Event c) => CEnv a b c d -> [ CEnv a b c d -> IO() ] -> IO ()
installConditions cenv conds = mapM_ (installCondition cenv) conds
