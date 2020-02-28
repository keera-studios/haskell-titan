-- | Simulation preferences
--
-- Copyright   : (C) Keera Studios Ltd, 2018
-- License     : GPL-3
-- Maintainer  : support@keera.co.uk
module FRP.Titan.Debug.Preferences where

-- | Debugging preferences.
data Preferences = Preferences
  { dumpInput :: Bool -- ^ Dump inputs to local log at every cycle (on simulation machine)
  }

-- | Default simulation preferences that do not dump the input to the log
--   every cycle.
defaultPreferences :: Preferences
defaultPreferences = Preferences
  { dumpInput = False }
