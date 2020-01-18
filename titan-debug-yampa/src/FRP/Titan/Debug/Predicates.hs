{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

-- | Debugging predicates
module FRP.Titan.Debug.Predicates where


-- | A notion of temporal point-wise (time-wise) predicate to be tested
-- during a simulation point. It needs to be something we can read
-- from the GUI bridge so that we can interactively read commands
-- from the user and test them.

-- TODO: Possibly use this:
-- https://hackage.haskell.org/package/hint

class Read p => Pred p i o | p -> i, p -> o where
  -- | Evaluate a predicate for a given input sample and a given output.
  evalPred :: p -> Maybe Double -> i -> o -> Bool
