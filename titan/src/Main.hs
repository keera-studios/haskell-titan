-- | This is the main program with which the graphical app is launched.
--
-- Copyright   : (C) Keera Studios Ltd, 2018
-- License     : GPL-3
-- Maintainer  : support@keera.co.uk
module Main where

-- Internal imports
import Controller

-- |The app starts here. Here we simply call the controller, which
-- takes control from now on.
main :: IO ()
main = startController
