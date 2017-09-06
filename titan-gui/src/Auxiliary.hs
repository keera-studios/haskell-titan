module Auxiliary where

import Data.Maybe

maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads
