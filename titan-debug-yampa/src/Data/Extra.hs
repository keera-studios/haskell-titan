-- | Utility functions
module Data.Extra where

import Data.Maybe

-- * Lists

-- takeLast n l = reverse $ take n $ reverse l

appAt :: Int -> (a -> a) -> [a] -> [a]
appAt _ f [] = []
appAt 0 f (x:xs) = f x : xs
appAt n f (x:xs) = x : appAt (n-1) f xs

-- * Maybe

maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads

--- * Either

-- TODO: Remove
fromLeft :: Either a b -> a
fromLeft (Left a) = a

-- TODO: Remove
fromRight :: Either a b -> b
fromRight (Right b) = b
