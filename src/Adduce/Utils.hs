
-- | Various utility functions used throughout the interpreter.
module Adduce.Utils where

import Control.Monad (foldM)
import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Data.Map as Map (Map, insert, lookup, toList)
import Data.Bifunctor (Bifunctor, bimap)

-- | Return the left-hand `Maybe` if it is a `Just`, otherwise return the right-hand `Maybe`.
(?:) :: Maybe a -> Maybe a -> Maybe a
(Just x) ?: _ = Just x
_        ?: x = x

-- | Inverse composition operator.
(~>) :: (a -> b) -> (b -> c) -> (a -> c)
f ~> g = g . f

-- | Return the value from an `Either` that has the same left and right types.
fromEither :: Either a a -> a
fromEither = either id id

-- | Partial fold operation which terminates early if the fold function returns a `Left`.
foldPartial :: (b -> a -> Either b b) -> b -> [a] -> b
foldPartial f b = fromEither . foldM f b

-- | IO-wrapped partial fold operation which terminates early if the fold function returns an `IO Left`.
foldPartialIO :: (b -> a -> IO (Either b b)) -> b -> [a] -> IO b
foldPartialIO f = foldM (\b a -> fromEither `fmap` f b a)

-- | Specialized version of `Data.Bifunctor.bimap` for applying the same function to both arguments.
bimapBoth :: Bifunctor p => (a -> b) -> p a a -> p b b
bimapBoth f = bimap f f

-- | Trim whitespace from both ends of a string.
trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

