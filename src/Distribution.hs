module Distribution where

import Control.Monad.Trans.Maybe
import Data.Map.Strict (Map, toList)
import System.Random (randomRIO)

-- A distribution type alias
type Distribution a = [(a, Int)]

-- A function to make a distribution given a frequency map
mkDistribution :: (Ord a) => Map a Int -> Distribution a
mkDistribution = toList

-- Choose i'th element of the distribution
choose :: Int -> Distribution a -> Maybe a
choose i [] = Nothing
choose i (x:xs)
  | i - snd x > 0 = choose (i - snd x) xs
  | otherwise = Just (fst x)

-- Size of the distribution
size :: Distribution a -> Int
size = foldr ((+) . snd) 0

-- Emulate the discrete random distribution and pick a random element
sample :: Distribution a -> MaybeT IO a
sample d = MaybeT $ flip choose d <$> randomRIO (1, size d)

-- Helper function to filter a distribution
filterD :: (a -> Bool) -> Distribution a -> Distribution a
filterD f = filter (f . fst)
