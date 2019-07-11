module Distribution where

import Data.Map.Strict (Map, toList)
import System.Random (randomRIO)
import Control.Monad.Trans.Maybe

type Distribution a = [(a, Int)]

mkDistribution :: (Ord a) => Map a Int -> Distribution a
mkDistribution = toList

choose :: Int -> Distribution a -> Maybe a
choose i [] = Nothing
choose i (x:xs)
  | i - snd x > 0 = choose (i - snd x) xs
  | otherwise = Just (fst x)

size :: Distribution a -> Int
size = foldr ((+) . snd) 0

sample :: Distribution a -> MaybeT IO a
sample d = MaybeT $ flip choose d <$> randomRIO (1, size d)

filterD :: (a -> Bool) -> Distribution a -> Distribution a
filterD f = filter (f . fst)
