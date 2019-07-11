module Ngram where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Distribution

chunksOf :: Int -> [a] -> [[a]]
chunksOf i s = filter ((== i) . length) $ taker i s
  where
    taker _ [] = []
    taker i t@(x:xs) = take i t : taker i xs

freq :: (Ord a) => [a] -> Map a Int
freq = foldr addFreq Map.empty
  where
    addFreq w m =
      case Map.lookup w m of
        Nothing -> Map.insert w 1 m
        Just x -> Map.insert w (x + 1) m

make :: (Ord a) => Int -> [a] -> Map [a] Int
make i = freq . chunksOf i

characters :: String
characters = "abcdefghijklmnopqrstuvwxyz"

extendWith :: (Ord a) => [a] -> [a] -> [a]
extendWith s = concatMap ((s ++) . (: []))

predictNext :: (Eq a, Ord a) => Int -> Map [a] Int -> [a] -> IO (Maybe [a])
predictNext n f s = do
  let tS = drop (length s - n + 1) s
  sample . filterD ((== tS) . init) . mkDistribution $ f
