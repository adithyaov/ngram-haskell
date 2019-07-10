module Ngram where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

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

