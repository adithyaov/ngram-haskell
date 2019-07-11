module Ngram where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Control.Monad.Trans.Maybe
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

predictNext :: (Eq a, Ord a) => Int -> Map [a] Int -> [a] -> MaybeT IO [a]
predictNext n f s = do
  let (tS', tS) = splitAt (length s - n + 1) s
  fmap (tS'++) . sample . filterD ((== tS) . init) . mkDistribution $ f

predictN :: (Show a, Ord a) => Int -> Int -> Map [a] Int -> [a] -> IO ()
predictN 0 _ _ _ = putStrLn "END: 0"
predictN k n f s0 = do
  mS1 <- runMaybeT $ predictNext n f s0
  case mS1 of
    Nothing -> putStrLn $ "END: " ++ show k
    (Just s1) -> print s1 >> predictN (k - 1) n f s1
  




