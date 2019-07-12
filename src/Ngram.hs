module Ngram where

import Control.Monad.Trans.Maybe
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Serialize as S
import Distribution

type Gram = Either Int Int

nSize :: Gram -> Int
nSize (Left i) = i
nSize (Right i) = i

type Model a = (Gram, Map a Int)

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

make :: (Ord a) => Gram -> [a] -> Model [a]
make g = (,) g . freq . chunksOf (nSize g)

makeC :: Int -> String -> Model String
makeC i = make (Right i)

makeW :: Int -> String -> Model [String]
makeW i s = make (Left i) $ wordsP punctuations "" [] s

characters :: String
characters = "abcdefghijklmnopqrstuvwxyz"

extendWith :: (Ord a) => [a] -> [a] -> [a]
extendWith s = concatMap ((s ++) . (: []))

predictNext :: (Eq a, Ord a) => Model [a] -> [a] -> MaybeT IO [a]
predictNext (n, f) s = do
  let (tS', tS) = splitAt (length s - nSize n + 1) s
  fmap (tS' ++) . sample . filterD ((== tS) . init) . mkDistribution $ f

predictN :: (Show a, Ord a) => Int -> Model [a] -> [a] -> IO [a]
predictN 0 _ s = return s
predictN k m s0 = do
  mS1 <- runMaybeT $ predictNext m s0
  case mS1 of
    Nothing -> return s0
    (Just s1) -> predictN (k - 1) m s1

predictND :: (Show a, Ord a) => Int -> Model [a] -> [a] -> IO ()
predictND 0 _ _ = putStrLn "END: 0"
predictND k m s0 = do
  mS1 <- runMaybeT $ predictNext m s0
  case mS1 of
    Nothing -> putStrLn $ "END: " ++ show k
    (Just s1) -> print s1 >> predictND (k - 1) m s1

punctuations :: String
punctuations = " ,."

-- wordsP :: split elements -> accumlator -> prospective result -> input string -> final result
-- This is a function that converts a string to a list of words.
wordsP :: String -> String -> [String] -> String -> [String]
wordsP _ a l [] = filter (not . null) (a : l)
wordsP p a l (x:xs)
  | x `elem` p = wordsP p [] (a : l) xs
  | otherwise = wordsP p (a ++ [x]) l xs
