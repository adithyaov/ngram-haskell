module Ngram where

import Control.Monad.Trans.Maybe
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Serialize as S
import Distribution

-- Helper type alias
type Gram = Either Int Int

-- A simple unwrapping function
nSize :: Gram -> Int
nSize (Left i) = i
nSize (Right i) = i

-- Helper type alias for Model
type Model a = (Gram, Map a Int)

-- Break any list of elements into chunks of n
chunksOf :: Int -> [a] -> [[a]]
chunksOf i s = filter ((== i) . length) $ taker i s
  where
    taker _ [] = []
    taker i t@(x:xs) = take i t : taker i xs

-- A function to count chunk frequency
freq :: (Ord a) => [a] -> Map a Int
freq = foldr addFreq Map.empty
  where
    addFreq w m =
      case Map.lookup w m of
        Nothing -> Map.insert w 1 m
        Just x -> Map.insert w (x + 1) m

-- a function to make a Model given an input
make :: (Ord a) => Gram -> [a] -> Model [a]
make g = (,) g . freq . chunksOf (nSize g)

-- derivative of 'make' to make character level ngram
makeC :: Int -> String -> Model String
makeC i = make (Right i)
 
-- derivative of 'make' to make word level ngram
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

-- A function to predict the n characters/words depending on the model
predictN :: (Show a, Ord a) => Int -> Model [a] -> [a] -> IO [a]
predictN 0 _ s = return s
predictN k m s0 = do
  mS1 <- runMaybeT $ predictNext m s0
  case mS1 of
    Nothing -> return s0
    (Just s1) -> predictN (k - 1) m s1

-- A version of predictN for debugging
predictND :: (Show a, Ord a) => Int -> Model [a] -> [a] -> IO ()
predictND 0 _ _ = putStrLn "END: 0"
predictND k m s0 = do
  mS1 <- runMaybeT $ predictNext m s0
  case mS1 of
    Nothing -> putStrLn $ "END: " ++ show k
    (Just s1) -> print s1 >> predictND (k - 1) m s1

-- Punctuation list to split the words
punctuations :: String
punctuations = [' ', ',', '.', '\n']

-- wordsP :: split elements -> accumlator -> prospective result -> input string -> final result
-- This is a function that converts a string to a list of words breaking the punctuations
wordsP :: String -> String -> [String] -> String -> [String]
wordsP _ a l [] = filter (not . null) (a : l)
wordsP p a l (x:xs)
  | x `elem` p = wordsP p [] (a : l) xs
  | otherwise = wordsP p (a ++ [x]) l xs
