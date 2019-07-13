module Test.Ngram where

import qualified Data.Map.Strict as Map
import Ngram
import Test.Internal

testChunksOf =
  [ chunksOf 3 "abcde" == ["abc", "bcd", "cde"]
  , chunksOf 3 "ab" == []]

testFreq = 
  [ freq [1, 2, 1] == Map.fromList [(1, 2 :: Int), (2, 1)] ]

testWordsP =
  [ wordsP punctuations "" [] "a sentence,or. not is" == ["is", "not", "or", "sentence", "a"] ]

test = do
  putStrLn "testing chunksOf"
  foldT testChunksOf 
  putStrLn "testing freq"
  foldT testFreq
  putStrLn "testing wordsP"
  foldT testWordsP
