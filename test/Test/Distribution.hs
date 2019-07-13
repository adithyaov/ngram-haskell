module Test.Distribution where

import Distribution
import Test.Internal

testChoose = 
  [ choose 0 [("a", 3), ("b", 2)] == Just "a"
  , choose 2 [("a", 3), ("b", 2)] == Just "a"
  , choose 5 [("a", 3), ("b", 2)] == Just "b"
  , choose 6 [("a", 3), ("b", 2)] == Nothing ]

test = do
  putStrLn "testing choose"
  foldT testChoose 
