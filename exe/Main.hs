module Main where

import NIO
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  runParse args

-- Simple helper function to run the parsed command
runParse :: [String] -> IO ()
runParse args = case parseCommand args of
    Nothing -> putStrLn "Improper arguments"
    (Just c) -> runCommand c

-- A function to parse an input command to the type Command
parseCommand :: [String] -> Maybe Command
parseCommand ["make", "char", n, i] =
  Just $ MakeCorpus (Right . toInt $ n) i Nothing
parseCommand ["make", "char", n, i, o] =
  Just $ MakeCorpus (Right . toInt $ n) i (Just o)
parseCommand ["make", "word", n, i] =
  Just $ MakeCorpus (Left . toInt $ n) i Nothing
parseCommand ["make", "word", n, i, o] =
  Just $ MakeCorpus (Left . toInt $ n) i (Just o)
parseCommand ["predict", i, s, k] = Just $ Predict i s (toInt k)
parseCommand _ = Nothing

-- Helper function to convert string to int
toInt :: String -> Int
toInt x = read x :: Int
