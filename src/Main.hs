module Main where

import System.Environment
import NIO
import Ngram
import qualified Data.Map.Strict as Map

defaultSaveLocation = "/mnt/c/Users/mota/Desktop/LinuxWorkStation/Prog/ngram/test/default.bn"

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["make", c, n] -> saveModel defaultSaveLocation . make (read n :: Int) =<< readCorpus c 
    ["predict", m, t, k] -> predict m t (read k :: Int)

predict p t k = do
  eM <- readModel p
  case eM of
    (Left err) -> putStrLn err
    (Right m) -> putStrLn =<< predictN k (length . head . Map.keys $ m) m t


