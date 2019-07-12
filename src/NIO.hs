module NIO where

import qualified Data.ByteString as BS
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)
import qualified Data.Serialize as S
import Ngram
import System.IO

defaultOutPath = "./test/default.bin"

data Command
  = MakeCorpus Gram
               FilePath
               (Maybe FilePath)
  | Predict FilePath
            String
            Int

readCorpus :: FilePath -> IO String
readCorpus = readFile

saveModel :: (Ord a, S.Serialize a) => FilePath -> Model a -> IO ()
saveModel p = BS.writeFile p . S.encode

readModel :: (Ord a, S.Serialize a) => FilePath -> IO (Either String (Model a))
readModel p = S.decode <$> BS.readFile p

runCommand :: Command -> IO ()
runCommand (MakeCorpus (Right i) inp mOut) =
  saveModel (fromMaybe defaultOutPath mOut) =<< makeC i <$> readCorpus inp
runCommand (MakeCorpus (Left i) inp mOut) =
  saveModel (fromMaybe defaultOutPath mOut) =<< makeW i <$> readCorpus inp
runCommand (Predict inp s k) = do
  eM <- readModel inp
  case eM of
    (Left err) -> putStrLn err
    (Right m) -> putStrLn =<< predictN k m s
