module NIO where

import System.IO
import Ngram
import Data.Map.Strict (Map)
import qualified Data.ByteString as BS
import qualified Data.Serialize as S

readCorpus :: FilePath -> IO String
readCorpus = readFile

saveModel :: (Ord a, S.Serialize a) => FilePath -> Model a -> IO ()
saveModel p = BS.writeFile p . S.encode
  
readModel :: FilePath -> IO (Either String (Map String Int))
readModel p = S.decode <$> BS.readFile p 



