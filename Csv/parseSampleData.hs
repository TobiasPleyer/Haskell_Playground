#!/usr/bin/env stack
{- stack
  script
  --resolver lts-9.13
  --package cassava
  --package vector
  --package text
  --package bytestring
-}

import Control.Monad (mzero)
import Data.Vector (Vector)
import Data.Csv
import qualified Data.ByteString.Lazy as B
import qualified Data.Text as T

data MyRecord = MyRecord {
    policyID :: !Int
  , stateCode :: !T.Text
  , county :: !T.Text
  } deriving (Show)

instance FromRecord MyRecord where
  parseRecord v
    | length v == 3 = MyRecord <$> v .! 0 <*> v .! 1 <*> v .! 2
    | otherwise     = mzero

main = do
  csv_data <- B.readFile "simple_sample_data.csv"
  let csv_parse = decode HasHeader csv_data :: Either String (Vector MyRecord)
  case csv_parse of
    Left error -> putStrLn error
    Right v -> putStrLn (show v)
