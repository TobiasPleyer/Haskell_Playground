module Main where


import           System.IO
import           System.Environment
import           Data.Csv
import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as BC
import           Pipes
import qualified Pipes.Csv             as PCsv
import qualified Pipes.Prelude         as PP
import qualified Pipes.ByteString      as PB


data Sample = Sample
  { timestamp :: Float
  , category1 :: Int
  , category2 :: Int
  } deriving (Show)

instance FromNamedRecord Sample where
  parseNamedRecord s =
    Sample <$> s .: BC.pack "timestamp"
           <*> s .: BC.pack "category1"
           <*> s .: BC.pack "category2"

sampleRec ~(Sample t c1 c2) = [BC.pack "timestamp" .= t
                              ,BC.pack "category1" .= c1
                              ,BC.pack "category2" .= c2
                              ]

instance ToNamedRecord Sample where
  toNamedRecord = namedRecord . sampleRec


main :: IO ()
main = do
  filename <- head <$> getArgs
  withFile filename ReadMode $ \hIn ->
    withFile "out.txt" WriteMode $ \hOut ->
      runEffect $ PB.fromHandle hIn >-> PB.toHandle hOut
