module Main where


import           System.IO
import           System.Environment
import           Data.Bits
import           Data.Word
import           Data.Csv
import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as BC
import           Pipes
import qualified Pipes.Csv             as PCsv
import qualified Pipes.Prelude         as PP
import qualified Pipes.ByteString      as PB


data Sample = Sample
  { timestamp :: Float
  , category1 :: Word8
  , category2 :: Word8
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


samples :: Monad m => Producer B.ByteString m () -> Producer (Either String Sample) m ()
samples = PCsv.decodeByName


extract :: Pipe (Either String Sample) Sample IO ()
extract = do
  sample <- await
  case sample of
    Left s -> lift $ putStrLn s
    Right s -> do
        yield s
        extract


aggregate :: Pipe Sample B.ByteString IO ()
aggregate = go Nothing 0
  where
    go Nothing _ = do
      s <- await
      go (Just s) 1
    go (Just (Sample t byte1 byte2)) idx = do
      Sample _ b1 b2 <- await
      let byte1' = byte1 .|. (shift b1 idx)
          byte2' = byte2 .|. (shift b2 idx)
          idx' = idx + 1
          s' = Sample t byte1' byte2'
      if idx' >= 8
      then
        do
          yield (BC.pack $ show s' ++ "\n")
          go Nothing 0
      else
        go (Just s') idx'


main :: IO ()
main = do
  filename <- head <$> getArgs
  withFile filename ReadMode $ \hIn ->
    withFile "out.txt" WriteMode $ \hOut ->
      runEffect (
        (samples (PB.fromHandle hIn))
        >-> extract
        >-> aggregate
        >-> PB.toHandle hOut)
