#!/usr/bin/env stack
{- stack
  script
  --resolver lts-9.12
  --package base
  --package mtl
  --package text
  --package parsec
-}

import Prelude as P
import Control.Monad
import Control.Monad.Identity
import System.Environment
import Text.Parsec
import Text.Parsec.Char
import Data.Text as T
import Data.Text.IO as TIO

parser :: ParsecT T.Text u Identity ([Char], [Char])
parser = do
           spaces
           key <- many1 alphaNum
           spaces
           char '='
           spaces
           val <- many1 alphaNum
           return (key, val)

main = do
    fileContent <- getArgs >>= TIO.readFile . P.head
    let fileLines = T.lines fileContent
        keyValuePairs = matchLines parser fileLines
    mapM_ print keyValuePairs

matchLines :: Parsec T.Text () ([Char], [Char]) -> [T.Text] -> [([Char], [Char])]
matchLines p ls = P.foldl matcher [] ls
  where
    matcher ms l = case (parse p "" l) of
                     Left err -> ms
                     Right m -> m:ms
