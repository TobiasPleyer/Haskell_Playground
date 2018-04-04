module Main where

import Lib
import qualified Data.Text as T

main :: IO ()
main = do
       let x = parse (text (T.pack "Hello")) (T.pack "Hello World!")
       putStrLn (show x)
