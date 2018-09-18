module Main where

import Conduit
import Data.Conduit


main :: IO ()
main = runConduit $ (yieldMany [1,2]) .| printC
