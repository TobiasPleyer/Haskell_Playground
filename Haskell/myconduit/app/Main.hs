module Main where

import Conduit

main :: IO ()
main = runConduit $ yieldMany [1,2] .| mapC (+1) .| printC
