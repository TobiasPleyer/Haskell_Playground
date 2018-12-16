#!/usr/bin/env stack
{- stack
  script
  --resolver lts-11.8
  --package base
-}

import Data.Foldable (traverse_)

showValue :: Int -> String
showValue i
    | i `mod` 15 == 0 = "FizzBuzz"
    | i `mod`  3 == 0 = "Fizz"
    | i `mod`  5 == 0 = "Buzz"
    | otherwise       = show i

getNum :: String -> IO Int
getNum s = do
    putStr s
    read <$> getLine

main = do
    from <- getNum "From: "
    to <- getNum "To: "
    putStrLn $ "\nStarting FizzBuzz [" ++ show from ++ ".." ++ show to ++ "]"
    traverse_ (putStrLn . showValue) [from..to]
