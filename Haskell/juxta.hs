#!/usr/bin/env stack
{- stack
  script
  --resolver lts-9.12
  --package base
-}


juxtaV1 :: [(a -> b)] -> a -> [b]
juxtaV1 fs x = map ($ x) fs

juxtaV2 :: [(a -> b)] -> a -> [b]
juxtaV2 fs x = foldr (\f ys -> (f x) : ys) [] fs


main = do
  let
    fs = [(+1), (*2), (\x -> x*x)]
    ys1 = juxtaV1 fs 42
    ys2 = juxtaV2 fs 42
  putStrLn "Comparing juxtaposition implementations"
  putStrLn $ "Version 1: " ++ (show ys1)
  putStrLn $ "Version 2: " ++ (show ys2)
