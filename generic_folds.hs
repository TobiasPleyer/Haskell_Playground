#!/usr/bin/env stack
{- stack
  script
  --resolver lts-9.12
  --package base
  --package data-fix
-}

import Data.Fix

data L a b = Nil | Cons a b

type List a = Fix (L a)

instance Functor (L a) where
   fmap f x = case x of
       Nil      -> Nil
       Cons a b -> Cons a (f b)

fix_length :: List a -> Int
fix_length = cata $ \x -> case x of
   Nil      -> 0
   Cons _ n -> n + 1

fix_sum :: Num a => List a -> a
fix_sum = cata $ \x -> case x of
   Nil      -> 0
   Cons a s -> a + s

fix_sum_base :: Num a => a -> List a -> a
fix_sum_base x xs = res x
  where
    res = c xs
    c = cata $ \x -> case x of
           Nil      -> (+ 0)
           Cons a s -> (+ (s a))

psi m n
  | n > m = Nil
  | otherwise = Cons n (n+1)

phi x = case x of
   Nil      -> 0
   Cons a s -> a + s

sum_from_to start end = (hylo phi (psi end)) start

main = do
  let list = Fix (Cons 1 (Fix (Cons 2 (Fix (Cons 3 (Fix (Cons 4 (Fix Nil))))))))
  let s = fix_sum list
  print s
  let s2 = fix_sum_base 5 list
  print s2
  print $ sum_from_to 8 10
