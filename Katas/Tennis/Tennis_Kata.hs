#!/usr/bin/env stack
{- stack
  script
  --resolver lts-11.8
  --package base
-}

import Data.Char
import Data.List (intersperse)
import System.Exit (exitSuccess)
import System.IO

data TennisValue = Love
                 | Fifteen
                 | Thirty
                 | Forty
                 deriving (Eq, Ord, Show)

nextValue :: TennisValue -> TennisValue
nextValue Love = Fifteen
nextValue Fifteen = Thirty
nextValue Thirty = Forty
nextValue Forty = Forty


data TennisScore = Values TennisValue TennisValue
                 | Deuce
                 | AdvP1
                 | AdvP2
                 | WinP1
                 | WinP2
                 deriving (Eq)

instance Show TennisScore where
    show (Values v1 v2) = show v1 ++ " : " ++ show v2
    show Deuce = "Deuce"
    show AdvP1 = "Advantage player 1"
    show AdvP2 = "Advantage player 2"
    show WinP1 = "Player 1 wins"
    show WinP2 = "Player 2 wins"
    showsPrec _ s = const $ show s


data Point = P1 | P2 deriving (Eq)

nextScore :: TennisScore -> Point -> TennisScore
nextScore WinP1 _  = WinP1
nextScore WinP2 _  = WinP2
nextScore Deuce P1 = AdvP1
nextScore Deuce P2 = AdvP2
nextScore AdvP1 P1 = WinP1
nextScore AdvP2 P1 = Deuce
nextScore AdvP1 P2 = Deuce
nextScore AdvP2 P2 = WinP2
nextScore (Values Forty Thirty) P2 = Deuce
nextScore (Values Thirty Forty) P1 = Deuce
nextScore (Values Forty _) P1 = WinP1
nextScore (Values _ Forty) P2 = WinP2
nextScore (Values v1 v2) p
    | p == P1 = Values (nextValue v1) v2
    | p == P2 = Values v1 (nextValue v2)


getUserInput :: [(Char,IO Point)] -> IO Point
getUserInput cmds = do
    c <- getChar
    let
        onBadInput = putStrLn ("\nError! Allowed values: " ++ (intersperse ',' (map fst cmds))) >> (getUserInput cmds)
        testChar (c',cmd) next = if (c == c') then cmd else next
        action = foldr testChar onBadInput cmds
    action

main = do
  hSetBuffering stdin NoBuffering
  loop input_cmds initialScore
  where
    initialScore = Values Love Love
    input_cmds = [
        ('1', return P1),
        ('2', return P2),
        ('q', exitSuccess)]
    loop cmds score = do
        putStrLn ""
        putStrLn (show score)
        point <- getUserInput cmds
        loop cmds (nextScore score point)
