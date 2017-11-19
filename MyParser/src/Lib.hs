module Lib
    ( apply
    , parse
    , Parser(..)
    , getc
    , guard
    , sat
    , char
    , text
    , lower
    , lowers
    , digit
    , manyP
    , someP
    , space
    , none
    , optional
    ) where

import Data.Char
import Control.Applicative
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

someFunc :: IO ()
someFunc = putStrLn "someFunc"

newtype Parser a = Parser (T.Text -> [(a, T.Text)])

apply :: Parser a -> T.Text -> [(a, T.Text)]
apply (Parser p) t = p t

parse :: Parser a -> T.Text -> a
parse p = fst . head . apply p

instance Functor Parser where
  fmap f p = Parser (\t0 -> do
                           -- In the List Monad here
                           (a, t1) <- apply p t0
                           return (f a, t1)
                    )

instance Applicative Parser where
  pure a = Parser (\t -> [(a, t)])
  q <*> p = Parser (\t0 -> do
                           -- In the List Monad here
                          (a, t1) <- apply p t0
                          (f, t2) <- apply q t1
                          return (f a, t2)
                   )

instance Monad Parser where
  return = pure
  p >>= k = Parser (\t0 -> do
                           -- In the List Monad here
                           (a, t1) <- apply p t0
                           (b, t2) <- apply (k a) t1
                           return (b, t2)
                   )

instance Alternative Parser where
  empty = Parser (\t -> [])
  p1 <|> p2 = Parser (\t0 ->
                        case (apply p1 t0) of
                          [] -> apply p2 t0
                          xs -> xs)

guard :: Bool -> Parser ()
guard True = return ()
guard False = empty

getc :: Parser Char
getc = Parser (\t0 ->
  case (T.uncons t0) of
    Nothing -> []
    Just (c, t1) -> [(c, t1)])

sat :: (Char -> Bool) -> Parser Char
sat p = do
        c <- getc
        if p c then return c
        else empty

char :: Char -> Parser ()
char x = getc >>= \c -> guard (c==x)

text :: T.Text -> Parser ()
text t =
  case T.uncons t of
    Nothing -> return ()
    Just (c, rest) -> do
                      char c
                      text rest

lower :: Parser Char
lower = sat isLower

lowers :: Parser T.Text
lowers = do {c <- lower; cs <- lowers; return $ T.cons c cs}
         <|> return T.empty

digit :: Parser Int
digit = do
        d <- sat isDigit
        return (read [d] :: Int)

none :: Parser [a]
none = return []

manyP :: Parser a -> Parser [a]
manyP p = do {a <- p; as <- manyP p; return (a:as)}
          <|> none

someP :: Parser a -> Parser [a]
someP p = do {a <- p; as <- manyP p; return (a:as)}

space :: Parser ()
space = manyP (sat isSpace) >> return ()

optional :: Parser [a] -> Parser [a]
optional p = p <|> none
