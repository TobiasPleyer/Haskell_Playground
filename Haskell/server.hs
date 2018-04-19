#!/usr/bin/env stack
{- stack
  script --resolver lts-9.12
  --package base
  --package transformers
-}
{-# LANGUAGE PatternGuards #-}

import Control.Monad
import Control.Monad.Trans.State.Strict
import Data.Maybe


data Request = Request { requestType :: String, requestBody :: String }
type Handler = (Request -> Maybe String)
newtype ServerState = ServerState [Handler]
type Server = State ServerState ()


addHandler :: Handler -> Server
addHandler h = do
  ServerState hs <- get
  put $ ServerState (h:hs)


serve :: Server -> Request -> String
serve srv = handle
  where
    handle = foldr tryHandler (const "Error - No handler found") handlers
    tryHandler handler cont request
      | Just s <- handler request = s
      | otherwise = cont request
    ServerState handlers = execState srv (ServerState [])


getHandler :: Handler
getHandler (Request t b) =
  if t == "GET"
  then Just b
  else Nothing


postHandler :: Handler
postHandler (Request t b) =
  if t == "POST"
  then Just b
  else Nothing


fooHandler :: Handler
fooHandler (Request t b) =
  if t == "FOO"
  then Just b
  else Nothing


server :: Server
server = do
  addHandler getHandler
  addHandler postHandler
  addHandler fooHandler


main = do
  putStrLn "Starting server, enter 'quit' to stop."
  loop
  where
    loop = do
      putStr "Request: "
      request <- getLine
      unless (request == "quit") $ do
        putStrLn $ serve server (Request request "body")
        loop
