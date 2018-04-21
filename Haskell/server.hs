#!/usr/bin/env stack
{- stack
  script --resolver lts-9.12
  --package base
  --package containers
  --package transformers
-}
{-# LANGUAGE PatternGuards #-}

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.State.Strict
import qualified Data.Map.Strict as M
import Data.Maybe


type Body = String
type Pattern = String
data Request = Request { requestType :: Pattern, requestBody :: Body }
type Handler = (Pattern -> Maybe Action)
data ServerData = SInt Int
                | SString String
type Environment = M.Map String ServerData
type Action = (Environment -> Body -> IO ())
data ServerState = ServerState {
                      serverHandlers :: [Handler]
                    , serverEnvironment :: Environment
                    }
type Server = StateT ServerState IO

instance Show ServerData where
  show (SInt i) = show i
  show (SString s) = s

newServerState :: ServerState
newServerState = ServerState [] M.empty


addHandler :: Pattern -> Action -> Server ()
addHandler pat act = do
  ServerState hs env <- get
  let h = \p -> if p == pat then Just act else Nothing
  put $ ServerState (h:hs) env


setStringData :: String -> String -> Server ()
setStringData k v = do
  s@(ServerState _ env) <- get
  let v'   = SString v
      env' = M.insert k v' env
  put $ s{ serverEnvironment=env' }


setIntData :: String -> Int -> Server ()
setIntData k v = do
  s@(ServerState _ env) <- get
  let v'   = SInt v
      env' = M.insert k v' env
  put $ s{ serverEnvironment=env' }


handleRequest :: Request -> Server Action
handleRequest request = do
  ServerState registeredHandlers _ <- get
  let action = getAction registeredHandlers request
  return action
    where
      getAction = foldr tryHandler defaultHandler
      tryHandler handler cont req
        | Just a <- handler (requestType req) = a
        | otherwise = cont req
      defaultHandler = \_ _ _ ->  putStrLn "Error - No handler found"


serverLoop :: Server ()
serverLoop = do
  liftIO $ putStr "Request: "
  line <- liftIO getLine
  let (r:bs) = words line
      body = unwords bs
      request = Request r body
  unless (r == "quit") $ do
    action <- handleRequest request
    env <- liftM serverEnvironment get
    liftIO $ action env body
    serverLoop


serve :: Server () -> IO ()
serve srv = evalStateT (srv >> serverLoop) newServerState


getAction :: Action
getAction env body = putStrLn $ "Value: " ++ (show value) ++ "   (via GET)"
  where value = M.findWithDefault (SString "N/A") body env


postAction :: Action
postAction env body = putStrLn $ "Value: " ++ (show value) ++ "   (via POST)"
  where value = M.findWithDefault (SString "N/A") body env


fooAction :: Action
fooAction env body = putStrLn $ "Value: " ++ (show value) ++ "   (via FOO)"
  where value = M.findWithDefault (SString "N/A") body env


toyServer :: Server ()
toyServer = do
  setIntData "varX" 7
  setIntData "varY" 42
  setStringData "server_name" "Toy Server"
  addHandler "GET" getAction
  addHandler "POST" postAction
  addHandler "FOO" fooAction


main = do
  putStrLn "Starting server, enter 'quit' to stop."
  serve toyServer
