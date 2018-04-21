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


type Args = [String]
type Command = String

data ServerData = SInt Int
                | SString String

instance Show ServerData where
  show (SInt i) = show i
  show (SString s) = s

isString :: ServerData -> Bool
isString (SString _) = True
isString _           = False

isInt :: ServerData -> Bool
isInt (SInt _) = True
isInt _        = False

type Environment = M.Map String ServerData

data ServerState = ServerState {
                      serverHandlers :: [Handler]
                    , serverEnvironment :: Environment
                    }

type Server = StateT ServerState IO
type Action = (Args -> Server ())
type Handler = (Command -> Maybe Action)


newServerState :: ServerState
newServerState = ServerState [] M.empty


addHandler :: Command -> Action -> Server ()
addHandler cmd act = do
  ServerState hs env <- get
  let h = \c -> if c == cmd then Just act else Nothing
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


actionFromCommand :: Command -> Server Action
actionFromCommand cmd = do
  ServerState registeredHandlers _ <- get
  let
    action = getAction registeredHandlers cmd
  return action
    where
      getAction = foldr tryAction defaultAction
      tryAction handler cont cmd
        | Just a <- handler cmd = a
        | otherwise = cont cmd
      defaultAction = \_ _ ->  liftIO $ putStrLn "Unknown command"


serverLoop :: Server ()
serverLoop = do
  liftIO $ putStr "Request: "
  line <- liftIO getLine
  let (cmd:args) = words line
  unless (cmd == "quit") $ do
    action <- actionFromCommand cmd
    action args
    serverLoop


serve :: Server () -> IO ()
serve srv = evalStateT (srv >> serverLoop) newServerState


envAction :: Action
envAction _ = do
  ServerState _ env <- get
  let
    assocs = M.assocs env
    strings = filter (isString . snd) assocs
    ints = filter (isInt . snd) assocs
  liftIO (do
      putStrLn "Strings"
      forM_ strings (\(k,v) -> putStrLn ("  " ++ show k ++ ": " ++ show v))
      putStrLn "Integers"
      forM_ ints (\(k,v) -> putStrLn ("  " ++ show k ++ ": " ++ show v)))


getAction :: Action
getAction args = do
  env <- liftM serverEnvironment get
  let value = M.findWithDefault (SString "N/A") (head args) env
  liftIO $ putStrLn $ "Value: " ++ (show value)


putIntAction :: Action
putIntAction args = do
  let
    key = args !! 0
    value = read (args !! 1) :: Int
  setIntData key value


putStringAction :: Action
putStringAction args = do
  let
    key = args !! 0
    value = args !! 1
  setStringData key value


fooAction :: Action
fooAction _ = liftIO $ putStrLn $ "He who asks for FOO will get a BAR!"


toyServer :: Server ()
toyServer = do
  setIntData "varX" 7
  setIntData "varY" 42
  setStringData "server_name" "Toy Server"
  addHandler "ENV" envAction
  addHandler "GET" getAction
  addHandler "PUTI" putIntAction
  addHandler "PUTS" putStringAction
  addHandler "FOO" fooAction


main = do
  putStrLn "Starting server, enter 'quit' to stop."
  serve toyServer
