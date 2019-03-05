module Main where

{- This example is a modification of the todo program
 - written by Gabriel Gonzalez in his
 - `Basic Haskell Examples` blog post
 - http://www.haskellforall.com/2015/10/basic-haskell-examples.html
 -}


import Control.Monad (when)
import Control.Monad.IO.Class
import Control.Monad.Trans.State.Lazy


-- Globals


helpLines =
  [ "Commands:"
  , "+ <String> - Add a TODO entry"
  , "- <Int>    - Delete the numbered entry"
  , "q          - Quit"
  , "h          - This help message"
  ]


-- Types


newtype TodoState = TS { getTodos :: [String] } deriving (Show)
type TodoMonad = StateT TodoState IO
type Cmd = String
type Msg = String


-- User Interface


main :: IO ()
main = do
    let initialState = TS []
    putStrLn $ unlines helpLines
    evalStateT prompt initialState

printTodo :: (Int, String) -> IO ()
printTodo (n, todo) = putStrLn (show n ++ ": " ++ todo)

printTodos :: TodoMonad ()
printTodos = mapM_ (liftIO . printTodo) =<< (zip [0..] . getTodos) <$> get

prompt :: TodoMonad ()
prompt = do
    liftIO $ putStrLn "\nCurrent TODO list:"
    printTodos
    (msgs, state', continue) <- interpret <$> liftIO getLine <*> get
    put state'
    mapM_ (liftIO . putStrLn) msgs
    when continue prompt


-- Pure Code


interpret :: Cmd -> TodoState -> ([Msg], TodoState, Bool)
interpret ('+':' ':todo) state@(TS todos) = ([], TS (todo:todos), True)
interpret ('-':' ':num ) state@(TS todos) =
    case delete (read num) todos of
        Nothing     -> (["No TODO entry matches the given number"], state, True)
        Just todos' -> ([], TS todos', True)
interpret  "q"           state = ([], state, False)
interpret  "h"           state = ("":helpLines, state, True)
interpret  command       state = (["Invalid command: `" ++ command ++ "`"], state, True)

delete :: Int -> [a] -> Maybe [a]
delete 0 (_:as) = Just as
delete n (a:as) = do
    as' <- delete (n - 1) as
    return (a:as')
delete _  []    = Nothing

