#!/usr/bin/env stack
{- stack
  script
  --resolver lts-9.12
  --package base
  --package directory
  --package unix
-}

import Control.Monad      (mapM)
import System.Directory   (listDirectory)
import System.Environment (getArgs)
import System.Posix.Files (getFileStatus, isDirectory)


listDirectoryRecursive :: Int            -- ^ recursion depth, <0 -> no limit
                       -> FilePath       -- ^ the file path we are starting at
                       -> IO [[String]]  -- ^ a list of directory paths
listDirectoryRecursive lvl fp = go lvl fp fp
  where
    go lvl fullPath currDir =
      if (lvl == 0)
      then return [[currDir]]
      else do
        fs <- getFileStatus fullPath
        if (isDirectory fs)
        then do
          ds <- listDirectory fullPath
          dss <- mapM (\d -> go (lvl-1) (fullPath ++ "/" ++ d) d) ds
          return (map (currDir:) (concat dss))
        else return [[currDir]]

main = do
  args <- getArgs
  let (depth,path) = case args of
                        [p]     -> (-1, p)
                        (p:d:_) -> (read d, p)
                        _       -> (-1,".")
  ds <- listDirectoryRecursive depth path
  print ds
