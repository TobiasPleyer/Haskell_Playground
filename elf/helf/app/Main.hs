module Main where

import Text.Printf
import System.Environment
import System.IO.MMap
import Foreign.Ptr (castPtr)
import Foreign.Storable (peek)
import ElfTypes

main :: IO ()
main = do
  filename <- head <$> getArgs
  (ptr, rawsize, offset, size) <- mmapFilePtr filename ReadOnly Nothing
  header <- peek (castPtr ptr) :: IO ElfHeader2
  print header
  printf "The entry address of %s is %lx\n" filename (h_entry2 header)
