{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}


module ElfTypes
(
    ElfHeader(..)
)
where

import Foreign
import Foreign.C
import Foreign.C.Types
import Foreign.Storable


#include "ElfStructs64.h"


data ElfHeader = ElfHeader
  { e_ident     :: [CUChar]
  , e_type      :: CUShort
  , e_machine   :: CUShort
  , e_version   :: CUInt
  , e_entry     :: CULong
  , e_phoff     :: CULong
  , e_shoff     :: CULong
  , e_flags     :: CUInt
  , e_ehsize    :: CUShort
  , e_phentsize :: CUShort
  , e_phnum     :: CUShort
  , e_shentsize :: CUShort
  , e_shnum     :: CUShort
  , e_shstrndx  :: CUShort
  } deriving (Eq, Show)
