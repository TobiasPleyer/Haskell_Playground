{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE RecordWildCards          #-}


module ElfTypes
(
    ElfHeader(..)
)
where

import Foreign
import Foreign.C
import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Array


#include "ElfStructs64.h"


data ElfHeader = ElfHeader
  { h_ident     :: [CUChar]
  , h_type      :: CUShort
  , h_machine   :: CUShort
  , h_version   :: CUInt
  , h_entry     :: CULong
  , h_phoff     :: CULong
  , h_shoff     :: CULong
  , h_flags     :: CUInt
  , h_ehsize    :: CUShort
  , h_phentsize :: CUShort
  , h_phnum     :: CUShort
  , h_shentsize :: CUShort
  , h_shnum     :: CUShort
  , h_shstrndx  :: CUShort
  } deriving (Eq, Show)


instance Storable ElfHeader where
  alignment _ = #{alignment ElfHeader_t}
  sizeOf _    = #{size      ElfHeader_t}
  peek p =
    ElfHeader <$> (peekArray 16 (castPtr p) :: IO [CUChar])
              <*> #{peek ElfHeader_t, e_type} p
              <*> #{peek ElfHeader_t, e_machine} p
              <*> #{peek ElfHeader_t, e_version} p
              <*> #{peek ElfHeader_t, e_entry} p
              <*> #{peek ElfHeader_t, e_phoff} p
              <*> #{peek ElfHeader_t, e_shoff} p
              <*> #{peek ElfHeader_t, e_flags} p
              <*> #{peek ElfHeader_t, e_ehsize} p
              <*> #{peek ElfHeader_t, e_phentsize} p
              <*> #{peek ElfHeader_t, e_phnum} p
              <*> #{peek ElfHeader_t, e_shentsize} p
              <*> #{peek ElfHeader_t, e_shnum} p
              <*> #{peek ElfHeader_t, e_shstrndx} p
  poke p ElfHeader{..} = do
    (pokeArray (castPtr p) h_ident)
    #{poke ElfHeader_t, e_type} p h_type
    #{poke ElfHeader_t, e_machine} p h_machine
    #{poke ElfHeader_t, e_version} p h_version
    #{poke ElfHeader_t, e_entry} p h_entry
    #{poke ElfHeader_t, e_phoff} p h_phoff
    #{poke ElfHeader_t, e_shoff} p h_shoff
    #{poke ElfHeader_t, e_flags} p h_flags
    #{poke ElfHeader_t, e_ehsize} p h_ehsize
    #{poke ElfHeader_t, e_phentsize} p h_phentsize
    #{poke ElfHeader_t, e_phnum} p h_phnum
    #{poke ElfHeader_t, e_shentsize} p h_shentsize
    #{poke ElfHeader_t, e_shnum} p h_shnum
    #{poke ElfHeader_t, e_shstrndx} p h_shstrndx


data ElfHeader2 = ElfHeader2
  { h_ident2     :: String
  , h_type2      :: Int
  , h_machine2   :: Int
  , h_version2   :: Int
  , h_entry2     :: Int
  , h_phoff2     :: Int
  , h_shoff2     :: Int
  , h_flags2     :: Int
  , h_ehsize2    :: Int
  , h_phentsize2 :: Int
  , h_phnum2     :: Int
  , h_shentsize2 :: Int
  , h_shnum2     :: Int
  , h_shstrndx2  :: Int
  } deriving (Eq, Show)


instance Storable ElfHeader2 where
  alignment _ = #{alignment ElfHeader_t}
  sizeOf _    = #{size      ElfHeader_t}
  peek p =
    ElfHeader2 <$> (peekCString (castPtr p))
               <*> #{peek ElfHeader_t, e_type} p
               <*> #{peek ElfHeader_t, e_machine} p
               <*> #{peek ElfHeader_t, e_version} p
               <*> #{peek ElfHeader_t, e_entry} p
               <*> #{peek ElfHeader_t, e_phoff} p
               <*> #{peek ElfHeader_t, e_shoff} p
               <*> #{peek ElfHeader_t, e_flags} p
               <*> #{peek ElfHeader_t, e_ehsize} p
               <*> #{peek ElfHeader_t, e_phentsize} p
               <*> #{peek ElfHeader_t, e_phnum} p
               <*> #{peek ElfHeader_t, e_shentsize} p
               <*> #{peek ElfHeader_t, e_shnum} p
               <*> #{peek ElfHeader_t, e_shstrndx} p
  poke p ElfHeader2{..} = do
    c_ident_ptr <- newCString h_ident2
    c_ident_arr <- peekArray 16 c_ident_ptr
    pokeArray (castPtr p) c_ident_arr
    #{poke ElfHeader_t, e_type} p h_type2
    #{poke ElfHeader_t, e_machine} p h_machine2
    #{poke ElfHeader_t, e_version} p h_version2
    #{poke ElfHeader_t, e_entry} p h_entry2
    #{poke ElfHeader_t, e_phoff} p h_phoff2
    #{poke ElfHeader_t, e_shoff} p h_shoff2
    #{poke ElfHeader_t, e_flags} p h_flags2
    #{poke ElfHeader_t, e_ehsize} p h_ehsize2
    #{poke ElfHeader_t, e_phentsize} p h_phentsize2
    #{poke ElfHeader_t, e_phnum} p h_phnum2
    #{poke ElfHeader_t, e_shentsize} p h_shentsize2
    #{poke ElfHeader_t, e_shnum} p h_shnum2
    #{poke ElfHeader_t, e_shstrndx} p h_shstrndx2
