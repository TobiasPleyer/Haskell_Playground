#!/usr/bin/env stack
{- stack
  script
  --resolver lts-9.12
  --package base
  --package bytestring
  --package xeno
-}

import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as SC
import qualified Xeno.DOM as XML
import Text.Printf
import Data.List

main = do
  file <- S.readFile "CDPlayer.vsr"
  case (XML.parse file) of
    Left err -> print err
    Right nModelFile -> do
      let sTopstate = SC.pack "topstate"
          sModelFile = SC.pack "modelFile"
          sElements = SC.pack "elements"
          sActionsFolder = SC.pack "actionsFolder"
          sActionFunction = SC.pack "actionFunction"
          sConstantsFolder = SC.pack "constantsFolder"
          sConstant = SC.pack "constant"
          sEventsFolder = SC.pack "eventsFolder"
          sEvent = SC.pack "event"
          sInternVariablesFolder = SC.pack "internVariablesFolder"
          sInternVariable = SC.pack "internVariable"
          sExternVariablesFolder = SC.pack "externVariablesFolder"
          sExternVariable = SC.pack "externVariable"
          sRegions = SC.pack "regions"
          sRegion = SC.pack "region"
          sVertices = SC.pack "vertices"
      if (XML.name nModelFile /= sModelFile)
      then printf "Unexpected node name '%s', expected 'modelFile'\n" (SC.unpack (XML.name nModelFile))
      else do
        case find (elemType sTopstate) (XML.children nModelFile) of
          Nothing -> printf "Error! Expected to find node %s\n" (SC.unpack sTopstate)
          Just nTopState -> print "Found it"


elemType :: S.ByteString -> XML.Node -> Bool
elemType s n = (XML.name n) == s
