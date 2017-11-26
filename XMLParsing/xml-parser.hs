#!/usr/bin/env stack
{- stack
  script
  --resolver lts-9.12
  --package bytestring
  --package xeno
-}

import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as SC
import qualified Xeno.DOM as XML
import Text.Printf

main = do
  file <- S.readFile "CDPlayer.vsr"
  case (XML.parse file) of
    Left err -> print err
    Right node -> do
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
      if (XML.name node /= sModelFile)
      then printf "Unexpected node name '%s', expected 'modelFile'\n" (SC.unpack (XML.name node))
      else do
        mapM_ print $ XML.attributes node
        let children1 = XML.children node
        mapM_ (print . XML.name) children1
        let children2 = XML.children $ head children1
        mapM_ (print . XML.name) children2
