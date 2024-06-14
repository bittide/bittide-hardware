{-# LANGUAGE RecordWildCards #-}
module Protocols.MemoryMap.TypeCollect where

import Clash.Prelude hiding (def)

import qualified Data.List as L

import Protocols.MemoryMap (MemoryMap (..), DeviceDefinition(..), Register(..))
import Protocols.MemoryMap.FieldType (FieldType(..), TypeName)
import qualified Data.Map.Strict as Map

data TypeDescription = TypeDescription
  { name :: TypeName
  , nGenerics :: Int
  , definition :: FieldType
  }
  deriving (Show)

typeMap :: [(TypeName, TypeDescription)] -> Map.Map TypeName TypeDescription
typeMap = L.foldl go Map.empty
  where
    go m (name, def) = Map.insert name def m

collect :: MemoryMap -> Map.Map TypeName TypeDescription
collect mm = typeMap $ collectTypeDefsFromMM mm

collectTypeDefsFromMM :: MemoryMap -> [(TypeName, TypeDescription)]
collectTypeDefsFromMM MemoryMap{..} = go $ snd <$> Map.toList deviceDefs
  where
    go [] = []
    go (deviceDef:devs) = goRegisters (registers deviceDef) <> go devs

    goRegisters [] = []
    goRegisters ((_, _, Register{..}):regs) = collectDefs fieldType <> goRegisters regs

collectDefs :: FieldType -> [(TypeName, TypeDescription)]
collectDefs fieldType = case fieldType of
    sop@(SumOfProductFieldType name variants) ->
      let
        def = TypeDescription name 0 sop
        inner = L.concat $ L.concatMap (\(_name, fields) -> L.map collectDefs $ snd <$> fields) variants
      in (name, def) : inner
    TypeReference inner@(SumOfProductFieldType tyName variants) args' ->
      let
        def = TypeDescription tyName (L.length args') inner
        variantInner = L.concat $ L.concatMap (\(_name, fields) -> L.map collectDefs $ snd <$> fields) variants
      in (tyName, def) : variantInner
    TypeReference ty args' -> collectDefs ty <> L.concatMap collectDefs args'
    _ -> []