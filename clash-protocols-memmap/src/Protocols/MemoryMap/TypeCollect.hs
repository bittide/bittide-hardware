-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE RecordWildCards #-}

-- | Collect all types referenced in a 'MemoryMap' and create a type-map.
module Protocols.MemoryMap.TypeCollect where

import Clash.Prelude hiding (def)

import qualified Data.List as L

import qualified Data.Map.Strict as Map
import Protocols.MemoryMap (DeviceDefinition (..), MemoryMap (..), Register (..))
import Protocols.MemoryMap.FieldType (FieldType (..), TypeName)

data TypeDescription = TypeDescription
  { name :: TypeName
  , nGenerics :: Int
  , definition :: FieldType
  }
  deriving (Show)

-- | Collect all referenced types in a 'MemoryMap' into a type map.
collect :: MemoryMap -> Map.Map TypeName TypeDescription
collect mm = typeMap $ collectTypeDefsFromMM mm

typeMap :: [(TypeName, TypeDescription)] -> Map.Map TypeName TypeDescription
typeMap = L.foldl go Map.empty
 where
  go m (name, def) = Map.insert name def m

collectTypeDefsFromMM :: MemoryMap -> [(TypeName, TypeDescription)]
collectTypeDefsFromMM MemoryMap{..} = go $ snd <$> Map.toList deviceDefs
 where
  go [] = []
  go (deviceDef : devs) = goRegisters (registers deviceDef) <> go devs

  goRegisters [] = []
  goRegisters ((_, _, Register{..}) : regs) = collectDefs fieldType <> goRegisters regs

collectDefs :: FieldType -> [(TypeName, TypeDescription)]
collectDefs fieldType = case fieldType of
  sop@(SumOfProductFieldType name variants) ->
    let
      def = TypeDescription name 0 sop
      inner =
        L.concat $ L.concatMap (\(_name, fields) -> L.map collectDefs $ snd <$> fields) variants
     in
      (name, def) : inner
  TypeReference inner@(SumOfProductFieldType tyName variants) args' ->
    let
      def = TypeDescription tyName (L.length args') inner
      variantInner =
        L.concat $ L.concatMap (\(_name, fields) -> L.map collectDefs $ snd <$> fields) variants
     in
      (tyName, def) : variantInner
  TypeReference ty args' -> collectDefs ty <> L.concatMap collectDefs args'
  _ -> []
