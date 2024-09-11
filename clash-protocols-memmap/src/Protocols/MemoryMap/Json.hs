-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Generate a JSON representation of a 'MemoryMapValid'
module Protocols.MemoryMap.Json where

import Clash.Prelude

import Data.Aeson
import Protocols.MemoryMap (
  Access (ReadOnly, ReadWrite, WriteOnly),
  DeviceDefinition (..),
  MemoryMap (..),
  MemoryMapTree (..),
  Name (..),
  Register (..),
 )
import Protocols.MemoryMap.Check (MemoryMapValid (..))
import qualified Protocols.MemoryMap.FieldType as FT
import Protocols.MemoryMap.TypeCollect

import Data.Aeson.Key (fromString)
import qualified Data.Map.Strict as Map
import GHC.Stack (SrcLoc (..))

-- | Generate a JSON representation of a 'MemoryMapValid'
memoryMapJson :: MemoryMapValid -> Value
memoryMapJson MemoryMapValid{..} =
  object
    [ "devices" .= object devicesVal
    , "types" .= object types
    , "tree" .= generateTree validTree
    ]
 where
  devicesVal =
    (\(name, def') -> fromString name .= generateDeviceDef def')
      <$> Map.toList (deviceDefs validMap)

  validTree = tree validMap
  types =
    (\(name, def') -> fromString (FT.name name) .= generateTypeDesc def')
      <$> Map.toList validTypes

generateTypeDesc :: TypeDescription -> Value
generateTypeDesc TypeDescription{name = tyName, ..} =
  object
    [ "name" .= toJSON (FT.name tyName)
    , "meta"
        .= object
          [ "module" .= FT.moduleName tyName
          , "package" .= FT.packageName tyName
          , "is_newtype" .= FT.isNewType tyName
          ]
    , "generics" .= nGenerics
    , "definition" .= generateTypeDef definition
    ]

generateTypeDef :: FT.FieldType -> Value
generateTypeDef ft = case ft of
  FT.BoolFieldType -> "bool"
  FT.BitVectorFieldType n -> toJSON ["bitvector", toJSON n]
  FT.SignedFieldType n -> toJSON ["signed", toJSON n]
  FT.SumOfProductFieldType tyName def' ->
    object
      [ "name" .= FT.name tyName
      , "meta"
          .= object
            [ "module" .= FT.moduleName tyName
            , "package" .= FT.packageName tyName
            , "is_newtype" .= FT.isNewType tyName
            ]
      , "variants" .= (genVariant <$> def')
      ]
   where
    genVariant :: FT.Named [FT.Named FT.FieldType] -> Value
    genVariant (n, fields) =
      object
        [ "name" .= n
        , "fields" .= (genField <$> fields)
        ]

    genField :: FT.Named FT.FieldType -> Value
    genField (name, field) = object ["name" .= name, "type" .= generateTypeDef field]
  FT.UnsignedFieldType n -> toJSON ["unsigned", toJSON n]
  FT.VecFieldType n ty -> toJSON ["vector", toJSON n, generateTypeDef ty]
  FT.TypeReference (FT.SumOfProductFieldType tyName _def) args ->
    toJSON ["reference", toJSON (FT.name tyName), toJSON (generateTypeDef <$> args)]
  FT.TypeReference ty [] -> generateTypeDef ty
  FT.TypeReference ty args -> error $ "shouldn't happen: " <> show ty <> show args
  FT.TypeVariable n -> toJSON ["variable", toJSON n]

generateTree :: MemoryMapTree -> Value
generateTree (Interconnect loc (Just absAddr) comps) =
  object
    [ "interconnect"
        .= object
          [ "src_location" .= location loc
          , "absolute_address" .= absAddr
          , "components" .= (generateComp <$> comps)
          ]
    ]
 where
  generateComp (addr, size, tree) =
    object
      [ "relative_address" .= addr
      , "size" .= size
      , "tree" .= generateTree tree
      ]
generateTree (DeviceInstance loc (Just absAddr) instanceName deviceName) =
  object
    [ "device_instance"
        .= object
          [ "instance_name" .= instanceName
          , "device_name" .= deviceName
          , "src_location" .= location loc
          , "absolute_address" .= absAddr
          ]
    ]
generateTree _ = error "memory map tree should have absolute addresses"

generateDeviceDef :: DeviceDefinition -> Value
generateDeviceDef DeviceDefinition{..} =
  object
    [ "name" .= Protocols.MemoryMap.name deviceName
    , "description" .= description deviceName
    , "src_location" .= location defLocation
    , "registers" .= (generateRegister <$> registers)
    ]
 where
  generateRegister (regName, srcLoc, Register{..}) =
    object
      [ "name" .= Protocols.MemoryMap.name regName
      , "description" .= description regName
      , "src_location" .= location srcLoc
      , "address" .= address
      , "access" .= case access of
          ReadOnly -> "read_only" :: String
          WriteOnly -> "write_only"
          ReadWrite -> "read_write"
      , "type" .= generateTypeDef fieldType
      , "size" .= fieldSize
      , "reset" .= maybe Null toJSON reset
      ]

location :: SrcLoc -> Value
location SrcLoc{..} =
  object
    [ "package" .= srcLocPackage
    , "module" .= srcLocModule
    , "file" .= srcLocFile
    , "start_line" .= srcLocStartLine
    , "start_col" .= srcLocStartCol
    , "end_line" .= srcLocEndLine
    , "end_col" .= srcLocEndCol
    ]
