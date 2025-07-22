-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Generate a JSON representation of a 'MemoryMapValid'
module Protocols.MemoryMap.Json where

import Clash.Prelude (
  Applicative (pure),
  Double,
  Either (Left, Right),
  Integer,
  Num ((+)),
  Semigroup ((<>)),
  Show (show),
  String,
  Traversable (mapM),
  error,
  flip,
  maybe,
  ($),
  (<$>),
 )

import Protocols.MemoryMap (
  Access (ReadOnly, ReadWrite, WriteOnly),
  DeviceDefinition (..),
  DeviceDefinitions,
  MemoryMapTreeAnn (AnnDeviceInstance, AnnInterconnect),
  Name (..),
  NamedLoc (..),
  Path,
  PathComp (..),
  Register (..),
  regByteSizeC,
  regFieldType,
 )
import Protocols.MemoryMap.Check.AbsAddress (MemoryMapTreeAbsNorm)
import Protocols.MemoryMap.TypeCollect

import Control.Monad (forM)
import Control.Monad.State
import Data.Aeson
import Data.Aeson.Key (fromString)
import Data.Scientific (floatingOrInteger)
import GHC.Stack (SrcLoc (..))

import qualified Data.Aeson.Encode.Pretty as Ae
import qualified Data.Aeson.Text as Aeson
import qualified Data.ByteString.Lazy as BS
import qualified Data.List as L
import qualified Data.Map.Strict as Map
import qualified Data.Text.Lazy.Builder as Builder
import qualified Protocols.MemoryMap.FieldType as FT

type JsonGenerator a = State (Integer, [SrcLoc]) a

{- | Custom number format for JSON encoding. Will not use scientific/float
notation if a number becomes \"too large\". This is currently a workaround
for the Rust side not accepting scientific notation when parsing.
-}
mmNumFormat :: Ae.NumberFormat
mmNumFormat = Ae.Custom $ \s ->
  case floatingOrInteger s of
    Left (_ :: Double) -> Aeson.encodeToTextBuilder $ Number s
    Right (i :: Integer) -> Builder.fromString (show i)

-- | Encode a 'Value' to JSON with a number formatter suitable for Rust parsing.
encode :: Value -> BS.ByteString
encode = Ae.encodePretty' Ae.defConfig{Ae.confNumFormat = mmNumFormat}

-- | Generate a JSON representation of a 'MemoryMapValid'
memoryMapJson :: DeviceDefinitions -> MemoryMapTreeAbsNorm -> Value
memoryMapJson deviceDefs tree =
  let
    action = do
      devices <- forM (Map.toList deviceDefs) $ \(name, def0) -> do
        def1 <- generateDeviceDef def0
        pure $ fromString name .= def1

      types <- forM (Map.toList validTypes) $ \(name, def0) -> do
        def1 <- generateTypeDesc def0
        pure $ fromString name.name .= def1

      tree1 <- generateTree tree

      (_, L.reverse -> locs) <- get
      let locs1 = flip L.map locs $ \SrcLoc{..} ->
            object
              [ "package" .= srcLocPackage
              , "module" .= srcLocModule
              , "file" .= srcLocFile
              , "start_line" .= srcLocStartLine
              , "start_col" .= srcLocStartCol
              , "end_line" .= srcLocEndLine
              , "end_col" .= srcLocEndCol
              ]

      pure
        $ object
          [ "devices" .= object devices
          , "types" .= object types
          , "tree" .= tree1
          , "src_locations" .= locs1
          ]

    (value, _) = runState action (0, [])
   in
    value
 where
  validTypes = collect deviceDefs

generateTypeDesc :: TypeDescription -> JsonGenerator Value
generateTypeDesc TypeDescription{name = tyName, ..} =
  pure
    $ object
      [ "name" .= toJSON tyName.name
      , "meta"
          .= object
            [ "module" .= tyName.moduleName
            , "package" .= tyName.packageName
            , "is_newtype" .= tyName.isNewType
            ]
      , "generics" .= nGenerics
      , "definition" .= generateTypeDef definition
      ]

generateTypeDef :: FT.FieldType -> Value
generateTypeDef ft = case ft of
  FT.BoolFieldType -> "bool"
  FT.FloatSingleType -> "float"
  FT.FloatDoubleType -> "double"
  FT.BitVectorFieldType n -> toJSON ["bitvector", toJSON n]
  FT.SignedFieldType n -> toJSON ["signed", toJSON n]
  FT.SumOfProductFieldType _tyName def' ->
    object
      [ "variants" .= (genVariant <$> def')
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
  FT.IndexFieldType n -> toJSON ["index", toJSON n]
  FT.VecFieldType n ty -> toJSON ["vector", toJSON n, generateTypeDef ty]
  FT.TypeReference (FT.SumOfProductFieldType tyName _def) args ->
    toJSON ["reference", toJSON tyName.name, toJSON (generateTypeDef <$> args)]
  FT.TypeReference ty [] -> generateTypeDef ty
  FT.TypeReference ty args -> error $ "shouldn't happen: " <> show ty <> show args
  FT.TypeVariable n -> toJSON ["variable", toJSON n]

generateTree :: MemoryMapTreeAbsNorm -> JsonGenerator Value
generateTree (AnnInterconnect (tags, path, absAddr) srcLoc comps) = do
  comps' <- mapM generateComp comps
  loc <- location srcLoc
  tags' <- genTags tags
  path1 <- pathVal path
  pure
    $ object
      [ "interconnect"
          .= object
            [ "path" .= path1
            , "tags" .= tags'
            , "src_location" .= loc
            , "absolute_address" .= absAddr
            , "components" .= comps'
            ]
      ]
 where
  generateComp (addr, tree) = do
    tree' <- generateTree tree
    pure
      $ object
        [ "relative_address" .= addr
        , "tree" .= tree'
        ]
generateTree (AnnDeviceInstance (tags, path, absAddr) srcLoc deviceName) = do
  loc <- location srcLoc
  tags' <- genTags tags
  path1 <- pathVal path
  pure
    $ object
      [ "device_instance"
          .= object
            [ "path" .= path1
            , "tags" .= tags'
            , "device_name" .= deviceName
            , "src_location" .= loc
            , "absolute_address" .= absAddr
            ]
      ]
generateDeviceDef :: DeviceDefinition -> JsonGenerator Value
generateDeviceDef dev = do
  regs <- mapM generateRegister dev.registers
  loc <- location dev.definitionLoc
  pure
    $ object
      [ "name" .= dev.deviceName.name
      , "description" .= dev.deviceName.description
      , "src_location" .= loc
      , "registers" .= regs
      , "tags" .= dev.tags
      ]
 where
  generateRegister :: NamedLoc Register -> JsonGenerator Value
  generateRegister namedReg = do
    let reg = namedReg.value
    loc <- location namedReg.loc
    pure
      $ object
        [ "name" .= namedReg.name.name
        , "description" .= namedReg.name.description
        , "src_location" .= loc
        , "address" .= reg.address
        , "access" .= case reg.access of
            ReadOnly -> "read_only" :: String
            WriteOnly -> "write_only"
            ReadWrite -> "read_write"
        , "type" .= generateTypeDef (regFieldType reg.fieldType)
        , "size" .= (regByteSizeC reg.fieldType :: Integer)
        , "reset" .= maybe Null toJSON reg.reset
        , "tags" .= reg.tags
        ]

pathVal :: Path -> JsonGenerator Value
pathVal comps = do
  comps1 <- forM comps pathComp
  pure $ toJSON comps1
 where
  pathComp (PathName srcLoc n) = do
    loc1 <- location srcLoc
    pure $ object ["name" .= n, "src_location" .= loc1]
  pathComp (PathUnnamed n) = pure $ toJSON n

location :: SrcLoc -> JsonGenerator Value
location loc = do
  (n, locs) <- get
  put (n + 1, loc : locs)
  pure $ toJSON n

genTags :: [(SrcLoc, String)] -> JsonGenerator Value
genTags tags = do
  tags1 <- forM tags $ \(loc0, tag) -> do
    loc1 <- location loc0
    pure $ object ["tag" .= tag, "src_location" .= loc1]
  pure $ toJSON tags1
