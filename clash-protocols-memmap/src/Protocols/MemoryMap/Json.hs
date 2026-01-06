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
  Maybe (Just, Nothing),
  Num ((+)),
  Show (show),
  String,
  Traversable (mapM),
  error,
  maybe,
  ($),
  (<$>),
 )
import Control.Monad (forM)
import Control.Monad.RWS
import Data.Aeson
import qualified Data.Aeson.Encode.Pretty as Ae
import Data.Aeson.Key (fromString)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Text as Aeson
import qualified Data.ByteString.Lazy as BS
import Data.Foldable (toList)
import qualified Data.Map.Strict as Map
import Data.Scientific (floatingOrInteger)
import qualified Data.Sequence as Seq
import qualified Data.Text.Lazy.Builder as Builder
import GHC.Stack (SrcLoc (..))
import qualified Language.Haskell.TH as TH
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
import Protocols.MemoryMap.Check.AbsAddress (AbsNormData (..), MemoryMapTreeAbsNorm)
import Protocols.MemoryMap.TypeCollect
import Protocols.MemoryMap.TypeDescription

type JsonGenerator a = RWS LocationStorage (Seq.Seq SrcLoc) Integer a

data LocationStorage = LocationInline | LocationSeparate

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
memoryMapJson :: LocationStorage -> DeviceDefinitions -> MemoryMapTreeAbsNorm -> Value
memoryMapJson locStore deviceDefs tree =
  let action = do
        devices <- forM (Map.toList deviceDefs) $ \(name, def0) -> do
          def1 <- generateDeviceDef def0
          pure $ fromString name .= def1

        types <- forM (Map.toList validTypes) $ \(name, def0) -> do
          def1 <- generateTypeDesc def0
          pure $ fromString (show name) .= def1

        tree1 <- generateTree tree

        pure
          $ object
            [ "devices" .= object devices
            , "types" .= object types
            , "tree" .= tree1
            ]

      (value, _, locs) = runRWS action locStore 0
   in case locStore of
        LocationInline -> value
        LocationSeparate ->
          case value of
            Object obj ->
              let locsVal = locToJson <$> (toList locs)
               in Object (KM.insert (fromString "src_locations") (toJSON locsVal) obj)
            _ -> error "The top level JSON value should be an object"
 where
  validTypes = collect deviceDefs

generateTypeDesc :: WithSomeTypeDescription -> JsonGenerator Value
generateTypeDesc (WithSomeTypeDescription proxy) = do
  let desc = typeDescription proxy
  def <- generateTypeDef desc.definition
  pure
    $ object
      [ "name" .= nameToVal desc.name
      , "type_args" .= (argToVal <$> desc.args)
      , "definition" .= def
      ]
 where
  argToVal (TadNat name) = object ["name" .= TH.nameBase name, "kind" .= ("number" :: String)]
  argToVal (TadType name) = object ["name" .= TH.nameBase name, "kind" .= ("type" :: String)]

nameToVal :: TH.Name -> Value
nameToVal name =
  object
    [ "name_base" .= TH.nameBase name
    , "name_module" .= case TH.nameModule name of
        Just mod -> toJSON mod
        Nothing -> Null
    , "name_package" .= case TH.namePackage name of
        Just pkg -> toJSON pkg
        Nothing -> Null
    ]

generateTypeDef :: TypeDefinition -> JsonGenerator Value
generateTypeDef (Builtin b) =
  pure
    $ object
      [ "builtin" .= case b of
          BitVector -> "bitvector" :: String
          Vector -> "vector"
          Protocols.MemoryMap.TypeDescription.Bool -> "bool"
          Float -> "float"
          Double -> "double"
          Signed -> "signed"
          Unsigned -> "unsigned"
          Index -> "index"
      ]
generateTypeDef (DataDef cons) = do
  cons1 <- mapM generateConstructor cons
  pure
    $ object
      ["datatype" .= cons1]
generateTypeDef (NewtypeDef con) = do
  con1 <- generateConstructor con
  pure
    $ object
      ["newtype" .= con1]
generateTypeDef (Synonym ty) = do
  ty1 <- generateTypeRef ty
  pure
    $ object
      ["type_synonym" .= ty1]

generateConstructor :: (TH.Name, ConstructorDescription) -> JsonGenerator Value
generateConstructor (name, Nameless fields) = do
  fields1 <- mapM generateTypeRef fields
  pure
    $ object
      [ "name"
          .= TH.nameBase name
      , "nameless" .= fields1
      ]
generateConstructor (name, Record fields) = do
  fields1 <- forM fields $ \(fieldName, field) -> do
    ty <- generateTypeRef field
    pure
      $ object
        [ "fieldname" .= (show fieldName)
        , "type" .= ty
        ]

  pure
    $ object
      [ "name" .= TH.nameBase name
      , "record" .= fields1
      ]

generateTypeRef :: TypeRef -> JsonGenerator Value
generateTypeRef (TypeInst name args) = do
  args1 <- mapM generateTypeRef args
  pure
    $ object
      [ "type_reference" .= nameToVal name
      , "args" .= args1
      ]
generateTypeRef (Variable name) = do
  pure $ object ["variable" .= TH.nameBase name]
generateTypeRef (TypeNat lit) = do
  pure $ object ["nat" .= lit]
generateTypeRef (TupleType _ args) = do
  args1 <- mapM generateTypeRef args
  pure $ object ["tuple" .= args1]

generateTree :: MemoryMapTreeAbsNorm -> JsonGenerator Value
generateTree (AnnInterconnect absData srcLoc comps) = do
  comps' <- mapM generateComp comps
  loc <- location srcLoc
  tags' <- genTags absData.tags
  path1 <- pathVal absData.path
  pure
    $ object
      [ "interconnect"
          .= object
            [ "path" .= path1
            , "tags" .= tags'
            , "src_location" .= loc
            , "absolute_address" .= absData.absoluteAddr
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
generateTree (AnnDeviceInstance absData srcLoc deviceName) = do
  loc <- location srcLoc
  tags' <- genTags absData.tags
  path1 <- pathVal absData.path
  pure
    $ object
      [ "device_instance"
          .= object
            [ "path" .= path1
            , "tags" .= tags'
            , "device_name" .= deviceName
            , "src_location" .= loc
            , "absolute_address" .= absData.absoluteAddr
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
    ty <- generateTypeRef (regFieldType reg.fieldType)
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
        , "type" .= ty
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
  store <- ask
  case store of
    LocationInline -> do
      pure $ locToJson loc
    LocationSeparate -> do
      n <- get
      modify (+ 1)
      tell (Seq.singleton loc)
      pure $ toJSON n

locToJson :: SrcLoc -> Value
locToJson SrcLoc{..} =
  object
    [ "package" .= srcLocPackage
    , "module" .= srcLocModule
    , "file" .= srcLocFile
    , "start_line" .= srcLocStartLine
    , "start_col" .= srcLocStartCol
    , "end_line" .= srcLocEndLine
    , "end_col" .= srcLocEndCol
    ]

genTags :: [(SrcLoc, String)] -> JsonGenerator Value
genTags tags = do
  tags1 <- forM tags $ \(loc0, tag) -> do
    loc1 <- location loc0
    pure $ object ["tag" .= tag, "src_location" .= loc1]
  pure $ toJSON tags1
