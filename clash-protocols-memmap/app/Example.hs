-- SPDX-FileCopyrightText: 2022-2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fplugin Protocols.Plugin #-}
-- {-# OPTIONS -fplugin-opt=Protocols.Plugin:debug #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NumericUnderscores #-}

import Clash.Prelude

import Protocols.MemoryMap
import Internal.HdlTest.UartMock (someCircuit)
import Control.Monad (forM_, when)
import Text.Printf (printf)

import qualified Data.Map.Strict as Map
import qualified Data.List as List
import qualified Data.Aeson as Ae
import qualified Data.ByteString.Lazy as BS
import Protocols.MemoryMap.Check (check, checkMemoryMap, MemoryMapValidationErrors(..), CheckConfiguration (..), MemoryMapValid (..))
import Protocols.MemoryMap.Check.AbsAddress (AbsAddressValidateError(..))
import Protocols.MemoryMap.Check.Overlap (OverlapError(..))
import qualified Protocols.MemoryMap.TypeCollect as Ty
import GHC.Stack (prettySrcLoc)
import Protocols.MemoryMap.TypeCollect
import Protocols.MemoryMap.FieldType (FieldType(..))
import qualified Protocols.MemoryMap.FieldType as FT
import System.Exit (exitFailure)
import Protocols.MemoryMap.Json (memoryMapJson)

checkConfig = CheckConfiguration { startAddr = 0x0000_0000, endAddr = 0xFFFF_FFFF }

checkMemoryMap @System
  (CheckConfiguration { startAddr = 0x0000_0000, endAddr = 0xFFFF_FFFF })
  someCircuit

main :: IO ()
main = do
  let
    SimOnly ann = annotation @System someCircuit
    -- ann' = makeAbsolute 0x0 ann
    -- validation = validateAbsAddresses Root ann' ann

  mmValid@MemoryMapValid{..} <- case check checkConfig ann of
      Left MemoryMapValidationErrors{..} -> do

        forM_ absAddrErrors $ \AbsAddressValidateError{..} -> do
          let path' = prettyPrintPath path
          let component = case componentName of
                Just name -> name
                Nothing -> "interconnect " <> path'
          printf "Expected component %s at %08X but found %08X\n" component expected got

        forM_ overlapErrors $ \case
          OverlapError{..} -> do
            printf "Component %s (%08X + %08X) overlaps with %s at address (%08X)"
              (prettyPrintPath path) startAddr componentSize
              (prettyPrintPath overlapsWith) overlapsAt
          SizeExceedsError{..} -> do
            printf "Component %s (%08X + %08X) exceeds available size %08X"
              (prettyPrintPath path) startAddr requestedSize
              availableSize

        exitFailure
      Right res -> pure res

  print validTypes

  putStrLn "\n"

  forM_ (Map.toList validTypes) $ \(name, def) -> do
    putStrLn $ generateRustTypeDef def

  forM_ (deviceDefs validMap) $ \def' -> do
    let rustCode = generateRustDeviceWrapper def'
    putStrLn rustCode

  let json = memoryMapJson mmValid
  BS.putStr (Ae.encode json)

  -- summary ann'

prettyPrintPath :: ComponentPath -> String
prettyPrintPath Root = "root"
prettyPrintPath (InterconnectComponent idx path) = prettyPrintPath path <> "." <> show idx

summary :: MemoryMap -> IO ()
summary MemoryMap{deviceDefs, tree} = do
  showDeviceDefs (snd <$> Map.toList deviceDefs)
  putStrLn "\nDevice instances:"
  showDeviceInstances tree

showDeviceDefs :: [DeviceDefinition] -> IO ()
showDeviceDefs defs = do
  putStrLn "Device Definitions"

  forM_ defs $ \DeviceDefinition{..} -> do
    printf "  %s: %s (%s)\n" (Protocols.MemoryMap.name deviceName) (description deviceName) (prettySrcLoc defLocation)

    forM_ registers $ \(Name{name=regName}, loc, Register{..}) -> do
      printf "    % 4X %s %s (%s)\n" address (show access) regName (prettySrcLoc loc)

showDeviceInstances :: MemoryMapTree -> IO ()
showDeviceInstances (Interconnect _ _ comps) = do
  forM_ comps $ \(_, _, comp) -> showDeviceInstances comp
showDeviceInstances (DeviceInstance loc addr instanceName typeName) = do
  let addr' = maybe "??      " (printf "%08X") addr
  printf "%s %s of type %s (%s)\n" addr' instanceName typeName (prettySrcLoc loc)



generateRustTypeDef :: TypeDescription -> String
generateRustTypeDef TypeDescription{definition = SumOfProductFieldType tyName vars, ..}
  | [] <- vars = "struct " <> typeName tyName <> generics <> ";"
  | [var] <- vars =
      let
        body = case var of
                (name, []) -> "();"
                (name, fields@(("", _):_)) -> "(" <> List.intercalate ", " fields' <> ");"
                  where
                    fields' = generateRustType . snd <$> fields
                (name, fields) -> " {\n" <> fields'' <> "\n}"
                  where
                    fields' = (\(name, ty) -> "\t" <> name <> ": " <> generateRustType ty) <$>fields
                    fields'' = List.intercalate ",\n" fields'
        generics = genericListDef nGenerics
      in "struct " <> typeName tyName <> generics <> body
  | otherwise = "enum " <> typeName tyName <> generics <> " {\n\t" <> variants' <> "\n}"
      where
        variants = generateVariant <$> vars
        variants' = List.intercalate "\n\t" variants
        generics = genericListDef nGenerics
        generateVariant (name, []) = name <> ","
        generateVariant (name, fields@(("", _):_)) = name <> "(" <> fields'' <> "),"
          where
            fields' = generateRustType . snd <$> fields
            fields'' = List.intercalate ", " fields'
        generateVariant (name, fields) = name <> " {" <> fields'' <> " }"
          where
            fields' = List.map (\(name, ty) -> name <> ": " <> generateRustType ty) fields
            fields'' = List.intercalate ", " fields'
generateRustTypeDef TypeDescription{..} = error "unimplemented"

typeName :: FT.TypeName -> String
typeName (FT.name -> "(,)") = "Pair"
typeName (FT.name -> "(,,)") = "Tuple3"
typeName (FT.name -> n) = n

generateRustType :: FieldType -> String
generateRustType ty = case ty of
  BoolFieldType -> "bool"
  BitVectorFieldType n -> "u" <> reprTypeWidth n
  SignedFieldType n -> "i" <> reprTypeWidth n
  sop@(SumOfProductFieldType name args) -> generateRustType (TypeReference sop []) -- error $ "SOP without TyRef shouldn't happen" <> show name <> show args
  UnsignedFieldType n -> "u" <> reprTypeWidth n
  VecFieldType n ty' -> "[" <> generateRustType ty' <> "; " <> show n <> "]"
  TypeReference (SumOfProductFieldType tyName _) [] ->
    typeName tyName
  TypeReference (SumOfProductFieldType tyName _) args ->
    typeName tyName <> "<" <> List.intercalate ", " args'  <> ">"
      where
        args' = generateRustType <$> args
  TypeReference ty' _args' -> generateRustType ty'
  TypeVariable n -> [genericVars List.!! fromInteger n]

genericVars :: [Char]
genericVars = ['A'..]

genericListDef :: Int -> String
genericListDef 0 = ""
genericListDef n = "<" <> varNames' <> ">"
  where
    varNames = List.take n ((:[]) <$> genericVars)
    varNames' = List.intercalate ", " varNames


generateRustDeviceWrapper :: DeviceDefinition -> String
generateRustDeviceWrapper DeviceDefinition{..} =
  "pub struct " <> typeName <> "(*mut u8);\n" <>
  "\n" <>
  "impl " <> typeName <> " {\n" <>
    "  pub const unsafe fn new(addr: *mut u8) -> Self {\n" <>
    "    Self(addr)\n" <>
    "  }\n" <>
    List.concat regGets <>
    List.concat regSets <>
  "}\n"
  where
    typeName = Protocols.MemoryMap.name deviceName

    regGets = uncurry3 regGetFunc <$> registers
    regSets = uncurry3 regSetFunc <$> registers

    uncurry3 f (a, b, c) = f a b c

    regGetFunc Name{..} _ Register{..}
      -- skip for now
      | VecFieldType _ _ <- fieldType = ""
      | WriteOnly <- access = ""
      | otherwise =
        "  pub fn " <> name <> "(&self) -> " <> generateRustType fieldType <> " {\n" <>
        "    unsafe {\n" <>
        "      *self.0.add(" <> printf "0x%X" address <>").cast()\n" <>
        "    }\n" <>
        "  }\n"

    regSetFunc Name{..} _ Register{..}
      -- skip for now
      | VecFieldType _ _ <- fieldType = ""
      | ReadOnly <- access = ""
      | otherwise =
        "  pub fn set_" <> name <> "(&mut self, value: " <>
            generateRustType fieldType <>
            ") {\n" <>
        "  }\n"


reprTypeWidth :: (Ord a, Num a) => a -> String
reprTypeWidth n
  | n <= 8 = "8"
  | n <= 16 = "16"
  | n <= 32 = "32"
  | n <= 64 = "64"
  | otherwise = "128"
