-- SPDX-FileCopyrightText: 2023 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE RecordWildCards #-}

-- | Infrastructure for post processing of ILA data
module Bittide.Instances.Hitl.Post.PostProcess where

import Prelude

import Data.Maybe (fromMaybe)
import GHC.Stack (HasCallStack)
import System.FilePath (makeRelative, splitDirectories, takeBaseName)
import Text.Read (readMaybe)

import Data.Map (Map)
import qualified Data.Map as Map

type TestName = String
type IlaName = String

data FpgaNum
  = DemoRack Int
  | Any
  deriving (Eq, Ord, Show)

type NestedIlaCsvPaths = Map TestName (Map FpgaNum (Map IlaName FilePath))

data FlattenedIlaCsvPath = FlattenedIlaCsvPath
  { testName :: TestName
  , fpgaNum :: FpgaNum
  , ilaName :: IlaName
  , csvPath :: FilePath
  }

addIlaCsvPath :: NestedIlaCsvPaths -> FlattenedIlaCsvPath -> NestedIlaCsvPaths
addIlaCsvPath m FlattenedIlaCsvPath{..} = Map.alter goTestName testName m
 where
  goTestName = Just . Map.alter goFpgaNum fpgaNum . fromMaybe Map.empty
  goFpgaNum = Just . Map.insert ilaName csvPath . fromMaybe Map.empty

{- | Convert a String of the format "{idx}_{fpga_id}" to an FpgaNum. idx is
either the index of the FPGA board in the demo rack, or 'X' if it was
programmed with `Any`. fpga_id is the unique identifier of the FPGA.

toFpgaNum "X_210308B09917" == Any
toFpgaNum "7_210308B0B0C2" == DemoRack 7
-}
toFpgaNum :: (HasCallStack) => String -> FpgaNum
toFpgaNum fpgaName =
  case prefix of
    "X" -> Any
    _ ->
      case readMaybe prefix of
        Nothing -> error $ "Expected an FPGA name with prefix 'X' or a number, but got: " <> fpgaName
        Just n -> DemoRack n
 where
  prefix = takeWhile (/= '_') fpgaName

{- | Create NestedIlaCsvPaths using a list of filepaths of CSV dumps and the
base directory of ILA data.
-}
toNestedIlaCsvPaths :: (HasCallStack) => FilePath -> [FilePath] -> NestedIlaCsvPaths
toNestedIlaCsvPaths ilaDataDir = foldl addIlaCsvPath Map.empty . toFlattenedIlaCsvPathList ilaDataDir

{- | Create a list of FlattenedIlaCsvPath using a list of filepaths of CSV dumps
and the base directory of ILA data.
-}
toFlattenedIlaCsvPathList ::
  (HasCallStack) => FilePath -> [FilePath] -> [FlattenedIlaCsvPath]
toFlattenedIlaCsvPathList ilaDataDir = map go
 where
  go :: FilePath -> FlattenedIlaCsvPath
  go csvPath = FlattenedIlaCsvPath{..}
   where
    relativeCsvPath = makeRelative ilaDataDir csvPath
    (testName, toFpgaNum -> fpgaNum, takeBaseName -> ilaName) =
      case splitDirectories relativeCsvPath of
        [a, b, c] -> (a, b, c)
        zs ->
          error $
            "Execpted to split "
              <> show relativeCsvPath
              <> " in tree parts,"
              <> " but was able to split it into: "
              <> show zs

-- | Like 'Map.!', but mentions key in error message if it can't be found
get :: (HasCallStack, Ord k, Show k) => Map k v -> k -> v
get m k =
  case Map.lookup k m of
    Just v -> v
    Nothing -> error $ "Could not find key: " <> show k

infixl 9 `get`
