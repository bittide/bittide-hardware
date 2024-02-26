-- SPDX-FileCopyrightText: 2023-2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE OverloadedStrings #-}

-- | Post processing of ILA data for 'boardTestExtended', serves as an example
-- of post processing.
module Bittide.Instances.Hitl.Post.BoardTestExtended (postBoardTestExtended) where

import Prelude

import Data.Csv
import System.Exit (ExitCode(..))
import Test.Tasty.HUnit

import Bittide.Instances.Hitl.Post.PostProcess

import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V


data Row = Row
  { sampleInBuffer :: Int
  , sampleInWindow :: Int
  , trigger :: Bool
  , capture :: Bool
  , testSelect :: Bool
  , testDone :: Bool
  , testSuccess :: Bool
  } deriving Show

instance FromNamedRecord Row where
  parseNamedRecord m = Row <$>
    m .: "Sample in Buffer" <*>
    m .: "Sample in Window" <*>
    (toEnum <$> m .: "trigger_AorB") <*>
    (toEnum <$> m .: "capture") <*>
    (toEnum <$> m .: "ilaTestSelect") <*>
    (toEnum <$> m .: "ilaTestDone") <*>
    (toEnum <$> m .: "ilaTestSuccess")


assertTriggerAtStart :: [Row] -> Assertion
assertTriggerAtStart rows =
  assertBool "Trigger should be True in the first sample" $ (trigger . head) rows

-- | Decode the CSV file and run all post processing checks
processCsv :: FilePath -> Assertion
processCsv csvPath_ = do
  csvData <- BL.readFile csvPath_
  case decodeByName csvData of
    Left err ->
      assertFailure $ "Failed to decode " <> csvPath_ <> ": " <> err
    Right (_header, rowVector) -> do
      let rows = V.toList rowVector
      assertTriggerAtStart rows

postBoardTestExtended :: ExitCode -> [FlattenedIlaCsvPath] -> Assertion
postBoardTestExtended _exitCode ilaCsvPaths = do
  let
    csvToProcess = filter ((== "boardTestIla") . ilaName) ilaCsvPaths
  assertBool "Expected at least 1 CSV file, but got 0" $ not (null csvToProcess)
  mapM_ (processCsv . csvPath) csvToProcess
  putStrLn $ "Successfully performed post processing of " <> show (length csvToProcess) <> " ILA CSV dumps"
