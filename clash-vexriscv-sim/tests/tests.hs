-- SPDX-FileCopyrightText: 2022-2023 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE NumericUnderscores #-}

import Clash.Prelude

import qualified Data.ByteString as BS
import qualified Data.List as L

import Control.Monad (forM)
import Data.Maybe (catMaybes, mapMaybe)
import Data.Word (Word8)
import GHC.Base (when)
import System.Directory (copyFile, doesFileExist, listDirectory)
import System.Exit (exitFailure)
import System.FilePath
import System.IO
import System.IO.Temp (withSystemTempFile)

import Test.Tasty
import Test.Tasty.HUnit (Assertion, testCase, (@?=))

import Utils.ProgramLoad (loadProgram)
import Utils.Cpu (cpu)


runProgramExpect ::
  -- | action to copy ELF file
  (FilePath -> IO ()) ->
  -- | number of cycles to simulate
  Int ->
  -- | expected output
  BS.ByteString ->
  Assertion
runProgramExpect act n expected = withSystemTempFile "ELF" $ \fp _ -> do
  act fp
  (iMem, dMem) <- withClockResetEnable @System clockGen (resetGenN (SNat @2)) enableGen $ loadProgram fp

  let _all@(unbundle -> (_circuit, writes, _iBus, _dBus)) =
        withClockResetEnable @System clockGen (resetGenN (SNat @2)) enableGen $
          bundle (cpu iMem dMem)

  let output = L.take (BS.length expected) $
        flip mapMaybe (sampleN_lazy n writes) $ \case
            Just (addr, value) | addr == 0x0000_1000 ->
              let (_ :: BitVector 24, b :: Word8) = unpack value
              in Just b
            _ -> Nothing

  BS.pack output @?= expected

findTests ::
  FilePath ->
  FilePath ->
  IO [(String, FilePath, FilePath)]
-- test name  bin path  expected-path
findTests srcDir binDir = do
  srcFiles <- listDirectory srcDir

  let expectFiles = L.filter (\p -> takeExtension p == ".expected") srcFiles
      binaryPaths = L.map (\p -> binDir </> takeBaseName p) expectFiles

  paths <- forM (L.zip binaryPaths expectFiles) $ \(binPath, expectPath) -> do
    exists <- doesFileExist binPath
    if exists
      then pure $ Just (takeBaseName binPath, binPath, srcDir </> expectPath)
      else do
        hPutStrLn stderr $
          "No binary file found for test program "
            <> takeBaseName binPath
            <> " (expected path "
            <> binPath
            <> ")"
        pure Nothing

  pure $ catMaybes paths

sourceDir, releaseBinDir, debugBinDir :: FilePath
sourceDir = "clash-vexriscv-sim/test-programs/src/bin"
releaseBinDir = "target/riscv32imc-unknown-none-elf/release"
debugBinDir = "target/riscv32imc-unknown-none-elf/debug"

releaseCycles, debugCycles :: Int
releaseCycles = 1_000_000 -- 1 million cycles
debugCycles = 1_000_000 -- 10 million cycles

runTest ::
  -- | name of the test
  String ->
  -- | mode of the test (debug / release)
  String ->
  -- | Cycles to simulate
  Int ->
  -- | path to the binary
  FilePath ->
  -- | path to the expected output file
  FilePath ->
  TestTree
runTest name mode n elfPath expectPath =
  testCase ("Integration test `" <> name <> "` (" <> mode <> ")") $ do
    expected <- BS.readFile expectPath
    let act = copyFile elfPath

    runProgramExpect act n expected
    pure ()

main :: IO ()
main = do
  debugTests <- findTests sourceDir debugBinDir
  releaseTests <- findTests sourceDir releaseBinDir

  when (L.null debugTests) $ do
    hPutStrLn stderr "No debug tests found! Was `cargo build` run?"
    exitFailure

  when (L.null releaseTests) $ do
    hPutStrLn stderr "No release tests found! Was `cargo build --release` run?"
    exitFailure

  let debugTestCases = flip L.map debugTests $ \(name, binPath, expectPath) ->
        runTest name "debug" debugCycles binPath expectPath

  let releaseTestCases = flip L.map releaseTests $ \(name, binPath, expectPath) ->
        runTest name "release" releaseCycles binPath expectPath

  let tests =
        testGroup
          "VexRiscv Tests"
          [ testGroup "Debug builds" debugTestCases,
            testGroup "Release builds" releaseTestCases
          ]

  defaultMain tests
