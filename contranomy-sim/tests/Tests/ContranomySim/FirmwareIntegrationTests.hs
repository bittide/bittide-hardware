-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Tests.ContranomySim.FirmwareIntegrationTests ( generateTests ) where

import           Clash.Prelude         hiding (map)
import           Prelude

import qualified Data.ByteString       as BS
import qualified Data.List             as L
import qualified Data.IntMap.Strict    as I
import           System.IO.Temp        (withSystemTempFile)
import           Test.HUnit.Base       (Assertion, (@?=))

import           Contranomy
import           ContranomySim.Print
import           ContranomySim.ReadElf
import           System.Directory      (copyFile, listDirectory)
import           System.FilePath       (dropExtension, takeBaseName,
                                        takeExtension, (</>))
import           Test.Tasty
import           Test.Tasty.HUnit      (testCase)
import           Paths_contranomy_sim
import           ContranomySim.DeviceTreeCompiler
import           ContranomySim.MemoryMapConsts
import           System.Exit (exitFailure)

-- | Load an elf binary, inspect the debug output
elfExpect :: (FilePath -> IO ()) -- ^ Action to place the @.elf@ file in the given 'FilePath'
          -> Int -- ^ Cycles to sample before giving up
          -> BS.ByteString -- ^ First bytes of expected output
          -> Assertion
elfExpect act n expected = do
  -- add device tree as a memory mapped component
  deviceTreePath <- getDataFileName "devicetree/contranomy-sim.dts"

  compileRes <- compileDeviceTreeSource deviceTreePath
  deviceTreeRaw <- maybe exitFailure pure compileRes

  -- add padding to prevent uninitialised accesses
  let padding = L.replicate (4 - (BS.length deviceTreeRaw `mod` 4)) 0
      deviceTree = fmap pack . BS.unpack $ deviceTreeRaw <> BS.pack padding
      deviceTreeMap = I.fromAscList (L.zip [fdtAddr ..] deviceTree)

  withSystemTempFile "ELF" $ \fp _ -> do
    act fp
    elfBytes <- BS.readFile fp
    let (entry, iMem, dMem) = readElfFromMemory elfBytes

    let dMem' = dMem `I.union` deviceTreeMap

    -- Hook up to println-debugging
    let res = getDataBytes (BS.length expected) characterDeviceAddr $ sampleN n $ fmap snd $
              contranomy' hasClock hasReset entry iMem dMem' $ pure (False, False, 0b0)

    res @?= expected


findIntegrationTests :: [FilePath] -> [(String, FilePath, FilePath)]
findIntegrationTests =
  map (\p -> (takeBaseName p, dropExtension p, p))
  . filter (\p -> takeExtension p == ".expected")


runTest :: String -- ^ Name of the test
        -> FilePath -- ^ Path to the ELF file
        -> FilePath -- ^ Path to the file containing the expected output
        -> TestTree
runTest name elfPath expectedPath =
  testCase ("Integration test `" <> name <> "`") $ do
    expected <- BS.readFile expectedPath
    let act path = copyFile elfPath path

    -- arbitrarily pick 1 million as an upper bound for tests
    elfExpect act 1000000 expected




generateTests :: FilePath -> IO TestTree
generateTests path = do
  files <- listDirectory path

  let integs = findIntegrationTests files

      tests = flip map integs $ \(name, elfPath, expectPath) ->
          runTest name (path </> elfPath) (path </> expectPath)

  pure $ testGroup "Firmware integration tests" tests
