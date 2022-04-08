module Tests.Contranomy.FirmwareIntegrationTests ( generateTests ) where

import           Clash.Prelude      hiding (map)
import           Prelude

import qualified Data.ByteString    as BS
import           System.IO.Temp     (withSystemTempFile)
import           Test.HUnit.Base    (Assertion, (@?=))

import           Contranomy
import           Contranomy.Println
import           ReadElf
import           System.Directory   (copyFile, listDirectory)
import           System.FilePath    (dropExtension, takeBaseName, takeExtension,
                                     (</>))
import           Test.Tasty
import           Test.Tasty.HUnit   (testCase)

-- | Load an elf binary, inspect the debug output
elfExpect :: (FilePath -> IO ()) -- ^ Action to place the @.elf@ file in the given 'FilePath'
          -> Int -- ^ Cycles to sample before giving up
          -> BS.ByteString -- ^ First bytes of expected output
          -> Assertion
elfExpect act n expected = do
  withSystemTempFile "ELF" $ \fp _ -> do
    act fp
    elfBytes <- BS.readFile fp
    let elf = parseElf elfBytes
    let (entry, iMem, dMem) = readElf elf

    -- TODO Use 'elfEntry' as an optional(?) argument to the core to start
    -- execution from a particular PC value.

    -- Hook up to println-debugging at special address 0x90000000
    let res = getDataBytes (BS.length expected) 0x90000000 $ sampleN n $ fmap snd $
              contranomy' hasClock hasReset entry iMem dMem $ pure (False, False, 0b0)

    res @?= expected


findIntegrationTests :: [FilePath] -> [(String, FilePath, FilePath)]
findIntegrationTests =
  map (\p -> (takeBaseName p, dropExtension p, p))
  . filter (\p -> takeExtension p == ".expected")


runTest :: String -- ^
        -> FilePath
        -> FilePath
        -> TestTree
runTest name elfPath expectedPath =
  testCase ("Integration test `" <> name <> "`") $ do
    expected <- BS.readFile expectedPath
    let act path = copyFile elfPath path

    -- randomly pick 100 thousand cycles as an upper bound for tests
    elfExpect act 100000 expected




generateTests :: FilePath -> IO TestTree
generateTests path = do
  files <- listDirectory path

  let integs = findIntegrationTests files

      tests = flip map integs $ \(name, elfPath, expectPath) ->
          runTest name (path </> elfPath) (path </> expectPath)

  pure $ testGroup "Firmware integration tests" tests
