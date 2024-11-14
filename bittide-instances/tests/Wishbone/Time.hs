-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# OPTIONS_GHC -fplugin=Protocols.Plugin #-}

module Wishbone.Time where

-- Preludes
import Clash.Prelude

-- Local
import Bittide.DoubleBufferedRam
import Bittide.Instances.Domains
import Bittide.ProcessingElement
import Bittide.ProcessingElement.Util
import Bittide.SharedTypes
import Bittide.Wishbone
import Project.FilePath

-- Other
import Control.Monad (forM_)
import Data.Char
import Data.Maybe
import Language.Haskell.TH
import Protocols
import Protocols.Idle
import System.FilePath
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.TH
import Text.Parsec
import Text.Parsec.String

-- Qualified
import qualified Protocols.Df as Df

sim :: IO ()
sim = putStrLn simResult
 where
  simResult = chr . fromIntegral <$> mapMaybe Df.dataToMaybe uartStream
  uartStream = sampleC def dut

{- | Run the timing module self test with processingElement and inspect it's uart output.
The test returns names of tests and a boolean indicating if the test passed.
-}
case_time_rust_self_test :: Assertion
case_time_rust_self_test =
  -- Run the test with HUnit
  case parseTestResults simResult of
    Left err -> assertFailure $ show err <> "\n" <> simResult
    Right results -> do
      forM_ results $ \result -> assertResult result
 where
  assertResult (TestResult name (Just err)) = assertFailure ("Test " <> name <> " failed with error" <> err)
  assertResult (TestResult _ Nothing) = return ()
  simResult = chr . fromIntegral <$> mapMaybe Df.dataToMaybe uartStream
  uartStream = sampleC def dut

{- | A simple instance containing just VexRisc and UART as peripheral.
Runs the `hello` binary from `firmware-binaries`.
-}
dut ::
  Circuit () (Df Basic50 (BitVector 8))
dut = withClockResetEnable clockGen resetGen enableGen
  $ circuit
  $ \_unit -> do
    (uartRx, jtag) <- idleSource -< ()
    [uartBus, timeBus] <- processingElement peConfig -< jtag
    (uartTx, _uartStatus) <- uartInterfaceWb d2 d2 uartSim -< (uartBus, uartRx)
    timeWb -< timeBus
    idC -< uartTx
 where
  (iMem, dMem) =
    $( do
        root <- runIO $ findParentContaining "cabal.project"
        let
          elfDir = root </> firmwareBinariesDir "riscv32imc-unknown-none-elf" Release
          elfPath = elfDir </> "time_self_test"

          iSize = 64 * 1024 -- 64 KB
          dSize = 64 * 1024 -- 64 KB
        memBlobsFromElf BigEndian (Just iSize, Just dSize) elfPath Nothing
     )

  peConfig =
    PeConfig
      (0b00 :> 0b01 :> 0b10 :> 0b11 :> Nil)
      (Reloadable $ Blob iMem)
      (Reloadable $ Blob dMem)

data TestResult = TestResult String (Maybe String) deriving (Show)

type Ascii = BitVector 8
asciiToChar :: Ascii -> Char
asciiToChar = chr . fromIntegral

testResultParser :: Parser TestResult
testResultParser = do
  testName <- manyTill anyChar (try (string ": "))
  result <-
    choice
      [ string "None" >> return Nothing
      , Just <$> (string "Some(" *> manyTill anyChar (char ')'))
      ]
  _ <- endOfLine
  return $ TestResult testName result

testResultsParser :: Parser [TestResult]
testResultsParser = do
  _ <- string "Start time self test" >> endOfLine
  manyTill testResultParser done
 where
  done = try (string "Done") >> endOfLine >> return ()

parseTestResults :: String -> Either ParseError [TestResult]
parseTestResults = parse testResultsParser ""

tests :: TestTree
tests = $(testGroupGenerator)
