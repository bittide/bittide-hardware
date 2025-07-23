-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Wishbone.Time where

-- Preludes
import Clash.Prelude

-- Local
import Bittide.DoubleBufferedRam
import Bittide.Instances.Domains
import Bittide.ProcessingElement
import Bittide.ProcessingElement.Util
import Bittide.SharedTypes (withBittideByteOrder)
import Bittide.Wishbone
import Project.FilePath

-- Other
import Clash.Class.BitPackC (ByteOrder (BigEndian))
import Control.Monad (forM_)
import Data.Char
import Data.Maybe
import Protocols
import Protocols.Idle
import Protocols.MemoryMap
import System.FilePath
import System.IO.Unsafe (unsafePerformIO)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.TH
import Text.Parsec
import Text.Parsec.String
import VexRiscv (DumpVcd (NoDumpVcd))

sim :: IO ()
sim = putStrLn simResult

simResult :: String
simResult = chr . fromIntegral <$> catMaybes uartStream
 where
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

{- | A simple instance containing just VexRisc and UART as peripheral.
Runs the `hello` binary from `firmware-binaries`.
-}
dut :: Circuit () (Df Basic50 (BitVector 8))
dut = withBittideByteOrder
  $ withClockResetEnable clockGen resetGen enableGen
  $ circuit
  $ \_unit -> do
    (uartRx, jtag) <- idleSource
    [(prefixUart, uartBus), (prefixTime, (mmTime, timeBus))] <-
      processingElement NoDumpVcd peConfig -< (mm, jtag)
    mm <- ignoreMM
    (uartTx, _uartStatus) <- uartInterfaceWb d2 d2 uartBytes -< (uartBus, uartRx)
    constBwd 0b10 -< prefixUart
    _localCounter <- timeWb -< (mmTime, timeBus)
    constBwd 0b11 -< prefixTime
    idC -< uartTx
 where
  peConfig = unsafePerformIO $ do
    root <- findParentContaining "cabal.project"
    let elfPath = root </> firmwareBinariesDir "riscv32imc" Release </> "time_self_test"
    (iMem, dMem) <- vecsFromElf @IMemWords @DMemWords BigEndian elfPath Nothing
    pure
      PeConfig
        { initI = Reloadable (Vec iMem)
        , prefixI = 0b00
        , initD = Reloadable (Vec dMem)
        , prefixD = 0b01
        , iBusTimeout = d0 -- No timeouts on the instruction bus
        , dBusTimeout = d0 -- No timeouts on the data bus
        , includeIlaWb = False
        }
{-# NOINLINE dut #-}

type IMemWords = DivRU (64 * 1024) 4
type DMemWords = DivRU (32 * 1024) 4

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
