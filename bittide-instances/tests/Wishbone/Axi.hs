-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Wishbone.Axi where

-- Preludes
import Clash.Explicit.Prelude
import Clash.Prelude (withClockResetEnable)

-- Local
import Bittide.Axi4
import Bittide.DoubleBufferedRam
import Bittide.ProcessingElement
import Bittide.ProcessingElement.Util
import Bittide.Wishbone
import Project.FilePath

-- Other
import Bittide.SharedTypes (withBittideByteOrder)
import Clash.Class.BitPackC (ByteOrder (BigEndian))
import Control.Monad (forM_)
import Data.Char
import Data.Maybe
import Data.Proxy
import Protocols
import Protocols.Axi4.Stream
import Protocols.Idle
import Protocols.MemoryMap
import Protocols.Wishbone
import System.FilePath
import System.IO.Unsafe (unsafePerformIO)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.TH
import Text.Parsec
import Text.Parsec.String
import VexRiscv (DumpVcd (NoDumpVcd))

-- Qualified
import qualified Protocols.DfConv as DfConv

-- {-# ANN module "HLint: Missing NOINLINE pragma" #-}

sim :: IO ()
sim =
  putStr
    $ fmap (chr . fromIntegral)
    $ catMaybes (sampleC def dut)

{- | Run the axi module self test with processingElement and inspect it's uart output.
The test returns names of tests and a boolean indicating if the test passed.
-}
case_axi_stream_rust_self_test :: Assertion
case_axi_stream_rust_self_test =
  -- Run the test with HUnit
  case parseTestResults simResult of
    Left errMsg -> assertFailure $ show errMsg <> "\n" <> simResult
    Right results -> do
      forM_ results $ \result -> assertResult result
 where
  assertResult (TestResult name (Just errMsg)) = assertFailure ("Test " <> name <> " failed with error \"" <> errMsg <> "\"")
  assertResult (TestResult _ Nothing) = return ()
  simResult = chr . fromIntegral <$> catMaybes uartStream
  uartStream = sampleC def dut

{- | A simple instance containing just VexRisc and UART as peripheral.
Runs the `hello` binary from `firmware-binaries`.
-}
dut :: Circuit () (Df System (BitVector 8))
dut =
  withBittideByteOrder
    $ withClockResetEnable clockGen (resetGenN d2) enableGen
    $ circuit
    $ \_unit -> do
      (uartTx, jtag) <- idleSource
      [ (prefixUart, uartBus)
        , (prefixAxiTx, (mmAxiTx, axiTxBus))
        , (prefixNull, (mmNull, wbNull))
        , (prefixAxiRx, (mmAxiRx, axiRxBus))
        ] <-
        processingElement NoDumpVcd peConfig -< (mm, jtag)
      wbAlwaysAck -< wbNull
      constBwd 0b100 -< prefixNull
      constBwd todoMM -< mmNull
      mm <- ignoreMM

      (uartRx, _uartStatus) <- uartInterfaceWb d2 d2 uartBytes -< (uartBus, uartTx)
      constBwd 0b010 -< prefixUart

      _interrupts <- wbAxisRxBufferCircuit (SNat @128) -< (axiRxBus, axiStream)
      constBwd 0b101 -< prefixAxiRx
      constBwd todoMM -< mmAxiRx

      axiStream <-
        axiUserMapC (const False)
          <| DfConv.fifo axiProxy axiProxy (SNat @1024)
          <| axiPacking
          <| wbToAxiTx
          -< axiTxBus
      constBwd 0b011 -< prefixAxiTx
      constBwd todoMM -< mmAxiTx
      idC -< uartRx
 where
  axiProxy = Proxy @(Axi4Stream System ('Axi4StreamConfig 4 0 0) ())
  peConfig = unsafePerformIO $ do
    root <- findParentContaining "cabal.project"
    let elfPath = root </> firmwareBinariesDir "riscv32imc" Release </> "axi_stream_self_test"
    (iMem, dMem) <- vecsFromElf @IMemWords @DMemWords BigEndian elfPath Nothing
    pure
      PeConfig
        { initI = Reloadable (Vec iMem)
        , prefixI = 0b000
        , initD = Reloadable (Vec dMem)
        , prefixD = 0b001
        , iBusTimeout = d0 -- No timeouts on the instruction bus
        , dBusTimeout = d0 -- No timeouts on the data bus
        , includeIlaWb = False
        , whoAmIPrefix = 0b111
        , whoAmID = 0x3075_7063
        }
{-# NOINLINE dut #-}

type IMemWords = DivRU (64 * 1024) 4
type DMemWords = DivRU (32 * 1024) 4

data TestResult = TestResult String (Maybe String) deriving (Show, Eq)

wbAlwaysAck ::
  (NFDataX a) =>
  Circuit
    (Wishbone dom 'Standard addrW a)
    ()
wbAlwaysAck = Circuit (const (pure $ emptyWishboneS2M{acknowledge = True}, ()))

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
  _ <- string "Start axi self test" >> endOfLine
  manyTill testResultParser done
 where
  done = try (string "Done") >> endOfLine >> return ()

{- | Parse test results from the simulation output. See 'case_parseTestResults'
for example inputs.
-}
parseTestResults :: String -> Either ParseError [TestResult]
parseTestResults = parse testResultsParser ""

case_parseTestResults :: Assertion
case_parseTestResults = do
  Right [] @=? parseTestResults "Start axi self test\nDone\n"

  Right [TestResult "a" Nothing]
    @=? parseTestResults "Start axi self test\na: None\nDone\n"

  Right [TestResult "a" Nothing, TestResult "b" Nothing]
    @=? parseTestResults "Start axi self test\na: None\nb: None\nDone\n"

  Right [TestResult "a" (Just "1"), TestResult "b" Nothing]
    @=? parseTestResults "Start axi self test\na: Some(1)\nb: None\nDone\n"

tests :: TestTree
tests = $(testGroupGenerator)
