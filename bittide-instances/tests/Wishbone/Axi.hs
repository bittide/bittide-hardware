-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# OPTIONS_GHC -fplugin=Protocols.Plugin #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=10 #-}

module Wishbone.Axi where

import Clash.Explicit.Prelude

import Bittide.Axi4
import Bittide.DoubleBufferedRam
import Bittide.Instances.Domains
import Bittide.ProcessingElement
import Bittide.ProcessingElement.Util
import Bittide.SharedTypes
import Bittide.Wishbone
import Clash.Cores.UART(uart, ValidBaud)
import Clash.Cores.UART.Extra(MaxBaudRate)
import Clash.Explicit.Testbench
import Clash.Prelude(withClockResetEnable)
import Clash.Xilinx.ClockGen
import Control.Monad (forM_)
import Data.Char
import Data.Maybe
import Data.Proxy
import Language.Haskell.TH
import Project.FilePath
import Protocols
import Protocols.Axi4.Stream
import Protocols.Wishbone
import System.FilePath
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.TH
import Text.Parsec
import Text.Parsec.String
import VexRiscv

import qualified Protocols.DfConv as DfConv


-- | Run the axi module self test with processingElement and inspect it's uart output.
-- The test returns names of tests and a boolean indicating if the test passed.
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
  baud = SNat @(MaxBaudRate Basic50)
  clk = clockGen
  rst = resetGen
  ena = enableGen
  simResult = fmap (chr . fromIntegral) $ catMaybes $ sampleN  500_000 uartStream
  (uartStream, _, _) = withClockResetEnable (clockGen @Basic50) rst ena $ uart baud uartTx (pure Nothing)
  (_, uartTx) = dut baud (clockToDiffClock clk) rst (pure 0, pure ())


-- | A simple instance containing just VexRisc and UART as peripheral.
-- Runs the `hello` binary from `firmware-binaries`.
dut ::
  forall dom baud .
  (KnownDomain dom, ValidBaud dom baud) =>
  SNat baud ->
  "SYSCLK_300" ::: DiffClock Ext300 ->
  "CPU_RESET" ::: Reset dom ->
  ("USB_UART_TX" ::: Signal dom Bit, Signal dom ()) ->
  (Signal dom (), "USB_UART_RX" ::: Signal dom Bit)
dut baud diffClk rst_in =
  toSignals $ withClockResetEnable clk200 rst200 enableGen $
    circuit $ \uartTx -> do
      [uartBus, axiTxBus, wbNull, axiRxBus] <- processingElement @dom peConfig <| jtagIdle -< ()
      wbAlwaysAck -< wbNull
      (uartRx, _uartStatus) <- uartWb d128 d2 baud -< (uartBus, uartTx)
      _interrupts <- wbAxisRxBufferCircuit (SNat @128) -< (axiRxBus, axiStream)
      axiStream <- axiUserMapC (const False) <| DfConv.fifo axiProxy axiProxy (SNat @1024) <|
        axiPacking <| wbToAxiTx -< axiTxBus
      idC -< uartRx
 where
  axiProxy = Proxy @(Axi4Stream dom ('Axi4StreamConfig 4 0 0) ())
  (clk200 :: Clock dom, pllLock :: Reset dom) = clockWizardDifferential diffClk noReset
  rst200 = resetSynchronizer clk200 (unsafeOrReset rst_in pllLock)

  (iMem, dMem) = $(do
      root <- runIO $ findParentContaining "cabal.project"
      let
        elfDir = root </> firmwareBinariesDir "riscv32imc-unknown-none-elf" Release
        elfPath = elfDir </> "axi_stream_self_test"
      memBlobsFromElf BigEndian (Nothing, Nothing) elfPath Nothing)

  jtagIdle = Circuit $ const ((), pure $ JtagIn low low low)
  peConfig =
    PeConfig
    (0b000 :> 0b001 :> 0b010 :> 0b011:> 0b100 :> 0b101 :> Nil)
    (Reloadable $ Blob iMem)
    (Reloadable $ Blob dMem)


data TestResult = TestResult String (Maybe String) deriving (Show, Eq)

wbAlwaysAck :: NFDataX a => Circuit
  (Wishbone dom 'Standard addrW a)
  ()
wbAlwaysAck = Circuit (const (pure $ emptyWishboneS2M{acknowledge = True}, ()))

testResultParser :: Parser TestResult
testResultParser = do
  testName <- manyTill anyChar (try (string ": "))
  result <- choice [string "None" >> return Nothing, Just <$> (string "Some(" *> manyTill anyChar (char ')'))]
  _ <- endOfLine
  return $ TestResult testName result

testResultsParser :: Parser [TestResult]
testResultsParser = do
  _ <- string "Start axi self test" >> endOfLine
  manyTill testResultParser done
  where
    done = try (string "Done") >> endOfLine >> return ()

-- | Parse test results from the simulation output. See 'case_parseTestResults'
-- for example inputs.
parseTestResults :: String -> Either ParseError [TestResult]
parseTestResults = parse testResultsParser ""

case_parseTestResults :: Assertion
case_parseTestResults = do
  Right [] @=? parseTestResults "Start axi self test\nDone\n"

  Right [TestResult "a" Nothing] @=?
    parseTestResults "Start axi self test\na: None\nDone\n"

  Right [TestResult "a" Nothing, TestResult "b" Nothing] @=?
    parseTestResults "Start axi self test\na: None\nb: None\nDone\n"

  Right [TestResult "a" (Just "1"),TestResult "b" Nothing] @=?
    parseTestResults "Start axi self test\na: Some(1)\nb: None\nDone\n"

tests :: TestTree
tests = $(testGroupGenerator)
