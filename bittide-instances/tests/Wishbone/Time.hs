-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# OPTIONS_GHC -fplugin=Protocols.Plugin #-}

module Wishbone.Time where

import Bittide.DoubleBufferedRam
import Bittide.Instances.Domains
import Bittide.ProcessingElement
import Bittide.ProcessingElement.Util
import Bittide.SharedTypes
import Bittide.Wishbone
import Clash.Cores.UART (ValidBaud, uart)
import Clash.Cores.UART.Extra (MaxBaudRate)
import Clash.Explicit.Prelude
import Clash.Explicit.Testbench
import Clash.Prelude (withClockResetEnable)
import Clash.Xilinx.ClockGen
import Control.Monad (forM_)
import Data.Char
import Data.Maybe
import Language.Haskell.TH
import Project.FilePath
import Protocols
import System.FilePath
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.TH
import Text.Parsec
import Text.Parsec.String
import VexRiscv

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
  baud = SNat @(MaxBaudRate Basic50)
  clk = clockGen
  rst = resetGen
  ena = enableGen
  simResult = fmap (asciiToChar . fromIntegral) $ catMaybes $ sampleN 1_000_000 uartStream
  (uartStream, _, _) = withClockResetEnable (clockGen @Basic50) rst ena $ uart baud uartTx (pure Nothing)
  uartTx = dut baud (clockToDiffClock clk) rst (pure 0)

{- | A simple instance containing just VexRisc and UART as peripheral.
Runs the `hello` binary from `firmware-binaries`.
-}
dut ::
  forall dom baud.
  (KnownDomain dom, ValidBaud dom baud) =>
  SNat baud ->
  "SYSCLK_300" ::: DiffClock Ext300 ->
  "CPU_RESET" ::: Reset dom ->
  "USB_UART_TX" ::: Signal dom Bit ->
  "USB_UART_RX" ::: Signal dom Bit
dut baud diffClk rst_in usbUartTx = usbUartRx
 where
  (_, usbUartRx) = go ((usbUartTx, pure $ JtagIn low low low), pure ())

  go =
    toSignals
      $ withClockResetEnable clk200 rst200 enableGen
      $ circuit
      $ \(uartRx, jtag) -> do
        [uartBus, timeBus] <- processingElement @dom peConfig -< jtag
        (uartTx, _uartStatus) <- uartWb d256 d16 baud -< (uartBus, uartRx)
        timeWb -< timeBus
        idC -< uartTx

  (clk200 :: Clock dom, pllLock :: Reset dom) = clockWizardDifferential diffClk noReset
  rst200 = resetSynchronizer clk200 (unsafeOrReset rst_in pllLock)

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
