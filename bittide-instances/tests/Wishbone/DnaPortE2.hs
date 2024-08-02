-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# OPTIONS_GHC -fplugin=Protocols.Plugin #-}

module Wishbone.DnaPortE2 where

import Clash.Explicit.Prelude
import Clash.Prelude(withClockResetEnable)

import Clash.Cores.UART(uart, ValidBaud)
import Clash.Cores.Xilinx.Unisim.DnaPortE2
import Clash.Explicit.Testbench
import Clash.Xilinx.ClockGen
import Data.Char
import Data.Maybe
import Language.Haskell.TH
import Numeric
import Project.FilePath
import Protocols
import System.FilePath
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.TH
import VexRiscv

import Bittide.DoubleBufferedRam
import Bittide.Instances.Domains
import Bittide.ProcessingElement
import Bittide.ProcessingElement.Util
import Bittide.SharedTypes
import Bittide.Wishbone
import Clash.Cores.UART.Extra(MaxBaudRate)

import qualified Prelude as P

-- | Test whether we can read the DNA from the DNA port peripheral.
case_dna_port_self_test :: Assertion
case_dna_port_self_test = assertBool msg (receivedDna == simDna2)
 where
  msg = "Received dna " <> showHex receivedDna "" <> " not equal to expected dna " <> showHex simDna2 ""
  receivedDna = parseResult simResult
  baud = SNat @(MaxBaudRate Basic50)
  clk = clockGen
  rst = resetGen
  ena = enableGen
  simResult = fmap (chr . fromIntegral) $ catMaybes $ sampleN 100_000 uartStream
  (uartStream, _, _) = withClockResetEnable (clockGen @Basic50) rst ena $ uart baud uartTx (pure Nothing)
  uartTx = dut baud (clockToDiffClock clk) rst (pure 0)

-- | A simple instance containing just VexRisc with UART and the DNA peripheral which
-- runs the `dna_port_e2_test` binary from `firmware-binaries`.
dut ::
  forall dom baud .
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
    toSignals $ withClockResetEnable clk200 rst200 enableGen $
      circuit $ \(uartRx, jtag) -> do
        [uartBus, dnaWb] <- processingElement @dom peConfig -< jtag
        (uartTx, _uartStatus) <- uartWb d256 d16 baud -< (uartBus, uartRx)
        readDnaPortE2Wb simDna2 -< dnaWb
        idC -< uartTx

  (clk200 :: Clock dom, pllLock :: Reset dom) = clockWizardDifferential diffClk noReset
  rst200 = resetSynchronizer clk200 (unsafeOrReset rst_in pllLock)

  (iMem, dMem) = $(do
      root <- runIO $ findParentContaining "cabal.project"
      let
        elfDir = root </> firmwareBinariesDir "riscv32imc-unknown-none-elf" Release
        elfPath = elfDir </> "dna_port_e2_test"

      memBlobsFromElf BigEndian (Nothing, Nothing) elfPath Nothing)

  peConfig =
    PeConfig (0b00 :> 0b01 :> 0b10 :> 0b11 :> Nil)
    (Reloadable $ Blob iMem)
    (Reloadable $ Blob dMem)

parseResult :: String -> BitVector 96
parseResult = pack . (read :: String -> Unsigned 96) . P.head . lines

tests :: TestTree
tests = $(testGroupGenerator)
