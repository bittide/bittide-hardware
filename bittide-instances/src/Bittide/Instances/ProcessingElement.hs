-- SPDX-FileCopyrightText: 2023 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fplugin=Protocols.Plugin #-}
module Bittide.Instances.ProcessingElement where

import Clash.Prelude

import Clash.Annotations.TH
import Clash.Explicit.Prelude(orReset, noReset)
import Clash.Xilinx.ClockGen
import Language.Haskell.TH
import Protocols
import Protocols.Internal
import System.FilePath
import VexRiscv

import Bittide.DoubleBufferedRam
import Bittide.Instances.Domains
import Bittide.ProcessingElement
import Bittide.ProcessingElement.Util
import Bittide.SharedTypes
import Bittide.Wishbone
import Project.FilePath

-- | A simple instance containing just VexRisc and UART as peripheral.
-- Runs the `hello` binary from `firmware-binaries`.
vexRiscUartHello ::
  "SYSCLK_300" ::: DiffClock Basic300 ->
  "CPU_RESET" ::: Reset Basic200 ->
  "TCK" ::: Clock Basic50 ->
  "JTAG_EN" ::: Enable Basic50 ->
  "TDI" ::: Signal Basic50 Bit ->
  "TMS" ::: Signal Basic50 Bit ->
  "USB_UART_TX" ::: Signal Basic200 Bit ->
  ( "USB_UART_RX" ::: Signal Basic200 Bit
  , "TDO" ::: Signal Basic50 Bit )
vexRiscUartHello diffClk rst_in tck jtagEn tdi tms usbUartTx =
  (usbUartRx, tdo)
 where
  (_, (CSignal usbUartRx, fmap testDataOut -> tdo)) =
    circuitFn (CSignal usbUartTx, (CSignal $ pure (), JtagIn <$> tms <*> tdi))

  circuitFn = toSignals $
    circuit $ \uartRx -> do
      ([uartBus], jtag) <- processingElement peConfig clk200 rst200 tck jtagEn
      (uartTx, _uartStatus) <- withClockResetEnable clk200 rst200 enableGen uartWb d16 d16 (SNat @921600) -< (uartBus, uartRx)
      idC -< (uartTx, jtag)

  (clk200, pllLock) = clockWizardDifferential (SSymbol @"pll_300_200") diffClk noReset
  rst200 = resetSynchronizer clk200 (orReset rst_in (unsafeFromActiveLow pllLock))

  ( (_iStart, _iSize, iMem)
    , (_dStart, _dSize, dMem)) = $(do
      root <- runIO $ findParentContaining "cabal.project"
      let
        elfDir = root </> firmwareBinariesDir "riscv32imc-unknown-none-elf" True
        elfPath = elfDir </> "hello"
      memBlobsFromElf BigEndian elfPath Nothing)

  peConfig = PeConfig (0 :> 1 :> 2 :> Nil) (Reloadable $ Blob iMem) (Reloadable $ Blob dMem)

makeTopEntity 'vexRiscUartHello
