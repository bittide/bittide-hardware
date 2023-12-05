-- SPDX-FileCopyrightText: 2023 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fplugin=Protocols.Plugin #-}
module Bittide.Instances.Pnr.ProcessingElement where

import Clash.Prelude

import Clash.Annotations.TH
import Clash.Explicit.Prelude(orReset, noReset)
import Clash.Xilinx.ClockGen
import Language.Haskell.TH
import Protocols
import Protocols.Internal
import System.FilePath

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
  "SYSCLK_300" ::: DiffClock Ext300 ->
  "CPU_RESET" ::: Reset Basic200 ->
  ("USB_UART_TX" ::: CSignal Basic200 Bit, CSignal Basic200 ()) ->
  (CSignal Basic200 (), "USB_UART_RX" ::: CSignal Basic200 Bit)
vexRiscUartHello diffClk rst_in =
  toSignals $ withClockResetEnable clk200 rst200 enableGen $
    circuit $ \uartRx -> do
      [uartBus] <- (processingElement @Basic200 peConfig) -< ()
      (uartTx, _uartStatus) <- uartWb d16 d16 (SNat @921600) -< (uartBus, uartRx)
      idC -< uartTx
 where
  (clk200, rst200_) = clockWizardDifferential diffClk noReset
  rst200 = rst200_ `orReset` rst_in

  ( (_iStart, _iSize, iMem)
    , (_dStart, _dSize, dMem)) = $(do
      root <- runIO $ findParentContaining "cabal.project"
      let
        elfDir = root </> firmwareBinariesDir "riscv32imc-unknown-none-elf" True
        elfPath = elfDir </> "hello"
      memBlobsFromElf BigEndian elfPath Nothing)

  peConfig = PeConfig (0 :> 1 :> 2 :> Nil) (Reloadable $ Blob iMem) (Reloadable $ Blob dMem)

makeTopEntity 'vexRiscUartHello
