  -- SPDX-FileCopyrightText: 2023 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fplugin=Protocols.Plugin #-}
module Bittide.Instances.ProcessingElement where

import Clash.Prelude

import Clash.Annotations.TH
import Language.Haskell.TH
import Paths_bittide_instances
import Bittide.DoubleBufferedRam
import Bittide.Instances.Domains
import Bittide.ProcessingElement
import Bittide.ProcessingElement.Util
import Bittide.Wishbone
import Bittide.SharedTypes
import Protocols
import Protocols.Internal
import Clash.Xilinx.ClockGen
import Clash.Cores.Xilinx.Extra

vexRiscUartEcho ::
  "SYSCLK_300_N" ::: Clock Basic300 ->
  "SYSCLK_300_P" ::: Clock Basic300 ->
  "CPU_RESET" ::: Reset Basic300 ->
  ("USB_UART_TX" ::: CSignal Basic200 Bit, CSignal Basic200 ()) ->
  (CSignal Basic200 (), "USB_UART_RX" ::: CSignal Basic200 Bit)
vexRiscUartEcho clk_n clk_p rst_in =
  toSignals $ withClockResetEnable clk200 rst200 enableGen $
    circuit $ \uartRx -> do
      [uartBus] <- (processingElement @Basic200 peConfig) -< ()
      (uartTx, _uartStatus) <- uartWb d16 d16 (SNat @921600) -< (uartBus, uartRx)
      idC -< uartTx
 where
  clk300 = ibufds clk_p clk_n
  rst300 = resetGlitchFilter d1024 clk300 rst_in
  (clk200, pllLock) = clockWizard (SSymbol @"pll_300_200") clk300 rst300
  rst200 = resetSynchronizer clk200 (unsafeFromLowPolarity pllLock)

  (  (_iStart, _iSize, iMem)
   , (_dStart, _dSize, dMem)) = $(do
      elfPath <- runIO $ getDataFileName "data/binaries/hello"
      memBlobsFromElf BigEndian elfPath Nothing)

  peConfig = PeConfig (0 :> 1 :> 2 :> Nil) (Reloadable $ Blob iMem) (Reloadable $ Blob dMem)

makeTopEntity 'vexRiscUartEcho
