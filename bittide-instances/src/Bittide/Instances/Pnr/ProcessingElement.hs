-- SPDX-FileCopyrightText: 2023-2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fplugin=Protocols.Plugin #-}
module Bittide.Instances.Pnr.ProcessingElement where

import Clash.Prelude

import Clash.Annotations.TH
import Clash.Cores.Xilinx.Ila (Depth(D4096))
import Clash.Explicit.Prelude(orReset, noReset)
import Clash.Xilinx.ClockGen
import Language.Haskell.TH
import Protocols
import Protocols.Internal
import System.FilePath
import Data.Maybe

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
  ( "USB_UART_TX" ::: CSignal Basic200 Bit
  , "sclBs" ::: BiSignalIn 'Floating Basic200 1
  , "sdaIn" ::: Signal Basic200 Bit
  ) ->
  ( "USB_UART_RX" ::: CSignal Basic200 Bit
  , "sclBs" ::: BiSignalOut 'Floating Basic200 1
  , "sdaOut" ::: Signal Basic200 Bit
  , "mux_select" ::: BitVector 3)
vexRiscUartHello diffClk rst_in (uartIn, sclBs, sdaIn) =
  ( uartOut
  , writeToBiSignal sclBs sclOut
  , fromMaybe 1 <$> sdaOut
  , 0b100
  )
 where
  peFunction =
    toSignals $ withClockResetEnable clk200 rst200 enableGen $
      circuit $ \(uartRx, i2cIn) -> do
        [uartBus, i2cBus0, timeBus] <- processingElement @Basic200 peConfig -< ()
        (uartTx, _uartStatus) <- uartWb d16 d16 (SNat @921600) -< (uartBus, uartRx)
        i2cBus1 <- ilaWb 0 D4096 -< i2cBus0
        i2cOut <- i2cWb -< (i2cBus1, i2cIn)
        timeWb -< timeBus
        idC -< (uartTx, i2cOut)

  (_,(uartOut, i2cO)) =
    peFunction ((uartIn, i2cIn), (CSignal $ pure (), CSignal $ pure ()))
  (CSignal (unbundle -> (sclOut, sdaOut))) = i2cO

  i2cIn :: CSignal Basic200 (Bit, Bit)
  i2cIn = CSignal $ bundle (readFromBiSignal sclBs, fromMaybe <$> sdaIn <*> sdaOut)
  (clk200, pllLock :: Reset Basic200) = clockWizardDifferential diffClk noReset
  rst200 = resetSynchronizer clk200 (orReset rst_in pllLock)

  ( (_iStart, _iSize, iMem)
    , (_dStart, _dSize, dMem)) = $(do
      root <- runIO $ findParentContaining "cabal.project"
      let
        elfDir = root </> firmwareBinariesDir "riscv32imc-unknown-none-elf" True
        elfPath = elfDir </> "hello"
      memBlobsFromElf BigEndian elfPath Nothing)

  peConfig =
    PeConfig (0 :> 1 :> 2 :> 3 :> 4 :> Nil)
    (Reloadable $ Blob iMem)
    (Reloadable $ Blob dMem)

makeTopEntity 'vexRiscUartHello
