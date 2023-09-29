-- SPDX-FileCopyrightText: 2023-2024 Google LLC
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
import Protocols.Wishbone
import Protocols.Internal (CSignal(..))
import System.FilePath
import VexRiscv

import Bittide.DoubleBufferedRam
import Bittide.Instances.Domains
import Bittide.ProcessingElement
import Bittide.ProcessingElement.Util
import Bittide.SharedTypes
import Bittide.Wishbone
import Project.FilePath

stripCSignal :: CSignal dom a -> Signal dom a
stripCSignal (CSignal s) = s

alwaysAck :: KnownNat nBytes => Circuit (Wishbone dom 'Standard addrW (Bytes nBytes)) ()
alwaysAck = Circuit go
 where
  go (_m2s, ()) = (pure emptyWishboneS2M{acknowledge=True}, ())


-- | A simple instance containing just VexRisc and UART as peripheral.
-- Runs the `hello` binary from `firmware-binaries`.
vexRiscUartHello ::
  "SYSCLK_300" ::: DiffClock Ext300 ->
  "CPU_RESET" ::: Reset Basic200 ->
  ( "" ::: ( "USB_UART_TX" ::: CSignal Basic200 Bit
           , "JTAG" ::: CSignal Basic200 JtagIn
          )
  , (CSignal Basic200 (), CSignal Basic200 ())
  ) ->
  ( ( CSignal Basic200 ()
    , CSignal Basic200 () )
  , "" ::: ( "USB_UART_RX" ::: CSignal Basic200 Bit
           , "JTAG" ::: CSignal Basic200 JtagOut
           )
  )
vexRiscUartHello diffClk rst_in =
  toSignals $ withClockResetEnable clk200 rst200 enableGen $
    circuit $ \(uartRx, jtagIn) -> do
      ([uartBus, timeBus], jtagOut) <- processingElement @Basic200 peConfig -< jtagIn
      (uartTx, _uartStatus) <- uartWb d16 d16 (SNat @921600) -< (uartBus, uartRx)
      timeWb -< timeBus
      idC -< (uartTx, jtagOut)
 where
  (clk200, rst200_) = clockWizardDifferential diffClk noReset
  rst200 = rst200_ `orReset` rst_in

  ( (_iStart, _iSize, iMem)
    , (_dStart, _dSize, dMem)) = $(do
      root <- runIO $ findParentContaining "cabal.project"
      let
        elfDir = root </> firmwareBinariesDir "riscv32imc-unknown-none-elf" False
        elfPath = elfDir </> "hello"
      memBlobsFromElf BigEndian elfPath Nothing)

  -- ╭────────┬───────┬───────┬────────────────────╮
  -- │ bin    │ hex   │ bus   │ description        │
  -- ├────────┼───────┼───────┼────────────────────┤
  -- │ 0b000. │ 0x0   │       │ INSTR              │
  -- │ 0b001. │ 0x2   │       │                    │
  -- │ 0b010. │ 0x4   │       │ DATA               │
  -- │ 0b011. │ 0x6   │       │                    │
  -- │ 0b100. │ 0x8   │       │ UART               │
  -- │ 0b101. │ 0xA   │       │                    │
  -- │ 0b110. │ 0xC   │       │ TIME               │
  -- │ 0b111. │ 0xE   │       │                    │
  -- ╰────────┴───────┴───────┴────────────────────╯


  peConfig =
    PeConfig (0b00 :> 0b01 :> 0b10 :> 0b11 :> Nil)
    (Reloadable $ Blob iMem)
    (Reloadable $ Blob dMem)

makeTopEntity 'vexRiscUartHello
