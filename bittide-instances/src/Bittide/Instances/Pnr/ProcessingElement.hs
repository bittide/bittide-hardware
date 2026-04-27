-- SPDX-FileCopyrightText: 2023 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
module Bittide.Instances.Pnr.ProcessingElement where

import Clash.Prelude

import Bittide.Cpus.Riscv32imc (vexRiscv0)
import Bittide.Instances.Common (
  PeConfigElfSource (TryEnv, backup, envVar),
  emptyPeConfig,
  peConfigFromElf,
 )
import Bittide.Instances.Domains
import Bittide.ProcessingElement
import Bittide.SharedTypes (withLittleEndian)
import Bittide.Wishbone
import Clash.Annotations.TH
import Clash.Cores.UART (ValidBaud)
import Clash.Explicit.Prelude (noReset, orReset)
import Clash.Xilinx.ClockGen
import Project.FilePath
import Protocols
import Protocols.MemoryMap as Mm
import VexRiscv

baudRate :: SNat 921600
baudRate = SNat

peConfigSim :: IO (PeConfig 4)
peConfigSim =
  peConfigFromElf
    (SNat @IMemWords)
    (SNat @DMemWords)
    TryEnv{envVar = "TEST_BINARY_NAME", backup = "vexriscv-hello"}
    Debug
    d0
    d0
    False
    vexRiscv0

peConfigRtl :: PeConfig 4
peConfigRtl = emptyPeConfig (SNat @IMemWords) (SNat @DMemWords) d0 d0 False vexRiscv0

vexRiscvUartHelloMM :: Mm.MemoryMap
vexRiscvUartHelloMM =
  getMMAny
    $ withClockResetEnable clockGen resetGen enableGen
    $ vexRiscvUartHelloC @Basic200 baudRate peConfigRtl

vexRiscvUartHelloC ::
  forall dom baud.
  ( HiddenClockResetEnable dom
  , KnownNat baud
  , ValidBaud dom baud
  ) =>
  SNat baud ->
  PeConfig 4 ->
  Circuit
    (ToConstBwd Mm, (CSignal dom Bit, Jtag dom))
    (CSignal dom Bit)
vexRiscvUartHelloC baudSnat peConfig = withLittleEndian $ circuit $ \(mm, (uartRx, jtag)) -> do
  [uartBus, timeBus] <- processingElement NoDumpVcd peConfig -< (mm, jtag)
  (uartTx, _uartStatus) <- uartInterfaceWb d16 d16 (uartDf baudSnat) -< (uartBus, uartRx)
  _localCounter <- timeWb Nothing -< timeBus
  idC -< uartTx

{- | A simple instance containing just VexRisc and UART as peripheral.
Runs the `hello` binary from `firmware-binaries`.
-}
vexRiscUartHello ::
  "SYSCLK_125" ::: DiffClock Ext125 ->
  "CPU_RESET" ::: Reset Basic200 ->
  ( ""
      ::: ( "USB_UART_TX" ::: Signal Basic200 Bit
          , "JTAG" ::: Signal Basic200 JtagIn
          )
  , ()
  ) ->
  ( ""
      ::: ( "" ::: ()
          , "JTAG" ::: Signal Basic200 JtagOut
          )
  , "USB_UART_RX" ::: Signal Basic200 Bit
  )
vexRiscUartHello diffClk rst_in ((uartTx, jtagIn), _) =
  let circuitFn =
        toSignals
          $ withClockResetEnable clk200 rst200 enableGen
          $ vexRiscvUartHelloC baudRate peConfigRtl
   in case circuitFn (((), (uartTx, jtagIn)), ()) of
        ((_mm, a), b) -> (a, b)
 where
  (clk200, rst200_) = clockWizardDifferential diffClk noReset
  rst200 = rst200_ `orReset` rst_in

type IMemWords = DivRU (2 * 1024) 4
type DMemWords = DivRU (1 * 1024) 4

makeTopEntity 'vexRiscUartHello
