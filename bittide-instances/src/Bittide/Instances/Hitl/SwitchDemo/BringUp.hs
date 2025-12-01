-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE CPP #-}

{- | Switch demo for a Bittide system. In concert with its driver file, this device under
test should demonstrate the predictability of a Bittide system once it has achieved logical
synchronicity.

For more details, see [QBayLogic's presentation](https://docs.google.com/presentation/d/1AGbAJQ1zhTPtrekKnQcthd0TUPyQs-zowQpV1ux4k-Y)
on the topic.
-}
module Bittide.Instances.Hitl.SwitchDemo.BringUp (bringUp) where

import Clash.Explicit.Prelude
import Clash.Prelude (HiddenClockResetEnable, withClockResetEnable)
import Protocols

import Bittide.BootPe (BootPeBusses, bootPe)
import Bittide.ClockControl
import Bittide.ClockControl.Callisto.Types (CallistoResult (..))
import Bittide.Df (asciiDebugMux)
import Bittide.DoubleBufferedRam (InitialContent (Undefined))
import Bittide.Instances.Domains (
  Basic125,
  Bittide,
  Ext200,
  GthRx,
  GthRxS,
  GthTxS,
 )
import Bittide.Instances.Hitl.Setup (LinkCount)
import Bittide.Instances.Hitl.SwitchDemo.Core (core)
import Bittide.Jtag (jtagChain, unsafeJtagSynchronizer)
import Bittide.ProcessingElement (PeConfig (..))
import Bittide.SharedTypes (Byte, withBittideByteOrder)
import Bittide.Sync (Sync)
import Bittide.Wishbone (arbiterMm, uartDf)
import Clash.Cores.Xilinx.DcFifo (dcFifoDf)
import Data.Char (ord)
import Protocols.MemoryMap (Mm)
import Protocols.Spi (Spi)
import VexRiscv (Jtag)

import qualified Bittide.Cpus.Riscv32imc as Riscv32imc
import qualified Bittide.Transceiver as Transceiver
import qualified Bittide.Transceiver.Wishbone as Transceiver
import qualified Clash.Cores.Xilinx.Gth as Gth

#ifdef SIM_BAUD_RATE
type Baud = MaxBaudRate Basic125
import Clash.Cores.UART.Extra
#else
type Baud = 921_600
#endif

baud :: SNat Baud
baud = SNat

uartLabels :: Vec 3 (Vec 2 Byte)
uartLabels =
  fmap (fromIntegral . ord)
    <$> ( $(listToVecTH "BT")
            :> $(listToVecTH "MU")
            :> $(listToVecTH "CC")
            :> Nil
        )

bootPeConfig :: PeConfig BootPeBusses
bootPeConfig =
  PeConfig
    { cpu = Riscv32imc.vexRiscv0
    , initI = Undefined @(Div (64 * 1024) 4)
    , initD = Undefined @(Div (64 * 1024) 4)
    , iBusTimeout = d0
    , dBusTimeout = d0
    , includeIlaWb = False
    }

-- ( ConstBwd MM
--     , Jtag dom
--     )
--     ( "UART_BYTES" ::: Df dom (BitVector 8)
--     , "SPI_DONE" ::: CSignal dom Bool
--     , Spi dom
--     , (ConstBwd MM, Wishbone dom 'Standard (RemainingBusWidth BootPeBusses) (Bytes 4))
--     )

{- | Reset logic:

HW:

  1. Wait for SPI
  2. Wait for transceivers handshakes (=> all domains are up after this)
  3. Send local counter for one cycle, connect to switch after (=> in parallel
     with steps 4 and onwards, just wait until the transceiver says it's sampling from the
     transmit data input (@txDatas@))
  4a. Deassert CC CPU reset
  4b. Deassert Bittide domain reset (=> MU CPU, PE)
  5. Wait for stable buffers
  6. Wait for elastic buffer initialization (=> signal we're ready to receive data)

SW (MU):

  1. Wait for all UGNs to be captured
-}
bringUp ::
  "REFCLK" ::: Clock Basic125 ->
  "TEST_RST" ::: Reset Basic125 ->
  Circuit
    ( "BT" ::: ToConstBwd Mm
    , "MU" ::: ToConstBwd Mm
    , "CC" ::: ToConstBwd Mm
    , Jtag Basic125
    , Gth.Gths GthRx GthRxS Bittide GthTxS Ext200 LinkCount
    )
    ( Spi Basic125
    , Sync Bittide Basic125
    , "UART_TX" ::: CSignal Basic125 Bit
    , "FINC_FDEC" ::: CSignal Bittide (FINC, FDEC)
    )
bringUp refClk refRst = withBittideByteOrder $ circuit $ \(bootMm, muMm, ccMm, jtag, gths) -> do
  (bootUartBytes, _spiDone, spi, bootTransceiverWb) <-
    withRefClockResetEnable
      $ bootPe bootPeConfig
      -< (bootMm, bootJtag)

  transceiverWb <- withRefClockResetEnable arbiterMm -< [bootTransceiverWb]
  [bootJtag, otherJtag] <- jtagChain -< jtag
  otherJtagBittide <- unsafeJtagSynchronizer refClk bittideClk -< otherJtag

  -- Start UART multiplexing
  uartTxBytes <-
    withRefClockResetEnable
      $ asciiDebugMux d1024 uartLabels
      -< [bootUartBytes, muUartBytes, ccUartBytes]
  (_uartInBytes, uartTx) <- withRefClockResetEnable $ uartDf baud -< (uartTxBytes, Fwd 0)

  muUartBytes <-
    dcFifoDf d5 bittideClk bittideRst refClk refRst -< muUartBytesBittide
  ccUartBytes <-
    dcFifoDf d5 bittideClk bittideRst refClk refRst -< ccUartBytesBittide
  -- Stop UART multiplexing

  Fwd tOutputs <-
    Transceiver.transceiverPrbsNWb @Bittide @GthRx @Ext200 @Basic125 @GthTxS @GthRxS
      refClk
      refRst
      Transceiver.defConfig
      -< (transceiverWb, gths, Fwd (bundle switchDataOut))

  ( Fwd callistoResult
    , Fwd switchDataOut
    , sync
    , muUartBytesBittide
    , ccUartBytesBittide
    ) <-
    core
      (refClk, refRst)
      (bittideClk, bittideRst, enableGen)
      tOutputs.rxClocks
      (unsafeFromActiveLow <$> tOutputs.handshakesDone)
      -< ( muMm
         , ccMm
         , otherJtagBittide
         , Fwd 0
         , Fwd (pure maxBound)
         , Fwd linksSuitableForCc
         , Fwd tOutputs.rxDatas
         )

  let
    withRefClockResetEnable :: forall r. ((HiddenClockResetEnable Basic125) => r) -> r
    withRefClockResetEnable = withClockResetEnable refClk refRst enableGen

    bittideClk :: Clock Bittide
    bittideClk = tOutputs.txClock

    bittideRst :: Reset Bittide
    bittideRst = tOutputs.txReset

    linksSuitableForCc :: Signal Bittide (BitVector LinkCount)
    linksSuitableForCc = fmap pack (bundle tOutputs.handshakesDoneTx)

    frequencyAdjustments :: Signal Bittide (FINC, FDEC)
    frequencyAdjustments =
      delay bittideClk enableGen minBound
        $ speedChangeToStickyPins
          bittideClk
          bittideRst
          enableGen
          (SNat @Si539xHoldTime)
          callistoResult.maybeSpeedChange

  idC -< (spi, sync, uartTx, Fwd frequencyAdjustments)
