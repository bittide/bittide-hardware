-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE CPP #-}

{- | Shared bring-up for the wire and soft-UGN demos. The two demos share an
identical boot/UART/transceiver topology; they differ only in the ring-buffer
depth and the post-handshake user-core circuit they hand to
'Bittide.Instances.Hitl.GenericDemo.Core.core'.
-}
module Bittide.Instances.Hitl.GenericDemo.BringUp (
  InternalCpuCount,
  NmuRemBusWidth,
  UserCoreCircuit,
  bringUp,
) where

import Clash.Explicit.Prelude
import Clash.Prelude (HiddenClockResetEnable, withClockResetEnable)
import Protocols

import Bittide.BootPe (BootPeBusses, bootPe)
import Bittide.ClockControl
import Bittide.Df (asciiDebugMux)
import Bittide.Instances.Domains (
  Basic125,
  Bittide,
  Ext200,
  GthRx,
  GthRxS,
  GthTxS,
 )
import Bittide.Instances.Hitl.GenericDemo.Core (
  InternalCpuCount,
  NmuExternalBusses,
  NmuInternalBusses,
  NmuRemBusWidth,
  UserCoreCircuit,
  core,
 )
import Bittide.Instances.Hitl.Setup (LinkCount)
import Bittide.Jtag (jtagChain, unsafeJtagSynchronizer)
import Bittide.ProcessingElement (PeConfig (..), PrefixWidth)
import Bittide.SharedTypes (Byte, withLittleEndian)
import Bittide.Sync (Sync)
import Bittide.Wishbone (arbiterMm, extendAddressWidthWb, uartDf)
import Clash.Cores.Xilinx.DcFifo (dcFifoDf)
import Data.Char (ord)
import Protocols.Extra
import Protocols.MemoryMap (Mm)
import Protocols.Spi (Spi)
import Protocols.Wishbone.Extra (xpmCdcHandshakeWb)
import VexRiscv (Jtag)

import qualified Bittide.Cpus.Riscv32imc as Riscv32imc
import qualified Bittide.Transceiver as Transceiver
import qualified Bittide.Transceiver.Wishbone as Transceiver
import qualified Clash.Cores.Xilinx.Gth as Gth
import qualified Protocols.Vec as Vec

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
    , depthI = SNat @(Div (4 * 1024) 4)
    , depthD = SNat @(Div (16 * 1024) 4)
    , initI = Nothing
    , initD = Nothing
    , iBusTimeout = d0
    , dBusTimeout = d0
    , includeIlaWb = False
    }

bringUp ::
  forall userCoreBusses ringBufferDepth.
  ( KnownNat userCoreBusses
  , KnownNat ringBufferDepth
  , 1 <= ringBufferDepth
  , PrefixWidth (NmuExternalBusses userCoreBusses + NmuInternalBusses) <= 30
  , 1 <= NmuRemBusWidth userCoreBusses
  , NmuRemBusWidth userCoreBusses <= 27
  ) =>
  SNat ringBufferDepth ->
  UserCoreCircuit userCoreBusses (NmuRemBusWidth userCoreBusses) ->
  "REFCLK" ::: Clock Basic125 ->
  "TEST_RST" ::: Reset Basic125 ->
  Circuit
    ( Vec (InternalCpuCount + 1) (ToConstBwd Mm)
    , Jtag Basic125
    , Gth.Gths GthRx GthRxS Bittide GthTxS Ext200 LinkCount
    )
    ( Spi Basic125
    , Sync Bittide Basic125
    , "UART_TX" ::: CSignal Basic125 Bit
    , "FINC_FDEC" ::: CSignal Bittide (FINC, FDEC)
    )
bringUp bufferDepth mkUserCore refClk refRst =
  withLittleEndian $ circuit $ \(memoryMaps, jtag, gths) -> do
    ([bootMm], coreMemoryMaps) <- Vec.split -< memoryMaps

    [bootJtag, otherJtag] <- jtagChain -< jtag
    otherJtagBittide <- unsafeJtagSynchronizer refClk bittideClk -< otherJtag

    transceiverWb <- withRefClockResetEnable arbiterMm -< [muTransceiverWb, bootTransceiverWb]
    muTransceiverWb <-
      fmapC (extendAddressWidthWb <| xpmCdcHandshakeWb bittideClk bittideRst refClk refRst)
        -< muTransceiverWbBittide

    -- Start boot PE
    (bootUartBytes, _spiDone, spi, bootTransceiverWb) <-
      withRefClockResetEnable
        $ bootPe bootPeConfig
        -< (bootMm, bootJtag)
    -- Stop boot PE

    -- Start UART multiplexing
    uartTxBytes <-
      withRefClockResetEnable
        $ asciiDebugMux d1024 uartLabels
        <| Vec.append
        -< ([bootUartBytes], uartBytes)
    (_uartInBytes, uartTx) <- withRefClockResetEnable $ uartDf baud -< (uartTxBytes, Fwd 0)

    uartBytes <- fmapC $ dcFifoDf d5 bittideClk bittideRst refClk refRst -< uartBytesBittide
    -- Stop UART multiplexing

    Fwd tOutputs <-
      Transceiver.transceiverPrbsNWb @Bittide @GthRx @Ext200 @Basic125 @GthTxS @GthRxS
        refClk
        refRst
        Transceiver.defConfig
        -< (transceiverWb, gths, Fwd (bundle coreToTransceivers))

    ( Fwd speedChanges
      , Fwd coreToTransceivers
      , sync
      , uartBytesBittide
      , muTransceiverWbBittide
      ) <-
      core
        bufferDepth
        mkUserCore
        (refClk, refRst)
        (bittideClk, bittideRst, enableGen)
        tOutputs.rxClocks
        (unsafeFromActiveLow <$> tOutputs.rxDataInitDones)
        -< ( coreMemoryMaps
           , otherJtagBittide
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
      linksSuitableForCc = fmap pack (bundle tOutputs.txDataInitDones)

      frequencyAdjustments :: Signal Bittide (FINC, FDEC)
      frequencyAdjustments =
        delay bittideClk enableGen minBound
          $ speedChangeToStickyPins
            bittideClk
            bittideRst
            enableGen
            (SNat @Si539xHoldTime)
            speedChanges

    idC -< (spi, sync, uartTx, Fwd frequencyAdjustments)
