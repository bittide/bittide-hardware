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

import Bittide.BootPe (Si5345SclPeriod, defBootPeConfig)
import Bittide.ClockControl.Si5345I2c (si5345I2cWb)
import Bittide.Df (asciiDebugMux)
import Bittide.I2c (I2c, i2cControllerWb)
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
import Bittide.ProcessingElement (PrefixWidth, RemainingBusWidth, processingElement)
import Bittide.SharedTypes (BitboneMm, Byte, withLittleEndian)
import Bittide.Sync (Sync)
import Bittide.Wishbone (
  arbiterMm,
  extendAddressWidthWb,
  timeWb,
  uartDf,
  uartInterfaceWb,
 )
#ifdef SIM_BAUD_RATE
import Clash.Cores.Uart.Extra (MaxBaudRate)
#endif
import Clash.Class.BitPackC (ByteOrder)
import Clash.Cores.Xilinx.DcFifo (dcFifoDf)
import Data.Char (ord)
import GHC.Stack (HasCallStack)
import Protocols.Extra
import Protocols.MemoryMap (Mm)
import Protocols.Wishbone.Extra (xpmCdcHandshakeWb)
import VexRiscv (DumpVcd (NoDumpVcd), Jtag)

import qualified Bittide.Transceiver as Transceiver
import qualified Bittide.Transceiver.Wishbone as Transceiver
import qualified Bittide.Wishbone as Wishbone
import qualified Clash.Cores.Xilinx.Gth as Gth
import qualified Protocols.Vec as Vec

#ifdef SIM_BAUD_RATE
type Baud = MaxBaudRate Basic125
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

{- | Number of busses of the processing element of 'demoBootPe', including the
busses internal to the 'processingElement'.
-}
type DemoBootPeBusses = 7

{- | Boot PE of the demos. Like 'Bittide.BootPe.bootPe', except that the clock
control CPU shares the Si5345 driver. The boot CPU uses it to configure the clock
chip, after which the clock control CPU uses it for frequency adjustments. The two
never use it at the same time: the Bittide domain (and therefore the clock control
CPU) only runs once the boot CPU finished configuring.
-}
demoBootPe ::
  forall dom.
  ( HasCallStack
  , HiddenClockResetEnable dom
  , 1 <= DomainPeriod dom
  , ?byteOrder :: ByteOrder
  ) =>
  Circuit
    ( ToConstBwd Mm
    , Jtag dom
    , "CC_CLOCK_CHIP_BUS" ::: BitboneMm dom (RemainingBusWidth DemoBootPeBusses)
    )
    ( "UART_BYTES" ::: Df dom (BitVector 8)
    , "MAIN_I2C" ::: I2c dom
    , "FMC_I2C" ::: I2c dom
    , "TRANSCEIVER_BUS" ::: BitboneMm dom (RemainingBusWidth DemoBootPeBusses)
    )
demoBootPe = circuit $ \(mm, jtag, ccClockChipBus) -> do
  [timeBus, uartBus, clockChipBus, transceiverBus, mainI2cBus] <-
    processingElement NoDumpVcd (defBootPeConfig @DemoBootPeBusses) -< (mm, jtag)

  Fwd _localCounter <- timeWb Nothing -< timeBus
  (uartBytes, _uartStatus) <-
    uartInterfaceWb d16 d16 Wishbone.uartBytes -< (uartBus, Fwd (pure Nothing))

  -- The boot CPU sets VADJ over the main I2C bus of the KCU105, before it
  -- configures the clock chip. See @firmware-support/bittide-cpus/src/fmc_power.rs@.
  mainI2c <- i2cControllerWb (SNat @(Microseconds 10)) -< mainI2cBus

  sharedClockChipBus <- arbiterMm -< [clockChipBus, ccClockChipBus]
  -- A speed change must finish within one update of the clock control CPU
  -- (200 us), which needs SCL at 400 kHz.
  (_clockChipDone, fmcI2c) <-
    si5345I2cWb (SNat @Si5345SclPeriod) 0x69 -< sharedClockChipBus

  idC -< (uartBytes, mainI2c, fmcI2c, transceiverBus)

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
    ( "MAIN_I2C" ::: I2c Basic125
    , "FMC_I2C" ::: I2c Basic125
    , Sync Bittide Basic125
    , "UART_TX" ::: CSignal Basic125 Bit
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
    ccClockChipWb <-
      fmapC (extendAddressWidthWb <| xpmCdcHandshakeWb bittideClk bittideRst refClk refRst)
        -< ccClockChipWbBittide
    (bootUartBytes, mainI2c, fmcI2c, bootTransceiverWb) <-
      withRefClockResetEnable demoBootPe
        -< (bootMm, bootJtag, ccClockChipWb)
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

    ( Fwd coreToTransceivers
      , sync
      , uartBytesBittide
      , muTransceiverWbBittide
      , ccClockChipWbBittide
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

    idC -< (mainI2c, fmcI2c, sync, uartTx)
