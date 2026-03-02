-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
module Bittide.Instances.Hitl.SoftUgnDemo.Core (InternalCpuCount, core) where

import Clash.Explicit.Prelude
import Clash.Prelude (HiddenClockResetEnable, hasClock, hasReset, withClockResetEnable, withEnable)
import Protocols

import Bittide.CaptureUgn (captureUgn)
import Bittide.ClockControl (SpeedChange)
import Bittide.ClockControl.CallistoSw (SwcccInternalBusses, callistoSwClockControlC)
import Bittide.DoubleBufferedRam (blockRamByteAddressableU, wbStorage)
import Bittide.ElasticBuffer (xilinxElasticBufferWb)
import Bittide.Instances.Domains (Basic125, Bittide, GthRx)
import Bittide.Instances.Hitl.Setup (LinkCount)
import Bittide.Jtag (jtagChain)
import Bittide.ProcessingElement (
  PeConfig (..),
  PeInternalBusses,
  PrefixWidth,
  RemainingBusWidth,
  processingElement,
 )
import Bittide.Ringbuffer (receiveRingbufferWb, transmitRingbufferWb)
import Bittide.SharedTypes (Bitbone, BitboneMm)
import Bittide.Sync (Sync)
import Bittide.Wishbone (readDnaPortE2WbWorker, timeWb, uartBytes, uartInterfaceWb)
import Clash.Class.BitPackC (ByteOrder)
import Clash.Cores.Xilinx.Unisim.DnaPortE2 (readDnaPortE2, simDna2)
import Protocols.Extra
import Protocols.Idle (idleSink)
import Protocols.MemoryMap (Mm)
import Protocols.Wishbone.Extra (delayWishbone)
import VexRiscv (DumpVcd (..), Jtag)

import qualified Bittide.Cpus.Riscv32imc as Riscv32imc
import qualified Protocols.MemoryMap as Mm
import qualified Protocols.Vec as Vec

-- | The number of CPUs in 'core'
type InternalCpuCount = 3

type FifoSize = 5 -- = 2^5 = 32

{- Internal busses:
    - Instruction memory
    - Data memory
    - `timeWb`
    - DNA
    - UART
-}
type NmuInternalBusses = 3 + PeInternalBusses

{- Busses per link:
    - UGN component
    - Elastic buffer
-}
type PeripheralsPerLink = 2

{- External busses:
    - Transceivers
    - Callisto
-}
type NmuExternalBusses = 2 + (LinkCount * PeripheralsPerLink)
type NmuRemBusWidth = RemainingBusWidth (NmuExternalBusses + NmuInternalBusses)

muConfig ::
  ( KnownNat n
  , PrefixWidth (n + NmuInternalBusses) <= 30
  ) =>
  PeConfig (n + NmuInternalBusses)
muConfig =
  PeConfig
    { cpu = Riscv32imc.vexRiscv1
    , depthI = SNat @(Div (64 * 1024) 4)
    , depthD = SNat @(Div (64 * 1024) 4)
    , initI = Nothing
    , initD = Nothing
    , iBusTimeout = d0
    , dBusTimeout = d0
    , includeIlaWb = False
    }

ccConfig ::
  ( KnownNat n
  , PrefixWidth (n + SwcccInternalBusses) <= 30
  ) =>
  PeConfig (n + SwcccInternalBusses)
ccConfig =
  PeConfig
    { cpu = Riscv32imc.vexRiscv2
    , depthI = SNat @(Div (64 * 1024) 4)
    , depthD = SNat @(Div (64 * 1024) 4)
    , initI = Nothing
    , initD = Nothing
    , iBusTimeout = d0
    , dBusTimeout = d0
    , includeIlaWb = False
    }

gppeConfig ::
  ( KnownNat n
  , 2 <= n
  , PrefixWidth n <= 30
  ) =>
  PeConfig n
gppeConfig =
  PeConfig
    { cpu = Riscv32imc.vexRiscv3
    , depthI = SNat @(Div (64 * 1024) 4)
    , depthD = SNat @(Div (64 * 1024) 4)
    , initI = Nothing
    , initD = Nothing
    , iBusTimeout = d0
    , dBusTimeout = d0
    , includeIlaWb = False
    }

managementUnit ::
  forall dom.
  ( HiddenClockResetEnable dom
  , ?busByteOrder :: ByteOrder
  , ?regByteOrder :: ByteOrder
  , 1 <= DomainPeriod dom
  ) =>
  -- | External counter
  Signal dom (Unsigned 64) ->
  -- | DNA value
  Signal dom (Maybe (BitVector 96)) ->
  Circuit
    (ToConstBwd Mm.Mm, Jtag dom)
    ( Df dom (BitVector 8)
    , Vec
        NmuExternalBusses
        ( ToConstBwd Mm.Mm
        , Bitbone dom NmuRemBusWidth
        )
    )
managementUnit externalCounter maybeDna =
  circuit $ \(mm, jtag) -> do
    -- Core and interconnect
    allBusses <- processingElement NoDumpVcd muConfig -< (mm, jtag)
    ([timeBus, uartBus, dnaBus], restBusses) <- Vec.split -< allBusses

    -- Peripherals
    _localCounter <- timeWb (Just externalCounter) -< timeBus
    (uartOut, _uartStatus) <-
      uartInterfaceWb d16 d16 uartBytes -< (uartBus, Fwd (pure Nothing))
    readDnaPortE2WbWorker maybeDna -< dnaBus

    -- Output
    idC -< (uartOut, restBusses)

gppe ::
  ( HiddenClockResetEnable dom
  , 1 <= DomainPeriod dom
  , ?busByteOrder :: ByteOrder
  , ?regByteOrder :: ByteOrder
  ) =>
  -- | External counter
  Signal dom (Unsigned 64) ->
  -- | DNA value
  Signal dom (Maybe (BitVector 96)) ->
  Vec LinkCount (Signal dom (BitVector 64)) ->
  Circuit
    ( ToConstBwd Mm
    , Jtag dom
    )
    ( Vec LinkCount (CSignal dom (BitVector 64))
    , Df dom (BitVector 8)
    )
gppe externalCounter maybeDna linksIn = circuit $ \(mm, jtag) -> do
  -- Core and interconnect
  (rxBufferBusses, wbs0) <- Vec.split <| processingElement NoDumpVcd gppeConfig -< (mm, jtag)
  (txBufferBusses, wbs1) <- Vec.split -< wbs0
  [timeBus, uartBus, dnaBus] <- idC -< wbs1

  -- Scatter Gather units
  idleSink
    <| fmapC (receiveRingbufferWb rxPrim bufferDepth)
    <| Vec.zip
    -< (rxBufferBusses, Fwd linksIn)
  linksOut <- fmapC (transmitRingbufferWb txPrim bufferDepth) -< txBufferBusses

  -- Peripherals
  _cnt <- timeWb (Just externalCounter) -< timeBus
  (uart, _uartStatus) <- uartInterfaceWb d16 d16 uartBytes -< (uartBus, Fwd (pure Nothing))
  readDnaPortE2WbWorker maybeDna -< dnaBus

  -- Output
  idC -< (linksOut, uart)
 where
  bufferDepth = SNat @4000
  rxPrim ena = blockRamU hasClock hasReset ena NoClearOnReset bufferDepth
  txPrim ena = withEnable ena blockRamByteAddressableU

core ::
  ( ?busByteOrder :: ByteOrder
  , ?regByteOrder :: ByteOrder
  ) =>
  (Clock Basic125, Reset Basic125) ->
  (Clock Bittide, Reset Bittide, Enable Bittide) ->
  Vec LinkCount (Clock GthRx) ->
  Vec LinkCount (Reset GthRx) ->
  Circuit
    ( Vec InternalCpuCount (ToConstBwd Mm)
    , Jtag Bittide
    , "MASK" ::: CSignal Bittide (BitVector LinkCount)
    , "CC_SUITABLE" ::: CSignal Bittide (BitVector LinkCount)
    , "RXS" ::: Vec LinkCount (CSignal GthRx (Maybe (BitVector 64)))
    )
    ( CSignal Bittide (Maybe SpeedChange)
    , "LOCAL_COUNTER" ::: CSignal Bittide (Unsigned 64)
    , "TXS" ::: Vec LinkCount (CSignal Bittide (BitVector 64))
    , Sync Bittide Basic125
    , "UARTS" ::: Vec InternalCpuCount (Df Bittide (BitVector 8))
    , "MU_TRANSCEIVER" ::: (BitboneMm Bittide NmuRemBusWidth)
    )
core (refClk, refRst) (bitClk, bitRst, bitEna) rxClocks rxResets =
  circuit $ \(memoryMaps, jtag, mask, linksSuitableForCc, Fwd rxs0) -> do
    [muMm, ccMm, gppeMm] <- idC -< memoryMaps

    [muJtag, ccJtag, gppeJtag] <- jtagChain -< jtag

    let
      maybeDna = readDnaPortE2 bitClk bitRst bitEna simDna2
      localCounter = register bitClk bitRst bitEna 0 (localCounter + 1)

    -- Start management unit
    (muUartBytesBittide, muWbAll) <-
      withBittideClockResetEnable managementUnit localCounter maybeDna -< (muMm, muJtag)
    (ugnWbs, muWbs1) <- Vec.split -< muWbAll
    (ebWbs, [muTransceiverBus, muCallistoBus]) <- Vec.split -< muWbs1

    -- Stop management unit

    -- Start internal links
    (_relDatCount, _underflow, _overflow, Fwd rxs1) <-
      unzip4Vec
        <| ( Vec.vecCircuits
              $ xilinxElasticBufferWb
                bitClk
                bitRst
                (SNat @FifoSize)
                localCounter
              <$> rxClocks
              <*> rxs0
           )
        <| repeatC (fmapC $ withBittideClockResetEnable delayWishbone)
        -< ebWbs

    -- Use of `dflipflop` to add pipelining should be replaced by
    -- https://github.com/bittide/bittide-hardware/pull/1134
    Fwd rxs2 <-
      withBittideClockResetEnable
        $ Vec.vecCircuits ((captureUgn localCounter . dflipflop bitClk) <$> rxs1)
        -< ugnWbs
    -- Stop internal links

    -- Start general purpose processing element
    (Fwd txs, gppeUartBytesBittide) <-
      withBittideClockResetEnable gppe localCounter maybeDna rxs2 -< (gppeMm, gppeJtag)
    -- Stop general purpose processing element

    -- Start clock control
    ( sync
      , Fwd swCcOut0
      , [ ccUartBus
          , ccSampleMemoryBus
          ]
      ) <-
      withBittideClockResetEnable
        $ callistoSwClockControlC
          @LinkCount
          refClk
          refRst
          rxClocks
          rxResets
          NoDumpVcd
          ccConfig
        -< (ccMm, muCallistoBus, (ccJtag, mask, linksSuitableForCc))

    withBittideClockResetEnable
      (wbStorage "SampleMemory" (SNat @36_000))
      Nothing
      -< ccSampleMemoryBus

    (ccUartBytesBittide, _uartStatus) <-
      withBittideClockResetEnable
        $ uartInterfaceWb d16 d16 uartBytes
        -< (ccUartBus, Fwd (pure Nothing))

    let
      swCcOut1 =
        if clashSimulation
          then
            let
              -- Should all clock control steps be run in simulation?
              -- False means that clock control will always immediately be done.
              simulateCc = False
             in
              if simulateCc
                then swCcOut0
                else pure Nothing
          else swCcOut0
    -- Stop clock control

    -- Use of `dflipflop` to add pipelining should be replaced by
    -- https://github.com/bittide/bittide-hardware/pull/1134
    idC
      -< ( Fwd swCcOut1
         , Fwd localCounter
         , Fwd (dflipflop bitClk <$> txs)
         , sync
         , [muUartBytesBittide, ccUartBytesBittide, gppeUartBytesBittide]
         , muTransceiverBus
         )
 where
  withBittideClockResetEnable :: forall r. ((HiddenClockResetEnable Bittide) => r) -> r
  withBittideClockResetEnable = withClockResetEnable bitClk bitRst bitEna

uncurry4 ::
  (a -> b -> c -> d -> e) ->
  (a, b, c, d) ->
  e
uncurry4 fn (a, b, c, d) = fn a b c d

unzip4Vec ::
  Circuit (Vec n (a, b, c, d)) (Vec n a, Vec n b, Vec n c, Vec n d)
unzip4Vec = applyC unzip4 (uncurry4 zip4)
