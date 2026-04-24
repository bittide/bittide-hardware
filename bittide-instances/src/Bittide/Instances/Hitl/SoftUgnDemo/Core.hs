-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{- | Demo for a bittide system which demonstrates we can capture UGNs in software without
communicating with a host PC. The accompanying driver function is still used program the
CPUs and checks that the software and hardware captured UGNs match.
-}
module Bittide.Instances.Hitl.SoftUgnDemo.Core (LinkCount, InternalCpuCount, core) where

import Clash.Explicit.Prelude
import Clash.Prelude (HiddenClockResetEnable, withClockResetEnable)
import Protocols

import Bittide.CaptureUgn (captureUgn)
import Bittide.ClockControl (SpeedChange)
import Bittide.ClockControl.CallistoSw (SwcccInternalBusses, callistoSwClockControlC)
import Bittide.DoubleBufferedRam (wbStorage)
import Bittide.ElasticBuffer (xilinxElasticBufferWb)
import Bittide.Instances.Domains (Basic125, Bittide, GthRx)
import Bittide.Jtag (jtagChain)
import Bittide.ProcessingElement (
  PeConfig (..),
  PeInternalBusses,
  PrefixWidth,
  RemainingBusWidth,
  processingElement,
 )
import Bittide.RingBuffer (receiveRingBuffer, transmitRingBuffer)
import Bittide.SharedTypes (Bitbone, BitboneMm)
import Bittide.Sync (Sync)
import Bittide.Wishbone (readDnaPortE2WbWorker, timeWb, uartBytes, uartInterfaceWb)
import Clash.Class.BitPackC (ByteOrder)
import Clash.Cores.Xilinx (withXilinx)
import Clash.Cores.Xilinx.BlockRam (tdpbram)
import Clash.Cores.Xilinx.Unisim.DnaPortE2 (readDnaPortE2, simDna2)
import Protocols.Df.Extra (tdpbramRamOp)
import Protocols.Extra
import Protocols.Idle (idleSink)
import Protocols.MemoryMap (Mm)
import Protocols.Wishbone.Extra (delayWishbone)
import VexRiscv (DumpVcd (..), Jtag)

import qualified Bittide.Cpus.Riscv32imc as Riscv32imc
import qualified Protocols.MemoryMap as Mm
import qualified Protocols.Vec as Vec

-- | The number of CPUs in 'core'
type InternalCpuCount = 2

type LinkCount = 3
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
    - Receive ringbuffer
    - Transmit ringbuffer
-}
type PeripheralsPerLink = 4

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
    , depthI = SNat @(Div (10 * 16 * 1024) 4) -- One RAMB18E2 is 16KB, this uses 10 of them.
    , depthD = SNat @(Div (10 * 16 * 1024) 4) -- One RAMB18E2 is 16KB, this uses 10 of them.
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
    , depthI = SNat @(Div (8 * 1024) 4)
    , depthD = SNat @(Div (16 * 1024) 4)
    , initI = Nothing
    , initD = Nothing
    , iBusTimeout = d0
    , dBusTimeout = d0
    , includeIlaWb = False
    }

managementUnit ::
  forall dom.
  ( HiddenClockResetEnable dom
  , ?byteOrder :: ByteOrder
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

core ::
  (?byteOrder :: ByteOrder) =>
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
core (refClk, refRst) (bitClk, bitRst, bitEna) rxClocks rxResets = withXilinx
  $ circuit
  $ \(memoryMaps, jtag, mask, linksSuitableForCc, Fwd rxs0) -> do
    [muMm, ccMm] <- idC -< memoryMaps

    [muJtag, ccJtag] <- jtagChain -< jtag

    let
      maybeDna = readDnaPortE2 bitClk bitRst bitEna simDna2
      localCounter = register bitClk bitRst bitEna 0 (localCounter + 1)

    -- Start management unit
    (muUartBytesBittide, muWbAll) <-
      withBittideClockResetEnable managementUnit localCounter maybeDna -< (muMm, muJtag)
    (ugnWbs, muWbs1) <- Vec.split -< muWbAll
    (ebWbs, muWbs2) <- Vec.split -< muWbs1
    (rxBufferBusses, muWbs3) <- Vec.split -< muWbs2
    (txBufferBusses, muWbs4) <- Vec.split -< muWbs3
    [muTransceiverBus, muCallistoBus] <- idC -< muWbs4
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
    rxs2 <-
      withBittideClockResetEnable
        $ Vec.vecCircuits ((captureUgn localCounter . dflipflop bitClk) <$> rxs1)
        -< ugnWbs
    -- Stop internal links

    -- Start ringbuffers
    let
      bufferDepth = (SNat @4000)
      rxPrim ena = blockRamU bitClk bitRst ena NoClearOnReset bufferDepth
      txPrim = tdpbramRamOp tdpbram bitClk bitClk

    idleSink
      <| fmapC (withBittideClockResetEnable receiveRingBuffer rxPrim bufferDepth)
      <| Vec.zip
      -< (rxBufferBusses, rxs2)
    Fwd txs <-
      fmapC (withBittideClockResetEnable $ transmitRingBuffer txPrim bufferDepth) -< txBufferBusses

    -- Stop ringbuffers

    -- Start clock control
    ( sync
      , Fwd swCcOut0
      , [ ccUartBus
          , ccSampleMemoryBus
          ]
      ) <-
      withBittideClockResetEnable
        $ callistoSwClockControlC @LinkCount
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
         , [muUartBytesBittide, ccUartBytesBittide]
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
