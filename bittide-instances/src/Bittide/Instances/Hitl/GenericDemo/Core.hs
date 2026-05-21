-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{- | The core of a basic bittide system. It instantiates:

  * A clock control CPU
  * A management unit (CPU)
  * /n/ elastic buffers
  * /n/ handshake pipelines
  * An arbitrary processing element, given as an argument

A handshake pipeline for two links looks as follows:

>                 ┌────────────────┐    ┌───────────┐    ┌───────────┐    ┌───────────┐    ┌───────────┐
>                 │                │    │           │    │           │    │           │    │           │
> ───────────────►│ ELASTIC BUFFER ├─┬─►│           ├───►│  CAPTURE  ├───►│  RECEIVE  ├───►│           │
>                 │                │ │  │           │    │    UGN    │    │  RINGBUF  │    │           │
>                 └────────────────┘ │  │           │    │           │    │           │    │           │
>                                    │  │ HANDSHAKE │    ├───────────┤    ├───────────┤    │           │
>    ┌────────────────┐              │  │           │    │           │    │           │    │           │
>    │   ARBITRARY    │◄─────────────┘  │           │    │   SEND    │    │  TRANSMIT │    │           │
> ◄──┤   PROCESSING   │                 │           │    │    UGN    │    │  RINGBUF  │    │           │
>    │   ELEMENT      │◄────────────────┤           │◄───┤           │◄───┤           │◄───┤           │
>    └────────────────┘                 └───────────┘    └───────────┘    └───────────┘    │           │
>        ▲  ▲                                                                              │ MGTM UNIT │
>        │  │     ┌────────────────┐    ┌───────────┐    ┌───────────┐    ┌───────────┐    │           │
>        │  │     │                │    │           │    │           │    │           │    │           │
> ───────┼──┼────►│ ELASTIC BUFFER ├─┬─►│           ├───►│  CAPTURE  ├───►│  RECEIVE  ├───►│           │
>        │  │     │                │ │  │           │    │    UGN    │    │  RINGBUF  │    │           │
>        │  │     └────────────────┘ │  │           │    │           │    │           │    │           │
>        │  │                        │  │ HANDSHAKE │    ├───────────┤    ├───────────┤    │           │
>        │  │                        │  │           │    │           │    │           │    │           │
>        │  └────────────────────────┘  │           │    │   SEND    │    │  TRANSMIT │    │           │
>        │                              │           │    │    UGN    │    │  RINGBUF  │    │           │
>        └──────────────────────────────┤           │◄───┤           │◄───┤           │◄───│           │
>                                       └───────────┘    └───────────┘    └───────────┘    └───────────┘
-}
module Bittide.Instances.Hitl.GenericDemo.Core (
  InternalCpuCount,
  NmuExternalBusses,
  NmuInternalBusses,
  NmuRemBusWidth,
  UserCoreCircuit,
  core,
) where

import Clash.Explicit.Prelude
import Clash.Prelude (
  HiddenClock,
  HiddenClockResetEnable,
  HiddenReset,
  withClock,
  withClockResetEnable,
  withReset,
 )
import Protocols

import Bittide.CaptureUgn (captureUgn, sendUgn)
import Bittide.ClockControl (SpeedChange)
import Bittide.ClockControl.CallistoSw (SwcccInternalBusses, callistoSwClockControlC)
import Bittide.DoubleBufferedRam (wbStorage)
import Bittide.ElasticBuffer (fromData, xilinxElasticBufferWb)
import Bittide.Extra.Maybe (toMaybe)
import Bittide.Handshake (handshakesWb)
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
import Bittide.RingBuffer (receiveRingBuffer, transmitRingBuffer)
import Bittide.SharedTypes (Bitbone, BitboneMm)
import Bittide.Sync (Sync)
import Bittide.Wishbone (readDnaPortE2WbWorker, timeWb, uartBytes, uartInterfaceWb)
import Clash.Class.BitPackC (ByteOrder)
import Clash.Cores.Xilinx (withXilinx)
import Clash.Cores.Xilinx.BlockRam (tdpbram)
import Clash.Cores.Xilinx.Unisim.DnaPortE2 (readDnaPortE2, simDna2)
import Clash.Functor.Extra ((<<$>>), (<<*>>))
import Protocols.Df.Extra (tdpbramRamOp)
import Protocols.Extra
import Protocols.Idle (idleSink)
import Protocols.MemoryMap (Mm)
import Protocols.Wishbone.Extra (delayWishbone)
import VexRiscv (DumpVcd (..), Jtag)

import qualified Bittide.Cpus.Riscv32imc as Riscv32imc
import qualified Bittide.Handshake as Handshake
import qualified Protocols.MemoryMap as Mm
import qualified Protocols.Vec as Vec

-- | The number of CPUs in 'core'
type InternalCpuCount = 2

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
    - Handshakes
    - <userCoreBusses for the demo's user-core circuit>
-}
type NmuExternalBusses userCoreBusses = 3 + userCoreBusses + (LinkCount * PeripheralsPerLink)
type NmuRemBusWidth userCoreBusses =
  RemainingBusWidth (NmuExternalBusses userCoreBusses + NmuInternalBusses)

{- | Post-handshake stage provided by the demo. Given the Bittide-domain
clock/reset/enable, a free-running local counter and the FPGA DNA, returns
a circuit that receives any extra MU busses the demo needs, a raw
elastic-buffer tap (pre-handshake) per link, and the handshake's TX output
per link, and produces the TX wire that goes to the GTH.

For the soft-UGN demo this circuit forwards the handshake output verbatim
(see 'Bittide.Instances.Hitl.SoftUgnDemo.UserCore'). For the wire demo this
circuit holds the programmable mux and wire-demo processing element (see
'Bittide.Instances.Hitl.WireDemo.UserCore').
-}
type UserCoreCircuit userCoreBusses muRemBusWidth =
  ( (?byteOrder :: ByteOrder) =>
    Clock Bittide ->
    Reset Bittide ->
    Enable Bittide ->
    Signal Bittide (Unsigned 64) ->
    Signal Bittide (Maybe (BitVector 96)) ->
    Circuit
      ( Vec userCoreBusses (ToConstBwd Mm, Bitbone Bittide muRemBusWidth)
      , "RXS2_RAW" ::: CSignal Bittide (Vec LinkCount (BitVector 64))
      , "HANDSHAKE_OUT" ::: CSignal Bittide (Vec LinkCount (BitVector 64))
      )
      ("GTH_TX" ::: CSignal Bittide (Vec LinkCount (BitVector 64)))
  )

muConfig ::
  ( KnownNat n
  , PrefixWidth (n + NmuInternalBusses) <= 30
  ) =>
  PeConfig (n + NmuInternalBusses)
muConfig =
  PeConfig
    { cpu = Riscv32imc.vexRiscv1
    , depthI = SNat @(Div (16 * 1024) 4) -- One RAMB18E2 is 16KB, this uses 1 of them.
    , depthD = SNat @(Div (16 * 1024) 4) -- One RAMB18E2 is 16KB, this uses 1 of them.
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
  forall dom userCoreBusses.
  ( HiddenClockResetEnable dom
  , ?byteOrder :: ByteOrder
  , 1 <= DomainPeriod dom
  , KnownNat userCoreBusses
  , PrefixWidth (NmuExternalBusses userCoreBusses + NmuInternalBusses) <= 30
  ) =>
  -- | External counter
  Signal dom (Unsigned 64) ->
  -- | DNA value
  Signal dom (Maybe (BitVector 96)) ->
  Circuit
    (ToConstBwd Mm.Mm, Jtag dom)
    ( Df dom (BitVector 8)
    , Vec
        (NmuExternalBusses userCoreBusses)
        ( ToConstBwd Mm.Mm
        , Bitbone dom (NmuRemBusWidth userCoreBusses)
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
  forall userCoreBusses ringBufferDepth.
  ( ?byteOrder :: ByteOrder
  , KnownNat userCoreBusses
  , KnownNat ringBufferDepth
  , 1 <= ringBufferDepth
  , PrefixWidth (NmuExternalBusses userCoreBusses + NmuInternalBusses) <= 30
  , 1 <= NmuRemBusWidth userCoreBusses
  ) =>
  SNat ringBufferDepth ->
  UserCoreCircuit userCoreBusses (NmuRemBusWidth userCoreBusses) ->
  (Clock Basic125, Reset Basic125) ->
  (Clock Bittide, Reset Bittide, Enable Bittide) ->
  Vec LinkCount (Clock GthRx) ->
  Vec LinkCount (Reset GthRx) ->
  Circuit
    ( Vec InternalCpuCount (ToConstBwd Mm)
    , Jtag Bittide
    , "MASK" ::: CSignal Bittide (BitVector LinkCount)
    , "CC_SUITABLE" ::: CSignal Bittide (BitVector LinkCount)
    , "RXS" ::: Vec LinkCount (CSignal GthRx (BitVector 64))
    )
    ( CSignal Bittide (Maybe SpeedChange)
    , "TXS" ::: Vec LinkCount (CSignal Bittide (BitVector 64))
    , Sync Bittide Basic125
    , "UARTS" ::: Vec InternalCpuCount (Df Bittide (BitVector 8))
    , "MU_TRANSCEIVER" ::: BitboneMm Bittide (NmuRemBusWidth userCoreBusses)
    )
core bufferDepth mkUserCore (refClk, refRst) (bitClk, bitRst, bitEna) rxClocks rxResets = withXilinx
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
    ([muTransceiverBus, muCallistoBus, muHandshakeBus], extraMuBusses) <- Vec.split -< muWbs4
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

    let
      rxs2 = dflipflop bitClk <$> rxs1
      rxs2Raw = fmap fromData <$> rxs2

    Fwd handshakesOut <-
      withBittideClockReset handshakesWb
        -< ( muHandshakeBus
           , Fwd
               ( Handshake.Inputs
                   { fromNeighbors = rxs2
                   , fromCores = txs1
                   }
               )
           )

    let
      -- TODO: Hardware UGN capture is currently mandatory, it shouldn't be.
      rxs3 = toMaybe <<$>> handshakesOut.toCoreDones <<*>> handshakesOut.toCores
      rxs4 = dflipflop bitClk <$> rxs3

      txs1 = withClock bitClk $ sendUgn localCounter <$> handshakesOut.fromCoreDones <*> txs0

    rxs5 <-
      withBittideClockResetEnable
        $ Vec.vecCircuits (captureUgn localCounter <$> rxs4)
        -< ugnWbs
    -- Stop internal links

    -- Start ringbuffers
    let
      rxPrim ena = blockRamU bitClk bitRst ena NoClearOnReset bufferDepth
      txPrim = tdpbramRamOp tdpbram bitClk bitClk

    idleSink
      <| fmapC (withBittideClockResetEnable receiveRingBuffer rxPrim bufferDepth)
      <| Vec.zip
      -< (rxBufferBusses, rxs5)
    Fwd txs0 <-
      fmapC (withBittideClockResetEnable $ transmitRingBuffer txPrim bufferDepth) -< txBufferBusses
    -- Stop ringbuffers

    -- Start user core: post-handshake stage drives the GTH-TX wire.
    Fwd txsOut <-
      mkUserCore bitClk bitRst bitEna localCounter maybeDna
        -< ( extraMuBusses
           , Fwd (bundle rxs2Raw)
           , Fwd (bundle handshakesOut.toNeighbors)
           )
    -- Stop user core

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
         , Fwd (unbundle txsOut)
         , sync
         , [muUartBytesBittide, ccUartBytesBittide]
         , muTransceiverBus
         )
 where
  withBittideClockResetEnable :: forall r. ((HiddenClockResetEnable Bittide) => r) -> r
  withBittideClockResetEnable = withClockResetEnable bitClk bitRst bitEna

  withBittideClockReset :: forall r. ((HiddenClock Bittide, HiddenReset Bittide) => r) -> r
  withBittideClockReset r = withClock bitClk $ withReset bitRst r

uncurry4 ::
  (a -> b -> c -> d -> e) ->
  (a, b, c, d) ->
  e
uncurry4 fn (a, b, c, d) = fn a b c d

unzip4Vec ::
  Circuit (Vec n (a, b, c, d)) (Vec n a, Vec n b, Vec n c, Vec n d)
unzip4Vec = applyC unzip4 (uncurry4 zip4)
