-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE CPP #-}

module Bittide.Instances.Hitl.Dut.SwitchDemo (
  ccWhoAmID,
  memoryMapCc,
  memoryMapMu,
  memoryMapGppe,
  muWhoAmID,
  gppeWhoAmID,
  switchCircuit,
  whoAmIPrefix,
  ActiveCycles,
  CyclesPerWrite,
  GppeConfig,
  GroupCycles,
  MetacycleLength,
  Padding,
  WindowCycles,
) where

import Clash.Explicit.Prelude
import Clash.Prelude (HiddenClockResetEnable, withClockResetEnable)

import Bittide.Calendar (CalendarConfig (..), ValidEntry (..))
import Bittide.ClockControl.Callisto.Types (CallistoResult (..), Stability (..))
import Bittide.ClockControl.CallistoSw (
  SwControlCConfig (..),
  SwcccInternalBusses,
  callistoSwClockControlC,
 )
import Bittide.Df (asciiDebugMux)
import Bittide.DoubleBufferedRam
import Bittide.Instances.Domains (Basic125, Bittide, GthRx)
import Bittide.Instances.Hitl.Setup (FpgaCount, LinkCount)
import Bittide.Instances.Hitl.Utils.Driver
import Bittide.Jtag (jtagChain)
import Bittide.ProcessingElement (PeConfig (..))
import Bittide.ScatterGather (GatherConfig (..), ScatterConfig (..))
import Bittide.SharedTypes (Byte, withBittideByteOrder)
import Bittide.Switch (CalendarEntry)
import Bittide.Wishbone (uartBytes, uartDf, uartInterfaceWb)

import Clash.Class.BitPackC (ByteOrder)
import Clash.Cores.Xilinx.DcFifo (dcFifoDf)
import Clash.Sized.Extra (extendLsb0s)
import Data.Char (ord)
import GHC.Stack (HasCallStack)
import Protocols
import VexRiscv (DumpVcd (..), Jtag, JtagIn (..))

import qualified Bittide.Calculator as Calc
import qualified Bittide.Node as Node
import qualified Protocols.MemoryMap as MM

#ifdef SIM_BAUD_RATE
type Baud = MaxBaudRate Basic125
#else
type Baud = 921_600
#endif

baud :: SNat Baud
baud = SNat

type NumGppes = 1

type Padding = Calc.WindowCycles FpgaCount 31
type GppeConfig = Calc.DefaultGppeConfig FpgaCount Padding
type CyclesPerWrite = Calc.CalCyclesPerWrite GppeConfig
type GroupCycles = Calc.CalGroupCycles GppeConfig
type WindowCycles = Calc.CalWindowCycles GppeConfig
type ActiveCycles = Calc.CalActiveCycles GppeConfig
type MetacycleLength = Calc.CalMetacycleLength GppeConfig

whoAmIPrefix :: forall n m. (KnownNat n, KnownNat m, n ~ m + 3) => Unsigned n
whoAmIPrefix = extendLsb0s @3 @m (0b111 :: Unsigned 3)
ccWhoAmID :: BitVector 32
ccWhoAmID = $(makeWhoAmIDTH "swcc")
muWhoAmID :: BitVector 32
muWhoAmID = $(makeWhoAmIDTH "mgmt")
gppeWhoAmID :: BitVector 32
gppeWhoAmID = $(makeWhoAmIDTH "gppe")

{- FOURMOLU_DISABLE -} -- Fourmolu doesn't do well with tabular code
calConf ::
  CalendarConfig (Node.NmuRemBusWidth LinkCount NumGppes) (CalendarEntry (LinkCount + NumGppes + 1))
calConf =
  CalendarConfig
    { maxCalDepth = SNat @LinkCount
    {- The '@12' is so that the generated Rust code works. At time of writing,
    the generator makes two separate device-specific types for 'ValidEntry' since
    they have differing repetition bit widths. To fix this, all tests are being
    set to a width of 12.
    -}
    , repetitionBits = SNat @12

    -- Active calendar. It will broadcast the PE (node 1) data to all links. Other
    -- than that we cycle through the other nodes.
    , activeCalendar =
        (      ValidEntry (2 :> repeat 1) nRepetitions
            :> ValidEntry (3 :> repeat 1) nRepetitions
            :> ValidEntry (4 :> repeat 1) nRepetitions
            :> ValidEntry (5 :> repeat 1) nRepetitions
            :> ValidEntry (6 :> repeat 1) nRepetitions
            :> ValidEntry (7 :> repeat 1) nRepetitions
            :> ValidEntry (8 :> repeat 1) nRepetitions
            :> Nil
        )

    -- Don't care about inactive calendar:
    , inactiveCalendar = (ValidEntry (repeat 0) 0 :> Nil)
    }
 where
  -- We want enough time to read _number of FPGAs_ triplets
  nRepetitions = numConvert (maxBound :: Index (FpgaCount * 3))
{- FOURMOLU_ENABLE -}

memoryMapCc :: MM.MemoryMap
memoryMapCc = let (mm, _, _) = memoryMaps in mm

memoryMapMu :: MM.MemoryMap
memoryMapMu = let (_, mm, _) = memoryMaps in mm

memoryMapGppe :: MM.MemoryMap
memoryMapGppe = let (_, _, mm) = memoryMaps in mm

memoryMaps :: ("CC" ::: MM.MemoryMap, "MU" ::: MM.MemoryMap, "GPPE" ::: MM.MemoryMap)
memoryMaps = (ccMm, muMm, gppeMm)
 where
  SimOnly gppeMm = head gppeMms
  ((SimOnly ccMm, SimOnly muMm, gppeMms, _, _, _, _, _), _) =
    switchCircuitFn
      (
        ( ()
        , ()
        , repeat ()
        , pure (JtagIn 0 0 0)
        , pure maxBound
        , pure maxBound
        , repeat (pure Nothing)
        , pure low
        )
      ,
        ( pure ()
        , repeat (pure ())
        , pure ()
        , repeat (pure ())
        , repeat (pure ())
        , pure ()
        , pure ()
        , pure ()
        )
      )
  Circuit switchCircuitFn =
    withBittideByteOrder
      $ switchCircuit
        (clockGen, resetGen, enableGen)
        (clockGen, resetGen, enableGen)
        (repeat clockGen)
        (repeat resetGen)

nodeConfig :: Node.NodeConfig LinkCount NumGppes (LinkCount + 1)
nodeConfig =
  Node.NodeConfig
    { managementConfig = muConfig
    , calendarConfig = calConf
    , gppeConfigs = gppeConfig0 :> Nil
    }

muConfig :: Node.ManagementConfig LinkCount NumGppes
muConfig =
  Node.ManagementConfig
    { scatterConfig =
        ScatterConfig
          { memDepth = SNat @(LinkCount + NumGppes + 1)
          , calendarConfig =
              CalendarConfig
                { maxCalDepth = SNat @LinkCount
                , repetitionBits = SNat
                , activeCalendar = repeat @LinkCount (ValidEntry @_ @1 0 0)
                , inactiveCalendar = repeat @LinkCount (ValidEntry @_ @1 0 0)
                }
          }
    , scatterCalPrefix = 0b01011
    , scatterPrefix = 0b01100
    , gatherConfig =
        GatherConfig
          { memDepth = SNat @(LinkCount + NumGppes + 1)
          , calendarConfig =
              CalendarConfig
                { maxCalDepth = SNat @LinkCount
                , repetitionBits = SNat
                , activeCalendar = repeat @LinkCount (ValidEntry @_ @1 0 0)
                , inactiveCalendar = repeat @LinkCount (ValidEntry @_ @1 0 0)
                }
          }
    , gatherCalPrefix = 0b01101
    , gatherPrefix = 0b01110
    , timerPrefix = 0b01000
    , switchPrefix = 0b01001
    , externalPrefix = 0b01010
    , captureUgnPrefixes =
        0b00001
          :> 0b00010
          :> 0b00011
          :> 0b00100
          :> 0b00101
          :> 0b00110
          :> 0b00111
          :> Nil
    , peScatterGatherPrefixes = (0b01111, 0b10001) :> Nil
    , peConfig =
        PeConfig
          { prefixI = 0b10000
          , prefixD = 0b11000
          , initI = Undefined @(Div (64 * 1024) 4)
          , initD = Undefined @(Div (64 * 1024) 4)
          , iBusTimeout = d0
          , dBusTimeout = d0
          , includeIlaWb = False
          , whoAmID = muWhoAmID
          , whoAmIPrefix = whoAmIPrefix
          }
    , dumpVcd = NoDumpVcd
    }

gppeConfig0 :: Node.GppeConfig (Node.NmuRemBusWidth LinkCount NumGppes) (LinkCount + 1)
gppeConfig0 =
  Node.GppeConfig
    { scatterConfig =
        ScatterConfig
          { memDepth = SNat @(LinkCount + NumGppes + 1)
          , calendarConfig =
              CalendarConfig
                { maxCalDepth = SNat @LinkCount
                , repetitionBits = SNat
                , activeCalendar = repeat @LinkCount (ValidEntry @_ @1 0 0)
                , inactiveCalendar = repeat @LinkCount (ValidEntry @_ @1 0 0)
                }
          }
    , scatterPrefix = 0b011
    , gatherConfig =
        GatherConfig
          { memDepth = SNat @(LinkCount + NumGppes + 1)
          , calendarConfig =
              CalendarConfig
                { maxCalDepth = SNat @LinkCount
                , repetitionBits = SNat
                , activeCalendar = repeat @LinkCount (ValidEntry @_ @1 0 0)
                , inactiveCalendar = repeat @LinkCount (ValidEntry @_ @1 0 0)
                }
          }
    , gatherPrefix = 0b001
    , peConfig =
        PeConfig
          { prefixI = 0b100
          , prefixD = 0b010
          , initI = Undefined @(Div (32 * 1024) 4)
          , initD = Undefined @(Div (32 * 1024) 4)
          , iBusTimeout = d0
          , dBusTimeout = d0
          , includeIlaWb = False
          , whoAmID = gppeWhoAmID
          , whoAmIPrefix = whoAmIPrefix
          }
    , dnaPrefix = 0b101
    , uartPrefix = 0b000
    , dumpVcd = NoDumpVcd
    , metaPeConfigPrefix = 0b110
    , metaPeConfigBufferWidth = SNat
    }

ccConfig ::
  forall n.
  ( CLog 2 (n + SwcccInternalBusses) <= 30
  , 3 <= CLog 2 (n + SwcccInternalBusses)
  , KnownNat n
  ) =>
  SwControlCConfig n
ccConfig =
  SwControlCConfig
    { peConfig =
        PeConfig
          { prefixI = 0b1000
          , prefixD = 0b0100
          , initI = Undefined @(Div (64 * 1024) 4)
          , initD = Undefined @(Div (64 * 1024) 4)
          , iBusTimeout = d0
          , dBusTimeout = d0
          , includeIlaWb = False
          , whoAmID = ccWhoAmID
          , whoAmIPrefix =
              whoAmIPrefix @(CLog 2 (n + SwcccInternalBusses)) @(CLog 2 (n + SwcccInternalBusses) - 3)
          }
    , ccRegPrefix = 0b1100
    , timePrefix = 0b0110
    , freezePrefix = 0b0010
    , syncOutGeneratorPrefix = 0b0001
    , domainDiffsPrefix = 0b0011
    }

ccLabel :: Vec 2 Byte
ccLabel = fromIntegral (ord 'C') :> fromIntegral (ord 'C') :> Nil

muLabel :: Vec 2 Byte
muLabel = fromIntegral (ord 'M') :> fromIntegral (ord 'U') :> Nil

gppeLabel :: Vec 2 Byte
gppeLabel = fromIntegral (ord 'G') :> fromIntegral (ord '0') :> Nil

type OtherBittide = GthRx

switchCircuit ::
  ( HasCallStack
  , ?busByteOrder :: ByteOrder
  , ?regByteOrder :: ByteOrder
  ) =>
  (Clock Basic125, Reset Basic125, Enable Basic125) ->
  (Clock Bittide, Reset Bittide, Enable Bittide) ->
  Vec LinkCount (Clock OtherBittide) ->
  Vec LinkCount (Reset OtherBittide) ->
  Circuit
    ( MM.ConstBwd MM.MM
    , MM.ConstBwd MM.MM
    , Vec NumGppes (MM.ConstBwd MM.MM)
    , Jtag Bittide
    , CSignal Bittide (BitVector LinkCount)
    , CSignal Bittide (BitVector LinkCount)
    , Vec LinkCount (CSignal Bittide (Maybe (BitVector 64)))
    , CSignal Bittide Bit
    )
    ( CSignal Bittide (CallistoResult LinkCount)
    , Vec LinkCount (CSignal Bittide (BitVector 64))
    , CSignal Bittide (Unsigned 64)
    , Vec NumGppes (CSignal Bittide (BitVector 64))
    , Vec NumGppes (CSignal Bittide (BitVector 64))
    , CSignal Bittide (Vec (LinkCount + NumGppes + 1) (Index (LinkCount + NumGppes + 2)))
    , CSignal Basic125 Bit
    , CSignal Basic125 Bit
    )
switchCircuit (refClk, refRst, refEna) (bitClk, bitRst, bitEna) rxClocks rxResets =
  circuit $ \(ccMM, muMM, gppeMMs, jtag, mask, linksSuitableForCc, rxs, Fwd syncIn) -> do
    [nodeJtag, ccJtag] <- jtagChain -< jtag

    (muUartBytesBittide, _muUartStatus) <-
      defaultBittideClkRstEn
        $ uartInterfaceWb d16 d16 uartBytes
        -< (muUartBus, Fwd (pure Nothing))

    muUartBytes <-
      dcFifoDf d5 bitClk bitRst refClk refRst
        -< muUartBytesBittide

    (txs, lc, muUartBus, peIn, peOut, [peUart], ce) <-
      defaultBittideClkRstEn $ Node.node nodeConfig -< (muMM, gppeMMs, nodeJtag, rxs)

    (ccUartBytesBittide, _uartStatus) <-
      defaultBittideClkRstEn
        $ uartInterfaceWb d16 d16 uartBytes
        -< (ccUartBus, Fwd (pure Nothing))

    ccUartBytes <-
      dcFifoDf d5 bitClk bitRst refClk refRst
        -< ccUartBytesBittide

    gppeUartBytes <- dcFifoDf d5 bitClk bitRst refClk refRst -< peUart

    uartTxBytes <-
      defaultRefClkRstEn
        $ asciiDebugMux d1024 (ccLabel :> muLabel :> gppeLabel :> Nil)
        -< [ccUartBytes, muUartBytes, gppeUartBytes]
    (_uartInBytes, uartTx) <- defaultRefClkRstEn $ uartDf baud -< (uartTxBytes, Fwd 0)

    defaultBittideClkRstEn
      (wbStorage "SampleMemory")
      (Undefined @36_000 @(BitVector 32))
      -< ccSampleMemoryBus

    (syncOut, Fwd swCcOut0, [(ccUartPfx, ccUartBus), (ccSampleMemoryPfx, ccSampleMemoryBus)]) <-
      defaultBittideClkRstEn
        $ callistoSwClockControlC
          @LinkCount
          @Bittide
          @Basic125
          @OtherBittide
          refClk
          refRst
          rxClocks
          rxResets
          NoDumpVcd
          ccConfig
        -< (ccMM, (Fwd syncIn, ccJtag, mask, linksSuitableForCc))

    MM.constBwd 0b0000 -< ccUartPfx
    MM.constBwd 0b1111 -< ccSampleMemoryPfx

    let swCcOut1 =
          if clashSimulation
            then
              let
                -- Should all clock control steps be run in simulation?
                -- False means that clock control will always immediately be done.
                simulateCc = False
               in
                if simulateCc
                  then swCcOut0
                  else
                    pure
                      $ CallistoResult
                        { maybeSpeedChange = Nothing
                        , stability = repeat (Stability{stable = True, settled = True})
                        , allStable = True
                        , allSettled = True
                        }
            else swCcOut0

    idC -< (Fwd swCcOut1, txs, lc, peIn, peOut, ce, uartTx, syncOut)
 where
  defaultRefClkRstEn :: forall r. ((HiddenClockResetEnable Basic125) => r) -> r
  defaultRefClkRstEn = withClockResetEnable refClk refRst refEna
  defaultBittideClkRstEn :: forall r. ((HiddenClockResetEnable Bittide) => r) -> r
  defaultBittideClkRstEn = withClockResetEnable bitClk bitRst bitEna
