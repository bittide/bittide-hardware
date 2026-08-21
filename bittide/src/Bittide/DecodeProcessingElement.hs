-- SPDX-FileCopyrightText: 2026 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

{- | Processing element for the decode demo: a streaming ring all-reduce with
the shape of LLM decoding (a serial loop of tokens, each token a sequence of
layers, each layer one all-reduce over the ring).

The all-reduce is a systolic two-lap ring. In lap 1 the injector streams
@local_pattern + i@ for @vector_words@ words and every relay adds its own
contribution flow-through (one register stage per hop). In lap 2 the summed
vector travels the ring once more so every node observes the grand total; the
injector sinks it and latches the token latency. Nodes verify checksums per
window: relays check the accumulating prefix sum in lap 1, everyone checks the
grand total in lap 2. Both expectations are host-computable from the per-node
patterns.

The datapath ('decodeReduceCore' + 'decodeSequencer') is transport-agnostic:
window starts arrive as 'FireInfo' events from a front-end. Two front-ends
exist: 'calendarFrontEnd' fires at pre-computed 'localCounter' values (the
scheduled variant), and "Bittide.CreditLink" fires on credit/header events
(the hardware-async variant).
-}
module Bittide.DecodeProcessingElement (
  -- * Fire/stream interface
  Lap (..),
  Role (..),
  FireInfo (..),
  StreamIn (..),
  StreamOut (..),
  LapResult (..),

  -- * Settings and status
  HistBins,
  PeMode (..),
  DecodePeSettings (..),
  DecodePeStatus (..),

  -- * Datapath
  decodeReduceCore,
  decodeSequencer,
  calendarFrontEnd,

  -- * Wishbone device
  decodePeConfig,
) where

import Clash.Prelude
import Protocols

import Clash.Class.BitPackC (ByteOrder)
import GHC.Stack (HasCallStack)
import Protocols.Experimental.Wishbone (Wishbone, WishboneMode (Standard))
import Protocols.MemoryMap (Access (ReadOnly, ReadWrite, WriteOnly), Mm)
import Protocols.MemoryMap.Registers.WishboneStandard (
  RegisterConfig (access),
  busActivityWrite,
  deviceConfig,
  deviceWbI,
  registerConfig,
  registerWbI,
  registerWbI_,
  registerWbVecI_,
 )
import Protocols.MemoryMap.Registers.WishboneStandard.Internal (
  RegisterWb,
  RegisterWbConstraints,
 )

import Data.Maybe (isJust)

-- | Which traversal of the ring a window belongs to.
data Lap = Lap1 | Lap2
  deriving (Eq, Show, Generic, NFDataX, BitPack)

{- | What a node does during a streaming window.

The injector runs @Inject@ (lap 1 start), @PassRelay@ (turnaround: forward the
returning lap-1 total into lap 2) and @Sink@ (receive the lap-2 total, latch
completion). Relays run @AddRelay@ (lap 1) and @PassRelay@ (lap 2).
-}
data Role = RoleInject | RoleAddRelay | RolePassRelay | RoleSink
  deriving (Eq, Show, Generic, NFDataX, BitPack)

-- | A window start event from a front-end.
data FireInfo = FireInfo
  { lap :: Lap
  , role :: Role
  }
  deriving (Eq, Show, Generic, NFDataX)

-- | Per-cycle input to 'decodeReduceCore'.
data StreamIn = StreamIn
  { fire :: Maybe FireInfo
  , rxWord :: BitVector 64
  }
  deriving (Generic, NFDataX)

{- | Per-cycle output of 'decodeReduceCore'. @txWord@ carries word /i/ of a
window one cycle after word /i/ was consumed (uniform one-register relay
stage, for all roles including 'RoleInject').
-}
data StreamOut = StreamOut
  { txWord :: Maybe (BitVector 64)
  , lapResult :: Maybe LapResult
  , busy :: Bool
  }
  deriving (Generic, NFDataX)

-- | Emitted one cycle after the last word of a window was consumed.
data LapResult = LapResult
  { role :: Role
  , checksum :: BitVector 64
  , endCycle :: Unsigned 64
  }
  deriving (Eq, Show, Generic, NFDataX)

{- | Number of histogram bins. Kept small: the Wishbone register machinery's
'Clash.Class.BitPackC.BitPackC' @Vec@ instance stops normalizing past ~20
elements (Clash inline limit); anything outside the bins is clamped to the
edges and covered exactly by @min_latency@/@max_latency@.
-}
type HistBins = 16

-- | Transport discipline selector.
data PeMode = ModeCalendar | ModeCredit
  deriving (Eq, Show, Generic, NFDataX)

-- | Host-written workload and schedule parameters.
data DecodePeSettings linkCount = DecodePeSettings
  { readLink :: Maybe (Index linkCount)
  , writeLink :: Maybe (Index linkCount)
  , isInjector :: Bool
  , mode :: PeMode
  , firstCycle :: Unsigned 64
  , lapOffset :: Unsigned 32
  , layerPeriod :: Unsigned 32
  , layersPerToken :: Unsigned 16
  , tokenPeriod :: Unsigned 32
  , tokenCount :: Unsigned 32
  , vectorWords :: Unsigned 16
  , computeCycles :: Unsigned 32
  , localPattern :: BitVector 64
  , expectedWindowA :: BitVector 64
  , expectedWindowB :: BitVector 64
  , histBase :: Unsigned 32
  , histShift :: Unsigned 8
  -- ^ Histogram bin width, log2 cycles
  }
  deriving (Generic, NFDataX)

-- | Hardware-maintained status, mirrored into read-only registers.
data DecodePeStatus = DecodePeStatus
  { tokensDone :: Unsigned 32
  , checksumFailCount :: Unsigned 32
  , firstFailCycle :: Unsigned 64
  , firstFailChecksum :: BitVector 64
  -- ^ The observed checksum of the first failing window (diagnostic)
  , minLatency :: Unsigned 32
  , maxLatency :: Unsigned 32
  , lastLatency :: Unsigned 32
  , done :: Bool
  , hist :: Vec HistBins (Unsigned 32)
  {- ^ Latency histogram, one cycle per bin from @hist_base@; out-of-range
  samples clamp to the edge bins. Maintained here (and mirrored into a
  read-only register) so the histogram state lives with the statistics.
  -}
  }
  deriving (Generic, NFDataX)

emptyStatus :: DecodePeStatus
emptyStatus =
  DecodePeStatus
    { tokensDone = 0
    , checksumFailCount = 0
    , firstFailChecksum = 0
    , firstFailCycle = 0
    , minLatency = maxBound
    , maxLatency = 0
    , lastLatency = 0
    , done = False
    , hist = repeat 0
    }

data CoreState
  = CoreIdle
  | {- | Streaming a window. @pendingTx@ is the one-register relay stage;
    @contribution@ is @local_pattern + wordIdx@, carried incrementally so
    the transmit word needs only a single 64-bit adder per cycle.
    -}
    CoreStream
      { role :: Role
      , wordIdx :: Unsigned 16
      , contribution :: BitVector 64
      , checksum :: BitVector 64
      , pendingTx :: Maybe (BitVector 64)
      }
  | -- | All words consumed; emit the trailing register stage and the result.
    CoreFlush
      { role :: Role
      , checksum :: BitVector 64
      , pendingTx :: Maybe (BitVector 64)
      }
  deriving (Generic, NFDataX)

{- | The streaming window engine. On a fire at cycle /t/ it consumes @rxWord@
at cycles /t .. t+V-1/ (for receiving roles), emits @txWord@ at cycles
/t+1 .. t+V/ (for forwarding roles) and emits a 'LapResult' at /t+V/.

Ignores fires while busy (front-ends must not overlap windows; the calendar
guarantees this by schedule, the credit link by construction).
-}
decodeReduceCore ::
  forall dom linkCount.
  ( HasCallStack
  , HiddenClock dom
  , KnownNat linkCount
  ) =>
  -- | Reset (typically the business-logic reset from the programmable mux)
  Reset dom ->
  -- | Local counter
  Signal dom (Unsigned 64) ->
  -- | Settings
  Signal dom (DecodePeSettings linkCount) ->
  Circuit
    (CSignal dom StreamIn)
    (CSignal dom StreamOut)
decodeReduceCore rst localCounter settings = Circuit go
 where
  go (streamIn, _) = ((), streamOut)
   where
    streamOut =
      withClockResetEnable hasClock rst enableGen
        $ mealy goMealy CoreIdle (bundle (streamIn, settings, localCounter))

  -- The output is a function of state (and the free-running counter) ONLY —
  -- never of this cycle's input. Consumers (notably the credit link's
  -- transmit machine) read @txWord@ while the fire path runs through them,
  -- so an input-strict output here would form an evaluation cycle.
  goMealy ::
    CoreState ->
    (StreamIn, DecodePeSettings linkCount, Unsigned 64) ->
    (CoreState, StreamOut)
  goMealy s (streamIn, cfg, counter) = (next, out)
   where
    out = case s of
      CoreIdle -> StreamOut Nothing Nothing False
      CoreStream{pendingTx} -> StreamOut{txWord = pendingTx, lapResult = Nothing, busy = True}
      CoreFlush{role, checksum, pendingTx} ->
        StreamOut
          { txWord = pendingTx
          , lapResult = Just LapResult{role, checksum, endCycle = counter}
          , busy = True
          }
    next = case s of
      CoreIdle -> case streamIn.fire of
        Nothing -> CoreIdle
        Just info ->
          step
            CoreStream
              { role = info.role
              , wordIdx = 0
              , contribution = cfg.localPattern
              , checksum = 0
              , pendingTx = Nothing
              }
      CoreStream{} -> step s
      CoreFlush{} -> CoreIdle

    step :: CoreState -> CoreState
    step CoreStream{role, wordIdx, contribution, checksum} =
      let
        rx = streamIn.rxWord
        thisTx = case role of
          RoleInject -> Just contribution
          RoleAddRelay -> Just (rx + contribution)
          RolePassRelay -> Just rx
          RoleSink -> Nothing
        thisChecksum = case role of
          RoleInject -> checksum
          _ -> checksum + rx
        lastWord = wordIdx == cfg.vectorWords - 1
       in
        if lastWord
          then CoreFlush{role, checksum = thisChecksum, pendingTx = thisTx}
          else
            CoreStream
              { role
              , wordIdx = wordIdx + 1
              , contribution = contribution + 1
              , checksum = thisChecksum
              , pendingTx = thisTx
              }
    step other = other

data SeqState = SeqState
  { layerIdx :: Unsigned 16
  , tokenStart :: Unsigned 64
  , pendingSample :: Maybe (Unsigned 32)
  {- ^ Latency (or external) sample awaiting statistics/histogram folding —
  a pipeline stage that keeps the 64-bit subtraction and the 32-bit
  min/max/bin logic in separate cycles.
  -}
  , status :: DecodePeStatus
  }
  deriving (Generic, NFDataX)

{- | Consumes 'LapResult's and fire events: counts layers and tokens, verifies
window checksums, latches token latencies and drives the histogram increment.

Token latency = last-layer completion ('RoleSink' for the injector,
'RolePassRelay' for relays) minus the token's first window fire. An external
histogram sample (per-transfer credit round-trip time, in credit mode on
relays) can override the token-latency histogram feed.
-}
decodeSequencer ::
  forall dom linkCount.
  ( HasCallStack
  , HiddenClock dom
  , KnownNat linkCount
  ) =>
  -- | Reset (business-logic reset)
  Reset dom ->
  -- | Local counter
  Signal dom (Unsigned 64) ->
  -- | Settings
  Signal dom (DecodePeSettings linkCount) ->
  -- | Arm pulse: clear all status and start a run
  Signal dom Bool ->
  -- | Fire events (as seen by the core)
  Signal dom (Maybe FireInfo) ->
  -- | Lap results from the core
  Signal dom (Maybe LapResult) ->
  -- | External histogram samples (credit RTTs); 'Nothing' when unused
  Signal dom (Maybe (Unsigned 32)) ->
  Signal dom DecodePeStatus
decodeSequencer rst localCounter settings armPulse fires lapResults extSamples =
  withClockResetEnable hasClock rst enableGen
    $ mealy
      goSeq
      SeqState{layerIdx = 0, tokenStart = 0, pendingSample = Nothing, status = emptyStatus}
      (bundle (settings, armPulse, fires, lapResults, extSamples, localCounter))
 where
  goSeq ::
    SeqState ->
    ( DecodePeSettings linkCount
    , Bool
    , Maybe FireInfo
    , Maybe LapResult
    , Maybe (Unsigned 32)
    , Unsigned 64
    ) ->
    (SeqState, DecodePeStatus)
  goSeq s (cfg, arm, fire, lapResult, extSample, counter)
    | arm =
        ( SeqState{layerIdx = 0, tokenStart = 0, pendingSample = Nothing, status = emptyStatus}
        , emptyStatus
        )
    | otherwise = (s2{pendingSample = nextSample, status = statusOut}, statusOut)
   where
    -- Latch the token start at the first window fire of layer 0.
    tokenStartRole = if cfg.isInjector then RoleInject else RoleAddRelay
    s1 = case fire of
      Just info
        | info.role == tokenStartRole && s.layerIdx == 0 ->
            s{tokenStart = counter}
      _ -> s

    -- Checksum verification and layer/token accounting on window completion.
    s2 = case lapResult of
      Nothing -> s1
      Just result -> tokenAccounting result (verifyChecksum result s1)

    verifyChecksum result st = case result.role of
      RoleInject -> st
      RoleAddRelay -> check cfg.expectedWindowA
      _ -> check cfg.expectedWindowB
     where
      check expected
        | result.checksum == expected = st
        | otherwise =
            st
              { status =
                  st.status
                    { checksumFailCount = st.status.checksumFailCount + 1
                    , firstFailCycle =
                        if st.status.firstFailCycle == 0
                          then result.endCycle
                          else st.status.firstFailCycle
                    , firstFailChecksum =
                        if st.status.firstFailCycle == 0
                          then result.checksum
                          else st.status.firstFailChecksum
                    }
              }

    tokenCompletionRole = if cfg.isInjector then RoleSink else RolePassRelay
    tokenAccounting result st
      | result.role /= tokenCompletionRole = st
      | st.layerIdx + 1 < cfg.layersPerToken = st{layerIdx = st.layerIdx + 1}
      | otherwise =
          st
            { layerIdx = 0
            , status = st.status{tokensDone = st.status.tokensDone + 1}
            }

    -- Statistics pipeline, stage 1: only the 64-bit subtraction happens on
    -- the completion cycle; the sample is folded into min/max/last and the
    -- histogram one cycle later (stage 2, below). In credit mode, relays
    -- histogram per-transfer credit round-trip times (the external samples)
    -- INSTEAD of token spans, so the two distributions never mix.
    tokenLatencySample = case lapResult of
      Just result
        | result.role == tokenCompletionRole
        , s1.layerIdx + 1 >= cfg.layersPerToken ->
            Just (truncateB (result.endCycle - s1.tokenStart) :: Unsigned 32)
      _ -> Nothing
    nextSample
      | cfg.mode == ModeCredit && not cfg.isInjector = extSample
      | otherwise = tokenLatencySample

    -- Statistics pipeline, stage 2: fold the sample registered last cycle.
    folded = case s.pendingSample of
      Nothing -> s2.status
      Just latency ->
        let bin = toBin cfg.histBase cfg.histShift latency
         in s2.status
              { minLatency = min s2.status.minLatency latency
              , maxLatency = max s2.status.maxLatency latency
              , lastLatency = latency
              , hist =
                  replace bin (satAdd SatBound (s2.status.hist !! bin) 1) s2.status.hist
              }

    statusOut =
      folded
        { done = s2.status.tokensDone >= cfg.tokenCount && cfg.tokenCount /= 0
        }

  toBin :: Unsigned 32 -> Unsigned 8 -> Unsigned 32 -> Index HistBins
  toBin base shift latency
    | latency <= base = 0
    | offset >= natToNum @(HistBins - 1) = maxBound
    | otherwise = unpack (resize (pack offset))
   where
    offset = (latency - base) `shiftR` fromIntegral shift
-- OPAQUE: separate Verilog module, so timing reports carry its name.
{-# OPAQUE decodeSequencer #-}

data CalState = CalState
  { running :: Bool
  , windowIdx :: Unsigned 2
  , calLayerIdx :: Unsigned 16
  , tokenIdx :: Unsigned 32
  , nextWindow :: Unsigned 64
  , layerStart :: Unsigned 64
  , calTokenStart :: Unsigned 64
  }
  deriving (Generic, NFDataX)

{- | The scheduled (variant A) front-end: fires windows at pre-computed
absolute 'localCounter' values written by the host.

Per layer, the injector runs three windows at offsets @{0, lap_offset,
2*lap_offset}@ ('RoleInject', 'RolePassRelay' turnaround, 'RoleSink'); relays
run two at @{0, lap_offset}@ ('RoleAddRelay', 'RolePassRelay'). Layers stride
by @layer_period@ from the layer-0 window, tokens by @token_period@ from
@first_cycle@. Arming (re)starts the schedule from the current settings.
-}
calendarFrontEnd ::
  forall dom linkCount.
  ( HasCallStack
  , HiddenClock dom
  , KnownNat linkCount
  ) =>
  -- | Reset (business-logic reset)
  Reset dom ->
  -- | Local counter
  Signal dom (Unsigned 64) ->
  -- | Settings
  Signal dom (DecodePeSettings linkCount) ->
  -- | Arm pulse
  Signal dom Bool ->
  Signal dom (Maybe FireInfo)
calendarFrontEnd rst localCounter settings armPulse =
  withClockResetEnable hasClock rst enableGen
    $ mealy goCal idleState (bundle (settings, armPulse, localCounter))
 where
  idleState =
    CalState
      { running = False
      , windowIdx = 0
      , calLayerIdx = 0
      , tokenIdx = 0
      , nextWindow = maxBound
      , layerStart = maxBound
      , calTokenStart = maxBound
      }

  goCal ::
    CalState ->
    (DecodePeSettings linkCount, Bool, Unsigned 64) ->
    (CalState, Maybe FireInfo)
  goCal s (cfg, arm, counter)
    | arm =
        ( CalState
            { running = True
            , windowIdx = 0
            , calLayerIdx = 0
            , tokenIdx = 0
            , nextWindow = cfg.firstCycle
            , layerStart = cfg.firstCycle
            , calTokenStart = cfg.firstCycle
            }
        , Nothing
        )
    | s.running && counter == s.nextWindow = (advance s cfg, Just fireInfo)
    | otherwise = (s, Nothing)
   where
    fireInfo
      | cfg.isInjector = case s.windowIdx of
          0 -> FireInfo{lap = Lap1, role = RoleInject}
          1 -> FireInfo{lap = Lap2, role = RolePassRelay}
          _ -> FireInfo{lap = Lap2, role = RoleSink}
      | otherwise = case s.windowIdx of
          0 -> FireInfo{lap = Lap1, role = RoleAddRelay}
          _ -> FireInfo{lap = Lap2, role = RolePassRelay}

  advance :: CalState -> DecodePeSettings linkCount -> CalState
  advance s cfg
    | s.windowIdx + 1 < windowsPerLayer =
        s
          { windowIdx = s.windowIdx + 1
          , nextWindow = s.nextWindow + widen cfg.lapOffset
          }
    | s.calLayerIdx + 1 < cfg.layersPerToken =
        s
          { windowIdx = 0
          , calLayerIdx = s.calLayerIdx + 1
          , nextWindow = s.layerStart + widen cfg.layerPeriod
          , layerStart = s.layerStart + widen cfg.layerPeriod
          }
    | s.tokenIdx + 1 < cfg.tokenCount =
        s
          { windowIdx = 0
          , calLayerIdx = 0
          , tokenIdx = s.tokenIdx + 1
          , nextWindow = s.calTokenStart + widen cfg.tokenPeriod
          , layerStart = s.calTokenStart + widen cfg.tokenPeriod
          , calTokenStart = s.calTokenStart + widen cfg.tokenPeriod
          }
    | otherwise = s{running = False, nextWindow = maxBound}
   where
    windowsPerLayer :: Unsigned 2
    windowsPerLayer = if cfg.isInjector then 3 else 2

  widen :: Unsigned 32 -> Unsigned 64
  widen = resize
-- OPAQUE: separate Verilog module, so timing reports carry its name.
{-# OPAQUE calendarFrontEnd #-}

{- | The Wishbone configuration/status device (@DecodePeConfig@). Exposes the
'DecodePeSettings' fields as read-write registers, mirrors 'DecodePeStatus'
into read-only registers, maintains the token-latency histogram (one cycle
per bin from @hist_base@; out-of-range samples clamp to the edge bins)
and derives the arm pulse from writes to the write-only @arm@ register. The
arm pulse clears all status, including the histogram.
-}
decodePeConfig ::
  forall dom addrW nBytes linkCount.
  ( HasCallStack
  , HiddenClockResetEnable dom
  , KnownNat addrW
  , KnownNat nBytes
  , 1 <= nBytes
  , KnownNat linkCount
  , 1 <= linkCount
  , ?byteOrder :: ByteOrder
  ) =>
  Circuit
    ( (ToConstBwd Mm, Wishbone dom 'Standard addrW nBytes)
    , "STATUS" ::: CSignal dom DecodePeStatus
    )
    ( "SETTINGS" ::: CSignal dom (DecodePeSettings linkCount)
    , "ARM" ::: CSignal dom Bool
    )
decodePeConfig = circuit $ \(bus, status) -> do
  [ wbReadLink
    , wbWriteLink
    , wbIsInjector
    , wbMode
    , wbFirstCycle
    , wbLapOffset
    , wbLayerPeriod
    , wbLayersPerToken
    , wbTokenPeriod
    , wbTokenCount
    , wbVectorWords
    , wbComputeCycles
    , wbLocalPattern
    , wbExpectedWindowA
    , wbExpectedWindowB
    , wbHistBase
    , wbHistShift
    , wbArm
    , wbTokensDone
    , wbChecksumFailCount
    , wbFirstFailCycle
    , wbFirstFailChecksum
    , wbMinLatency
    , wbMaxLatency
    , wbLastLatency
    , wbDone
    , wbHist
    ] <-
    deviceWbI (deviceConfig "DecodePeConfig") -< bus

  Fwd status' <- idC -< status

  Fwd readLink <- rwReg "read_link" "Ring upstream link index." Nothing -< wbReadLink
  Fwd writeLink <- rwReg "write_link" "Ring downstream link index." Nothing -< wbWriteLink
  Fwd isInjector <- rwReg "is_injector" "This node injects and sinks tokens." False -< wbIsInjector
  Fwd modeRaw <-
    rwReg "mode" "Transport: 0 = calendar (A), 1 = credit (B)." (0 :: Unsigned 8) -< wbMode
  Fwd firstCycle <-
    rwReg
      "first_cycle"
      "Absolute local counter value of the first window (A) or the injector launch gate (B)."
      (maxBound :: Unsigned 64)
      -< wbFirstCycle
  Fwd lapOffset <-
    rwReg "lap_offset" "Local cycles between a node's lap-1 and lap-2 windows." (0 :: Unsigned 32)
      -< wbLapOffset
  Fwd layerPeriod <-
    rwReg "layer_period" "Local cycles between consecutive layers of a token." (0 :: Unsigned 32)
      -< wbLayerPeriod
  Fwd layersPerToken <-
    rwReg "layers_per_token" "Layers (all-reduces) per token." (1 :: Unsigned 16) -< wbLayersPerToken
  Fwd tokenPeriod <-
    rwReg "token_period" "Local cycles between consecutive tokens." (0 :: Unsigned 32) -< wbTokenPeriod
  Fwd tokenCount <-
    rwReg "token_count" "Tokens to run; a run is done when tokens_done reaches this." (0 :: Unsigned 32)
      -< wbTokenCount
  Fwd vectorWords <-
    rwReg "vector_words" "64-bit words per all-reduce vector." (64 :: Unsigned 16) -< wbVectorWords
  Fwd computeCycles <-
    rwReg
      "compute_cycles"
      "Emulated per-layer compute gap, credit mode only (calendar mode bakes it into layer_period)."
      (0 :: Unsigned 32)
      -< wbComputeCycles
  Fwd localPattern <-
    rwReg "local_pattern" "This node's contribution seed." (0 :: BitVector 64) -< wbLocalPattern
  Fwd expectedWindowA <-
    rwReg
      "expected_window_a"
      "Expected checksum of incoming lap-1 windows (relays)."
      (0 :: BitVector 64)
      -< wbExpectedWindowA
  Fwd expectedWindowB <-
    rwReg
      "expected_window_b"
      "Expected checksum of incoming lap-2 windows (grand total)."
      (0 :: BitVector 64)
      -< wbExpectedWindowB
  Fwd histBase <-
    rwReg "hist_base" "Histogram bin 0 latency, in cycles." (0 :: Unsigned 32) -< wbHistBase
  Fwd histShift <-
    rwReg "hist_shift" "Histogram bin width, log2 cycles." (0 :: Unsigned 8) -< wbHistShift

  (_armValue, Fwd armActivity) <-
    registerWbI
      (registerConfig "arm" "Clear all status (incl. histogram) and start a run with the current settings.")
        { access = WriteOnly
        }
      False
      -< (wbArm, Fwd (pure Nothing))

  roReg "tokens_done" "Completed tokens this run." (0 :: Unsigned 32) ((.tokensDone) <$> status')
    -< wbTokensDone
  roReg
    "checksum_fail_count"
    "Windows whose checksum mismatched."
    (0 :: Unsigned 32)
    ((.checksumFailCount) <$> status')
    -< wbChecksumFailCount
  roReg
    "first_fail_cycle"
    "Local counter at the first checksum failure; 0 = none."
    (0 :: Unsigned 64)
    ((.firstFailCycle) <$> status')
    -< wbFirstFailCycle
  roReg
    "first_fail_checksum"
    "Observed checksum of the first failing window (diagnostic)."
    (0 :: BitVector 64)
    ((.firstFailChecksum) <$> status')
    -< wbFirstFailChecksum
  roReg
    "min_latency"
    "Minimum histogram sample this run, in cycles."
    (maxBound :: Unsigned 32)
    ((.minLatency) <$> status')
    -< wbMinLatency
  roReg
    "max_latency"
    "Maximum histogram sample this run, in cycles."
    (0 :: Unsigned 32)
    ((.maxLatency) <$> status')
    -< wbMaxLatency
  roReg
    "last_latency"
    "Most recent histogram sample, in cycles."
    (0 :: Unsigned 32)
    ((.lastLatency) <$> status')
    -< wbLastLatency
  roReg "done" "All tokens completed." False ((.done) <$> status') -< wbDone

  registerWbVecI_
    ( registerConfig
        "hist"
        "Latency histogram, one cycle per bin from hist_base; out-of-range samples clamp to the edge bins."
    )
      { access = ReadOnly
      }
    (0 :: Unsigned 32)
    -< (wbHist, Fwd (fmap Just <$> ((.hist) <$> status')))

  let
    armPulse = isJust . busActivityWrite <$> armActivity
    settings =
      DecodePeSettings
        <$> readLink
        <*> writeLink
        <*> isInjector
        <*> (decodeMode <$> modeRaw)
        <*> firstCycle
        <*> lapOffset
        <*> layerPeriod
        <*> layersPerToken
        <*> tokenPeriod
        <*> tokenCount
        <*> vectorWords
        <*> computeCycles
        <*> localPattern
        <*> expectedWindowA
        <*> expectedWindowB
        <*> histBase
        <*> histShift

  idC -< (Fwd settings, Fwd armPulse)
 where
  rwReg ::
    forall a.
    (RegisterWbConstraints a dom nBytes addrW) =>
    String ->
    String ->
    a ->
    Circuit
      (RegisterWb dom addrW nBytes)
      (CSignal dom a)
  rwReg name doc resetValue = circuit $ \wb -> do
    (value, _activity) <-
      registerWbI (registerConfig name doc){access = ReadWrite} resetValue
        -< (wb, Fwd (pure Nothing))
    idC -< value

  roReg ::
    forall a.
    (RegisterWbConstraints a dom nBytes addrW) =>
    String ->
    String ->
    a ->
    Signal dom a ->
    Circuit (RegisterWb dom addrW nBytes) ()
  roReg name doc resetValue value = circuit $ \wb -> do
    registerWbI_ (registerConfig name doc){access = ReadOnly} resetValue
      -< (wb, Fwd (Just <$> value))

  decodeMode :: Unsigned 8 -> PeMode
  decodeMode 0 = ModeCalendar
  decodeMode _ = ModeCredit
