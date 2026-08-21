-- SPDX-FileCopyrightText: 2026 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

{- | Traffic generator for the decode demo's contention experiment (PLAN2):
a second, verified traffic flow on the same physical ring links as the decode
all-reduce, plus the machinery that lets two flows share a link fairly.

Each node's generator sends fixed-size bursts to its ring neighbor (one hop,
on the decode ring's forward direction) at a driver-programmed schedule of
slot offsets within a repeating period. The receiving neighbor checks every
payload word against the deterministic pattern, so the competing flow is
verified traffic: starvation or corruption is counted, never silent.

Two disciplines, matching the decode variants they contend with:

* __Scheduled__ (variant A_c): bursts are calendar slots, chosen by the
  driver to be disjoint from the decode windows on the same link. No headers,
  no credits, no arbitration — the calendar is the arbiter. A hardware
  collision counter proves the slots really were disjoint.

* __Credit__ (variant B_c): bursts are framed (header magic + sequence
  number) and flow-controlled by the same credit protocol as the decode
  link, with their own credit word and a configurable number of outstanding
  credits. Decode frames and generator bursts meet at 'linkPortArbiter', a
  work-conserving round-robin frame arbiter. The generator's queueing delay
  (slot time to grant) is histogrammed — that distribution and the decode
  latency histogram are the experiment's outputs.

The receive direction of a shared link carries both flows' frames, so
'rxStreamDemux' splits the raw tap into two streams by header magic,
masking each flow's payload from the other's idle-state header matcher
(decode payload words have arbitrary top bits and could otherwise alias a
header). The reverse direction carries both flows' single-word credits;
these are distinguished by magic alone and need no demux.
-}
module Bittide.TrafficGen (
  -- * Configuration and status
  TgMode (..),
  TrafficGenSettings (..),
  TrafficGenStatus (..),
  TgSlotCount,

  -- * The generator
  trafficGen,
  TrafficGenOut (..),

  -- * Link sharing
  rxStreamDemux,
  linkPortArbiter,
  ArbiterOut (..),

  -- * Wishbone device
  trafficGenConfig,

  -- * Word encodings (exposed for tests and the host driver)
  tgHeaderMagic,
  tgCreditMagic,
  mkTgHeaderWord,
  mkTgCreditWord,
  isTgHeaderWord,
  isTgCreditWord,
  mkTgPatternWord,
) where

import Clash.Prelude
import Protocols

import Clash.Class.BitPackC (ByteOrder)
import GHC.Stack (HasCallStack)
import Protocols.Experimental.Wishbone (Wishbone, WishboneMode (Standard))
import Protocols.MemoryMap (Access (ReadOnly, ReadWrite), Mm)
import Protocols.MemoryMap.Registers.WishboneStandard (
  RegisterConfig (access),
  deviceConfig,
  deviceWbI,
  registerConfig,
  registerWbI,
  registerWbI_,
  registerWbVecI,
  registerWbVecI_,
 )
import Protocols.MemoryMap.Registers.WishboneStandard.Internal (
  RegisterWb,
  RegisterWbConstraints,
 )

import Bittide.CreditLink (isHeaderWord)
import Bittide.DecodeProcessingElement (HistBins)

-- | Top 16 bits of a traffic-generator frame header word.
tgHeaderMagic :: BitVector 16
tgHeaderMagic = 0x769E

-- | Top 16 bits of a traffic-generator credit word.
tgCreditMagic :: BitVector 16
tgCreditMagic = 0x7C4E

mkTgHeaderWord :: Unsigned 32 -> BitVector 64
mkTgHeaderWord seqNr = tgHeaderMagic ++# (0 :: BitVector 16) ++# pack seqNr

mkTgCreditWord :: Unsigned 32 -> BitVector 64
mkTgCreditWord seqNr = tgCreditMagic ++# (0 :: BitVector 16) ++# pack seqNr

isTgHeaderWord :: BitVector 64 -> Bool
isTgHeaderWord w = slice d63 d48 w == tgHeaderMagic

isTgCreditWord :: BitVector 64 -> Bool
isTgCreditWord w = slice d63 d48 w == tgCreditMagic

tgWordSeq :: BitVector 64 -> Unsigned 32
tgWordSeq = unpack . slice d31 d0

{- | Payload word @i@ of burst @seqNr@. The top 16 bits stay zero, so a
pattern word can never alias a header magic on a shared link.
-}
mkTgPatternWord :: Unsigned 32 -> Unsigned 16 -> BitVector 64
mkTgPatternWord seqNr i = (0 :: BitVector 16) ++# pack seqNr ++# pack i

-- | Burst slots per period the schedule table holds.
type TgSlotCount = 8

data TgMode = TgOff | TgScheduled | TgCredit
  deriving (Generic, NFDataX, Eq, Show)

-- | Host-programmed settings.
data TrafficGenSettings = TrafficGenSettings
  { tgMode :: TgMode
  , tgFirstCycle :: Unsigned 64
  -- ^ Local counter value of the first period's start
  , tgRxFirstCycle :: Unsigned 64
  -- ^ Scheduled mode: when the neighbor's first period lands here
  , tgPeriod :: Unsigned 32
  -- ^ Slot-schedule repeat period, in cycles
  , tgBurstWords :: Unsigned 16
  -- ^ Payload words per burst
  , tgBurstsPerPeriod :: Unsigned 8
  -- ^ How many of 'tgOffsets' are active (from index 0)
  , tgOffsets :: Vec TgSlotCount (Unsigned 32)
  -- ^ Burst slot offsets within a period
  , tgBurstCount :: Unsigned 32
  -- ^ Total bursts to send, then stop
  , tgCreditMax :: Unsigned 8
  -- ^ Credit mode: outstanding-burst window
  , tgHistShift :: Unsigned 8
  -- ^ Queue-delay histogram bin width, log2 cycles
  }
  deriving (Generic, NFDataX)

-- | Hardware-maintained status, mirrored into read-only registers.
data TrafficGenStatus = TrafficGenStatus
  { tgSent :: Unsigned 32
  , tgReceived :: Unsigned 32
  , tgPatternErrors :: Unsigned 32
  -- ^ Payload mismatches plus unexpected burst sequence numbers
  , tgCollisions :: Unsigned 32
  -- ^ Cycles both flows drove the shared port (must stay 0)
  , tgCreditsReturned :: Unsigned 32
  , tgMinQueue :: Unsigned 32
  , tgMaxQueue :: Unsigned 32
  , tgLastQueue :: Unsigned 32
  -- ^ Queueing delay: burst slot time to port grant, in cycles
  , tgTxDone :: Bool
  , tgFirstBadExpected :: BitVector 64
  -- ^ Expected word at the first pattern mismatch (diagnostic)
  , tgFirstBadActual :: BitVector 64
  -- ^ Received word at the first pattern mismatch (diagnostic)
  , tgHist :: Vec HistBins (Unsigned 32)
  -- ^ Queueing-delay histogram, @2^tg_hist_shift@ cycles per bin
  }
  deriving (Generic, NFDataX)

-- | Everything the generator drives.
data TrafficGenOut = TrafficGenOut
  { tgTxWord :: Maybe (BitVector 64)
  -- ^ Word for the forward (downstream) link; 'Nothing' = not ours
  , tgPortReq :: Bool
  -- ^ Credit mode: a burst is due and a credit is held
  , tgTxActive :: Bool
  -- ^ The generator owns the port (streaming or about to)
  , tgCreditTx :: Maybe (BitVector 64)
  -- ^ Credit word for the reverse (upstream) link
  , tgStatus :: TrafficGenStatus
  -- ^ Status with 'tgCollisions' left at 0 (the arbiter owns that count)
  }
  deriving (Generic, NFDataX)

data TgTxFsm
  = TtIdle
  | -- | Credit mode: burst due at 'ttSlot', waiting for credit + port grant.
    TtWait
  | -- | Streaming; 'ttHdr' emits the header word first (credit mode only).
    TtStream {ttHdr :: Bool, ttWordIdx :: Unsigned 16, ttQueue :: Unsigned 32}
  | TtDone
  deriving (Generic, NFDataX)

data TgTxState = TgTxState
  { ttFsm :: TgTxFsm
  , ttSlot :: Unsigned 64
  -- ^ The current burst's scheduled slot cycle
  , ttPeriodBase :: Unsigned 64
  , ttBurstIdx :: Index TgSlotCount
  , ttSent :: Unsigned 32
  , ttCredits :: Unsigned 8
  , ttLastCredit :: BitVector 64
  , ttCreditsReturned :: Unsigned 32
  , ttMinQueue :: Unsigned 32
  , ttMaxQueue :: Unsigned 32
  , ttLastQueue :: Unsigned 32
  , ttHist :: Vec HistBins (Unsigned 32)
  }
  deriving (Generic, NFDataX)

data TgTxOut = TgTxOut
  { ttoWord :: Maybe (BitVector 64)
  , ttoReq :: Bool
  , ttoActive :: Bool
  , ttoSent :: Unsigned 32
  , ttoCreditsReturned :: Unsigned 32
  , ttoMinQueue :: Unsigned 32
  , ttoMaxQueue :: Unsigned 32
  , ttoLastQueue :: Unsigned 32
  , ttoDone :: Bool
  , ttoHist :: Vec HistBins (Unsigned 32)
  }
  deriving (Generic, NFDataX)

data TgRxFsm
  = TrIdle
  | {- | Receiving a burst; credit mode counts from a header, scheduled mode
    from the slot's start cycle.
    -}
    TrRecv {trSeqNr :: Unsigned 32, trWordIdx :: Unsigned 16}
  | {- | Driving the credit word upstream (credit mode); stalls while the
    decode link owns the reverse direction.
    -}
    TrGrant {trSeqDone :: Unsigned 32, trPulseIdx :: Unsigned 8}
  deriving (Generic, NFDataX)

data TgRxState = TgRxState
  { trFsm :: TgRxFsm
  , trPeriodBase :: Unsigned 64
  , trBurstIdx :: Index TgSlotCount
  , trReceived :: Unsigned 32
  , trErrors :: Unsigned 32
  , trExpectedSeq :: Unsigned 32
  , trFirstBadExpected :: BitVector 64
  , trFirstBadActual :: BitVector 64
  }
  deriving (Generic, NFDataX)

data TgRxOut = TgRxOut
  { troCreditWord :: Maybe (BitVector 64)
  , troReceived :: Unsigned 32
  , troErrors :: Unsigned 32
  , troFirstBadExpected :: BitVector 64
  , troFirstBadActual :: BitVector 64
  }
  deriving (Generic, NFDataX)

{- | The traffic generator: one transmit schedule toward the downstream
neighbor, one verifying receiver for the upstream neighbor's bursts.
-}
trafficGen ::
  forall dom.
  ( HasCallStack
  , HiddenClock dom
  ) =>
  -- | Reset (business-logic reset)
  Reset dom ->
  -- | Local counter
  Signal dom (Unsigned 64) ->
  Signal dom TrafficGenSettings ->
  -- | Arm pulse: clear status, restart the schedule
  Signal dom Bool ->
  {- | Receive stream carrying the upstream neighbor's bursts (demuxed in
  credit mode, the raw tap in scheduled mode)
  -}
  Signal dom (BitVector 64) ->
  -- | Raw reverse-direction tap (credit words from the downstream neighbor)
  Signal dom (BitVector 64) ->
  -- | Port grant from 'linkPortArbiter' (credit mode)
  Signal dom Bool ->
  -- | Reverse direction is owned by the decode link this cycle
  Signal dom Bool ->
  Signal dom TrafficGenOut
trafficGen rst localCounter settings armPulse rxStream creditRx portGrant creditTxBusy =
  mkOut <$> txWordR <*> txOut <*> rxOut
 where
  withCrst :: forall a. ((HiddenClockResetEnable dom) => a) -> a
  withCrst f = withClockResetEnable hasClock rst enableGen f

  -- One output register stage, matching the decode core's: the pattern-word
  -- construction must not reach the transceiver combinationally, and the
  -- shared convention means a burst emitted at slot @S@ occupies the wire
  -- exactly like a decode window fired at @S@.
  txWordR = withCrst $ register Nothing ((.ttoWord) <$> txOut)

  -- The grant is consumed registered: the arbiter's grant is a function of
  -- this machine's own request, so reading it back in the same cycle would
  -- form an evaluation cycle. The arbiter's activity grace covers the
  -- extra cycle.
  portGrantR = withCrst $ register False portGrant

  txOut =
    withCrst
      $ mealy
        goTgTx
        TgTxState
          { ttFsm = TtIdle
          , ttSlot = 0
          , ttPeriodBase = 0
          , ttBurstIdx = 0
          , ttSent = 0
          , ttCredits = 0
          , ttLastCredit = 0
          , ttCreditsReturned = 0
          , ttMinQueue = maxBound
          , ttMaxQueue = 0
          , ttLastQueue = 0
          , ttHist = repeat 0
          }
        (bundle (settings, armPulse, localCounter, creditRx, portGrantR))

  rxOut =
    withCrst
      $ mealy
        goTgRx
        TgRxState
          { trFsm = TrIdle
          , trPeriodBase = 0
          , trBurstIdx = 0
          , trReceived = 0
          , trErrors = 0
          , trExpectedSeq = 0
          , trFirstBadExpected = 0
          , trFirstBadActual = 0
          }
        (bundle (settings, armPulse, localCounter, rxStream, creditTxBusy))

  mkOut :: Maybe (BitVector 64) -> TgTxOut -> TgRxOut -> TrafficGenOut
  mkOut w t r =
    TrafficGenOut
      { tgTxWord = w
      , tgPortReq = t.ttoReq
      , tgTxActive = t.ttoActive
      , tgCreditTx = r.troCreditWord
      , tgStatus =
          TrafficGenStatus
            { tgSent = t.ttoSent
            , tgReceived = r.troReceived
            , tgPatternErrors = r.troErrors
            , tgCollisions = 0
            , tgCreditsReturned = t.ttoCreditsReturned
            , tgMinQueue = t.ttoMinQueue
            , tgMaxQueue = t.ttoMaxQueue
            , tgLastQueue = t.ttoLastQueue
            , tgTxDone = t.ttoDone
            , tgFirstBadExpected = r.troFirstBadExpected
            , tgFirstBadActual = r.troFirstBadActual
            , tgHist = t.ttoHist
            }
      }

  txIdleOut :: TgTxState -> TgTxOut
  txIdleOut s =
    TgTxOut
      { ttoWord = Nothing
      , ttoReq = False
      , ttoActive = False
      , ttoSent = s.ttSent
      , ttoCreditsReturned = s.ttCreditsReturned
      , ttoMinQueue = s.ttMinQueue
      , ttoMaxQueue = s.ttMaxQueue
      , ttoLastQueue = s.ttLastQueue
      , ttoDone = False
      , ttoHist = s.ttHist
      }

  -- Advance the slot pointer past a completed burst.
  advanceTx :: TrafficGenSettings -> TgTxState -> TgTxState
  advanceTx cfg s
    | wrap = s{ttBurstIdx = 0, ttPeriodBase = s.ttPeriodBase + resize cfg.tgPeriod, ttSent = sent'}
    | otherwise = s{ttBurstIdx = s.ttBurstIdx + 1, ttSent = sent'}
   where
    sent' = s.ttSent + 1
    wrap = fromIntegral s.ttBurstIdx + 1 >= max 1 cfg.tgBurstsPerPeriod

  foldQueue :: TrafficGenSettings -> Unsigned 32 -> TgTxState -> TgTxState
  foldQueue cfg q s =
    s
      { ttMinQueue = min s.ttMinQueue q
      , ttMaxQueue = max s.ttMaxQueue q
      , ttLastQueue = q
      , ttHist = replace bin (satAdd SatBound (s.ttHist !! bin) 1) s.ttHist
      }
   where
    shifted = q `shiftR` fromIntegral cfg.tgHistShift
    bin :: Index HistBins
    bin
      | shifted >= natToNum @(HistBins - 1) = maxBound
      | otherwise = unpack (resize (pack shifted))

  goTgTx ::
    TgTxState ->
    (TrafficGenSettings, Bool, Unsigned 64, BitVector 64, Bool) ->
    (TgTxState, TgTxOut)
  goTgTx s (cfg, arm, counter, creditIn, grant)
    | arm =
        ( TgTxState
            { ttFsm = TtIdle
            , ttSlot = 0
            , ttPeriodBase = cfg.tgFirstCycle
            , ttBurstIdx = 0
            , ttSent = 0
            , ttCredits = cfg.tgCreditMax
            , ttLastCredit = 0
            , ttCreditsReturned = 0
            , ttMinQueue = maxBound
            , ttMaxQueue = 0
            , ttLastQueue = 0
            , ttHist = repeat 0
            }
        , txIdleOut s
        )
    | cfg.tgMode == TgOff = (s, txIdleOut s)
    | otherwise = (sFinal, out)
   where
    -- Credit intake: dedup on word-value change, not presence, so the other
    -- flow's credit words interleaving on the same tap cannot fake edges.
    creditEdge = isTgCreditWord creditIn && creditIn /= s.ttLastCredit
    sCredit
      | creditEdge =
          s
            { ttCredits = satAdd SatBound s.ttCredits 1
            , ttCreditsReturned = s.ttCreditsReturned + 1
            , ttLastCredit = creditIn
            }
      | otherwise = s

    slotCycle = sCredit.ttPeriodBase + resize (cfg.tgOffsets !! sCredit.ttBurstIdx)
    slotDue = counter >= slotCycle && sCredit.ttSent < cfg.tgBurstCount
    allSent = sCredit.ttSent >= cfg.tgBurstCount

    (sFinal, out) = case sCredit.ttFsm of
      TtDone -> (sCredit, (txIdleOut sCredit){ttoDone = True})
      TtIdle
        | allSent -> (sCredit{ttFsm = TtDone}, (txIdleOut sCredit){ttoDone = True})
        | slotDue
        , TgScheduled <- cfg.tgMode ->
            -- The calendar is the arbiter: stream the first pattern word now.
            ( (foldQueue cfg 0 sCredit)
                { ttFsm = TtStream{ttHdr = False, ttWordIdx = 1, ttQueue = 0}
                , ttSlot = slotCycle
                }
            , (txIdleOut sCredit)
                { ttoWord = Just (mkTgPatternWord sCredit.ttSent 0)
                , ttoActive = True
                }
            )
        | slotDue -> (sCredit{ttFsm = TtWait, ttSlot = slotCycle}, txIdleOut sCredit)
        | otherwise -> (sCredit, txIdleOut sCredit)
      TtWait
        | grant && haveCredit ->
            -- Granted: the header goes out next cycle (the arbiter owns the
            -- port from now); record the queueing delay.
            ( sCredit
                { ttFsm =
                    TtStream
                      { ttHdr = True
                      , ttWordIdx = 0
                      , ttQueue = truncateB (counter - sCredit.ttSlot)
                      }
                , ttCredits = sCredit.ttCredits - 1
                }
            , (txIdleOut sCredit){ttoActive = True}
            )
        | otherwise -> (sCredit, (txIdleOut sCredit){ttoReq = haveCredit})
       where
        haveCredit = sCredit.ttCredits > 0
      TtStream{ttHdr, ttWordIdx, ttQueue}
        | ttHdr ->
            ( sCredit{ttFsm = TtStream{ttHdr = False, ttWordIdx = 0, ttQueue}}
            , (txIdleOut sCredit)
                { ttoWord = Just (mkTgHeaderWord sCredit.ttSent)
                , ttoActive = True
                }
            )
        | lastWord ->
            ( (advanceTx cfg (foldQueue cfg ttQueue sCredit)){ttFsm = TtIdle}
            , (txIdleOut sCredit){ttoWord = Just w, ttoActive = True}
            )
        | otherwise ->
            ( sCredit{ttFsm = TtStream{ttHdr = False, ttWordIdx = ttWordIdx + 1, ttQueue}}
            , (txIdleOut sCredit){ttoWord = Just w, ttoActive = True}
            )
       where
        w = mkTgPatternWord sCredit.ttSent ttWordIdx
        lastWord = ttWordIdx >= cfg.tgBurstWords - 1

  rxIdleOut :: TgRxState -> TgRxOut
  rxIdleOut s =
    TgRxOut
      { troCreditWord = Nothing
      , troReceived = s.trReceived
      , troErrors = s.trErrors
      , troFirstBadExpected = s.trFirstBadExpected
      , troFirstBadActual = s.trFirstBadActual
      }

  advanceRx :: TrafficGenSettings -> TgRxState -> TgRxState
  advanceRx cfg s
    | wrap = s{trBurstIdx = 0, trPeriodBase = s.trPeriodBase + resize cfg.tgPeriod}
    | otherwise = s{trBurstIdx = s.trBurstIdx + 1}
   where
    wrap = fromIntegral s.trBurstIdx + 1 >= max 1 cfg.tgBurstsPerPeriod

  goTgRx ::
    TgRxState ->
    (TrafficGenSettings, Bool, Unsigned 64, BitVector 64, Bool) ->
    (TgRxState, TgRxOut)
  goTgRx s (cfg, arm, counter, rx, crdBusy)
    | arm =
        ( TgRxState
            { trFsm = TrIdle
            , trPeriodBase = cfg.tgRxFirstCycle
            , trBurstIdx = 0
            , trReceived = 0
            , trErrors = 0
            , trExpectedSeq = 0
            , trFirstBadExpected = 0
            , trFirstBadActual = 0
            }
        , rxIdleOut s
        )
    | cfg.tgMode == TgOff = (s, rxIdleOut s)
    | otherwise = case s.trFsm of
        TrIdle
          | TgCredit <- cfg.tgMode
          , isTgHeaderWord rx ->
              let
                seqNr = tgWordSeq rx
                seqOk = seqNr == s.trExpectedSeq
               in
                ( s
                    { trFsm = TrRecv{trSeqNr = seqNr, trWordIdx = 0}
                    , trExpectedSeq = seqNr + 1
                    , trErrors = if seqOk then s.trErrors else s.trErrors + 1
                    }
                , rxIdleOut s
                )
          | TgScheduled <- cfg.tgMode
          , s.trReceived < cfg.tgBurstCount
          , counter >= slotCycle ->
              -- This cycle carries pattern word 0 of the scheduled burst.
              checkWord s{trFsm = TrRecv{trSeqNr = s.trReceived, trWordIdx = 0}} s.trReceived 0
          | otherwise -> (s, rxIdleOut s)
         where
          slotCycle = s.trPeriodBase + resize (cfg.tgOffsets !! s.trBurstIdx)
        TrRecv{trSeqNr, trWordIdx} -> checkWord s trSeqNr trWordIdx
        TrGrant{trSeqDone, trPulseIdx}
          | crdBusy -> (s, (rxIdleOut s){troCreditWord = Just w})
          | lastPulse -> (s{trFsm = TrIdle}, (rxIdleOut s){troCreditWord = Just w})
          | otherwise ->
              ( s{trFsm = TrGrant{trSeqDone, trPulseIdx = trPulseIdx + 1}}
              , (rxIdleOut s){troCreditWord = Just w}
              )
         where
          w = mkTgCreditWord trSeqDone
          lastPulse = trPulseIdx + 1 >= 4
   where
    checkWord st0 seqNr wordIdx =
      let
        expectW = mkTgPatternWord seqNr wordIdx
        bad = rx /= expectW
        lastWord = wordIdx >= cfg.tgBurstWords - 1
        -- Latch the first mismatch: the received word's value identifies
        -- what leaked into the stream (diagnostic).
        st
          | bad && st0.trErrors == 0 =
              st0{trFirstBadExpected = expectW, trFirstBadActual = rx}
          | otherwise = st0
        errors' = if bad then st.trErrors + 1 else st.trErrors
        stDone = (advanceRx cfg st){trReceived = st.trReceived + 1, trErrors = errors'}
       in
        if lastWord
          then case cfg.tgMode of
            TgCredit ->
              (stDone{trFsm = TrGrant{trSeqDone = seqNr, trPulseIdx = 0}}, rxIdleOut st)
            _ -> (stDone{trFsm = TrIdle}, rxIdleOut st)
          else
            ( st{trFsm = TrRecv{trSeqNr = seqNr, trWordIdx = wordIdx + 1}, trErrors = errors'}
            , rxIdleOut st
            )

-- OPAQUE: separate Verilog module, so timing reports carry its name.
{-# OPAQUE trafficGen #-}

data DemuxState
  = DmIdle
  | DmDecode {dmRemaining :: Unsigned 16}
  | DmTg {dmRemaining :: Unsigned 16}
  deriving (Generic, NFDataX)

{- | Split a shared receive tap into a decode-frame stream and a
traffic-generator stream by header magic. Zero added latency: the header
word itself is routed combinationally. While one flow's frame streams, the
other flow sees zeros, so payload words with arbitrary top bits can never
alias the other flow's header magic. Idle words pass through to both (both
idle-state matchers ignore non-headers).
-}
rxStreamDemux ::
  forall dom.
  ( HasCallStack
  , HiddenClock dom
  ) =>
  Reset dom ->
  -- | Arm pulse (resets mid-frame state on re-arm)
  Signal dom Bool ->
  -- | (decode payload words per frame, generator payload words per burst)
  Signal dom (Unsigned 16, Unsigned 16) ->
  -- | Raw receive tap
  Signal dom (BitVector 64) ->
  -- | (decode stream, generator stream)
  Signal dom (BitVector 64, BitVector 64)
rxStreamDemux rst armPulse lens rx =
  withClockResetEnable hasClock rst enableGen
    $ mealy go DmIdle (bundle (armPulse, lens, rx))
 where
  go ::
    DemuxState ->
    (Bool, (Unsigned 16, Unsigned 16), BitVector 64) ->
    (DemuxState, (BitVector 64, BitVector 64))
  go s (arm, (decodeLen, tgLen), w)
    | arm = (DmIdle, (0, 0))
    | otherwise = case s of
        DmIdle
          | isHeaderWord w -> (DmDecode{dmRemaining = decodeLen}, (w, 0))
          | isTgHeaderWord w -> (DmTg{dmRemaining = tgLen}, (0, w))
          | otherwise -> (DmIdle, (w, w))
        DmDecode{dmRemaining}
          | dmRemaining <= 1 -> (DmIdle, (w, 0))
          | otherwise -> (DmDecode{dmRemaining = dmRemaining - 1}, (w, 0))
        DmTg{dmRemaining}
          | dmRemaining <= 1 -> (DmIdle, (0, w))
          | otherwise -> (DmTg{dmRemaining = dmRemaining - 1}, (0, w))
{-# OPAQUE rxStreamDemux #-}

data PortOwner = PortFree | PortDecode | PortTg
  deriving (Generic, NFDataX, Eq)

data ArbState = ArbState
  { arbOwner :: PortOwner
  , arbSeenActive :: Bool
  , arbTgNext :: Bool
  -- ^ Round-robin: the generator wins the next same-cycle tie
  , arbGrace :: Unsigned 3
  -- ^ Cycles left for a granted flow to become active before release
  , arbCollisionCnt :: Unsigned 32
  , arbPortFreeR :: Bool
  -- ^ Registered \"a cut-through reservation would succeed now\"
  }
  deriving (Generic, NFDataX)

data ArbiterOut = ArbiterOut
  { arbDecodeGrant :: Bool
  , arbTgGrant :: Bool
  , arbPortFree :: Bool
  -- ^ For the decode receiver's cut-through decision (registered view)
  , arbCollisions :: Unsigned 32
  }
  deriving (Generic, NFDataX)

{- | Work-conserving round-robin frame arbiter for one transmit port shared
by the decode credit link and the traffic generator.

Frames are granted whole: once a flow owns the port it streams its frame
contiguously (both protocols count payload positionally). A decode relay's
cut-through path claims the port by reservation ('reservePort', asserted the
cycle a header is detected) — a reservation is only ever asserted when the
registered 'arbPortFree' said the port was free, and it beats a same-cycle
generator grant, so the two can never overlap. When the port is busy, the
decode relay falls back to its store-and-forward buffer and competes through
the level request like any other sender. Ties alternate (round-robin).

Fairness notes for the experiment's credibility: the arbiter is
work-conserving (the port never idles while a sendable frame waits), grants
alternate on contention, and cut-through is preserved exactly when the port
is idle — the same opportunism a well-designed async fabric would implement.
-}
linkPortArbiter ::
  forall dom.
  ( HasCallStack
  , HiddenClock dom
  ) =>
  Reset dom ->
  -- | Arm pulse
  Signal dom Bool ->
  -- | Decode cut-through reservation (header cycle; stream starts next cycle)
  Signal dom Bool ->
  -- | Decode level request (store-and-forward or injector, credit in hand)
  Signal dom Bool ->
  -- | Generator level request (burst due, credit in hand)
  Signal dom Bool ->
  -- | Decode transmit active (registered)
  Signal dom Bool ->
  -- | Generator transmit active
  Signal dom Bool ->
  -- | (decode word valid, generator word valid) — for collision counting
  Signal dom (Bool, Bool) ->
  Signal dom ArbiterOut
linkPortArbiter rst armPulse ctReserve decodeReq tgReq decodeActive tgActive wordValids =
  withClockResetEnable hasClock rst enableGen
    $ mealy
      go
      ArbState
        { arbOwner = PortFree
        , arbSeenActive = False
        , arbTgNext = False
        , arbGrace = 0
        , arbCollisionCnt = 0
        , arbPortFreeR = True
        }
      (bundle (armPulse, ctReserve, decodeReq, tgReq, decodeActive, tgActive, wordValids))
 where
  go ::
    ArbState ->
    (Bool, Bool, Bool, Bool, Bool, Bool, (Bool, Bool)) ->
    (ArbState, ArbiterOut)
  go s (arm, reserve, dReq, tReq, dActive, tActive, (dValid, tValid))
    | arm =
        ( s
            { arbOwner = PortFree
            , arbSeenActive = False
            , arbGrace = 0
            , arbCollisionCnt = 0
            , arbPortFreeR = True
            }
        , out False False
        )
    | otherwise = (sNext{arbPortFreeR = portFreeNext, arbCollisionCnt = collisions'}, out dGrant tGrant)
   where
    collisions' = if dValid && tValid then s.arbCollisionCnt + 1 else s.arbCollisionCnt
    out dg tg =
      ArbiterOut
        { arbDecodeGrant = dg
        , arbTgGrant = tg
        , arbPortFree = s.arbPortFreeR
        , arbCollisions = s.arbCollisionCnt
        }

    ownerActive = case s.arbOwner of
      PortDecode -> dActive
      PortTg -> tActive
      PortFree -> False

    -- Release: the owner streamed and finished, or never showed up.
    released
      | s.arbOwner == PortFree = True
      | s.arbSeenActive && not ownerActive = True
      | not s.arbSeenActive && s.arbGrace == 0 = True
      | otherwise = False

    (dGrant, tGrant, ownerNext, seenNext, tgNextNext, graceNext)
      | not released =
          (False, False, s.arbOwner, s.arbSeenActive || ownerActive, s.arbTgNext, satPred SatZero s.arbGrace)
      -- A cut-through reservation wins the cycle outright; it can only be
      -- asserted when the registered port-free view was true.
      | reserve = (False, False, PortDecode, False, s.arbTgNext, maxBound)
      | dReq && tReq =
          if s.arbTgNext
            then (False, True, PortTg, False, False, maxBound)
            else (True, False, PortDecode, False, True, maxBound)
      | dReq = (True, False, PortDecode, False, s.arbTgNext, maxBound)
      | tReq = (False, True, PortTg, False, s.arbTgNext, maxBound)
      | otherwise = (False, False, PortFree, False, s.arbTgNext, 0)

    sNext =
      s
        { arbOwner = ownerNext
        , arbSeenActive = seenNext
        , arbTgNext = tgNextNext
        , arbGrace = graceNext
        }

    portFreeNext = ownerNext == PortFree && not reserve
{-# OPAQUE linkPortArbiter #-}

{- | The Wishbone configuration/status device (@TrafficGenConfig@). Schedule
and knobs are read-write; status mirrors 'TrafficGenStatus' (cleared by the
processing element's @arm@ pulse, which resets the hardware counters).
-}
trafficGenConfig ::
  forall dom addrW nBytes.
  ( HasCallStack
  , HiddenClockResetEnable dom
  , KnownNat addrW
  , KnownNat nBytes
  , 1 <= nBytes
  , ?byteOrder :: ByteOrder
  ) =>
  Circuit
    ( (ToConstBwd Mm, Wishbone dom 'Standard addrW nBytes)
    , "STATUS" ::: CSignal dom TrafficGenStatus
    )
    ("SETTINGS" ::: CSignal dom TrafficGenSettings)
trafficGenConfig = circuit $ \(bus, status) -> do
  [ wbMode
    , wbFirstCycle
    , wbRxFirstCycle
    , wbPeriod
    , wbBurstWords
    , wbBurstsPerPeriod
    , wbOffsets
    , wbBurstCount
    , wbCreditMax
    , wbHistShift
    , wbSent
    , wbReceived
    , wbPatternErrors
    , wbCollisions
    , wbCreditsReturned
    , wbMinQueue
    , wbMaxQueue
    , wbLastQueue
    , wbTxDone
    , wbFirstBadExpected
    , wbFirstBadActual
    , wbHist
    ] <-
    deviceWbI (deviceConfig "TrafficGenConfig") -< bus

  Fwd status' <- idC -< status

  Fwd mode <-
    rwReg "tg_mode" "0 = off, 1 = scheduled (calendar slots), 2 = credit." (0 :: Unsigned 8)
      -< wbMode
  Fwd firstCycle <-
    rwReg "tg_first_cycle" "Local counter value of the first period's start." (0 :: Unsigned 64)
      -< wbFirstCycle
  Fwd rxFirstCycle <-
    rwReg
      "tg_rx_first_cycle"
      "Scheduled mode: when the upstream neighbor's first period lands here."
      (0 :: Unsigned 64)
      -< wbRxFirstCycle
  Fwd period <-
    rwReg "tg_period" "Slot-schedule repeat period, in cycles." (0 :: Unsigned 32) -< wbPeriod
  Fwd burstWords <-
    rwReg "tg_burst_words" "Payload words per burst." (64 :: Unsigned 16) -< wbBurstWords
  Fwd burstsPerPeriod <-
    rwReg "tg_bursts_per_period" "Active entries of tg_offsets." (0 :: Unsigned 8)
      -< wbBurstsPerPeriod
  (Fwd offsets, _offsetsActivity) <-
    registerWbVecI
      (registerConfig "tg_offsets" "Burst slot offsets within a period."){access = ReadWrite}
      (0 :: Unsigned 32)
      -< (wbOffsets, Fwd (pure (repeat Nothing)))
  Fwd burstCount <-
    rwReg "tg_burst_count" "Total bursts to send, then stop." (0 :: Unsigned 32) -< wbBurstCount
  Fwd creditMax <-
    rwReg "tg_credit_max" "Credit mode: outstanding-burst window." (4 :: Unsigned 8) -< wbCreditMax
  Fwd histShift <-
    rwReg "tg_hist_shift" "Queue-delay histogram bin width, log2 cycles." (0 :: Unsigned 8)
      -< wbHistShift

  roReg "tg_sent" "Bursts sent this run." (0 :: Unsigned 32) ((.tgSent) <$> status') -< wbSent
  roReg "tg_received" "Bursts received and verified." (0 :: Unsigned 32) ((.tgReceived) <$> status')
    -< wbReceived
  roReg
    "tg_pattern_errors"
    "Payload mismatches plus unexpected sequence numbers."
    (0 :: Unsigned 32)
    ((.tgPatternErrors) <$> status')
    -< wbPatternErrors
  roReg
    "tg_collisions"
    "Cycles both flows drove the shared port (must stay 0)."
    (0 :: Unsigned 32)
    ((.tgCollisions) <$> status')
    -< wbCollisions
  roReg
    "tg_credits_returned"
    "Credit words accepted this run."
    (0 :: Unsigned 32)
    ((.tgCreditsReturned) <$> status')
    -< wbCreditsReturned
  roReg
    "tg_min_queue"
    "Minimum burst queueing delay (slot to grant), in cycles."
    (maxBound :: Unsigned 32)
    ((.tgMinQueue) <$> status')
    -< wbMinQueue
  roReg
    "tg_max_queue"
    "Maximum burst queueing delay, in cycles."
    (0 :: Unsigned 32)
    ((.tgMaxQueue) <$> status')
    -< wbMaxQueue
  roReg
    "tg_last_queue"
    "Most recent burst queueing delay, in cycles."
    (0 :: Unsigned 32)
    ((.tgLastQueue) <$> status')
    -< wbLastQueue
  roReg "tg_tx_done" "All bursts sent." False ((.tgTxDone) <$> status') -< wbTxDone
  roReg
    "tg_first_bad_expected"
    "Expected word at the first pattern mismatch (diagnostic)."
    (0 :: BitVector 64)
    ((.tgFirstBadExpected) <$> status')
    -< wbFirstBadExpected
  roReg
    "tg_first_bad_actual"
    "Received word at the first pattern mismatch (diagnostic)."
    (0 :: BitVector 64)
    ((.tgFirstBadActual) <$> status')
    -< wbFirstBadActual

  registerWbVecI_
    ( registerConfig
        "tg_hist"
        "Queue-delay histogram, 2^tg_hist_shift cycles per bin; clamps to the edge bins."
    )
      { access = ReadOnly
      }
    (0 :: Unsigned 32)
    -< (wbHist, Fwd (fmap Just <$> ((.tgHist) <$> status')))

  idC
    -< Fwd
      ( TrafficGenSettings
          <$> (decodeTgMode <$> mode)
          <*> firstCycle
          <*> rxFirstCycle
          <*> period
          <*> burstWords
          <*> burstsPerPeriod
          <*> offsets
          <*> burstCount
          <*> creditMax
          <*> histShift
      )
 where
  decodeTgMode :: Unsigned 8 -> TgMode
  decodeTgMode 0 = TgOff
  decodeTgMode 1 = TgScheduled
  decodeTgMode _ = TgCredit

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
