-- SPDX-FileCopyrightText: 2026 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- | Tests for the decode demo's contention machinery (PLAN2): the traffic
generator, the receive-stream demux and the shared-port arbiter, wired into
a full two-node loop exactly the way the demo user core wires them.

The load-bearing assertions:

* With the generator off, the shared-link node reproduces the base demo's
  numbers field for field — the 0%-duty regression gate.
* Scheduled + scheduled (variant A_c's shape): the decode latency is
  bit-identical to the quiet run, the generator's traffic verifies cleanly,
  and the hardware collision counter stays zero — the calendar interleave is
  collision-free by construction.
* Credit + credit (variant B_c's shape): both flows complete losslessly with
  exact credit accounting, and the decode latency shifts and spreads — the
  queueing the experiment exists to measure.
-}
module Tests.TrafficGen (tests) where

import Clash.Prelude

import Data.Maybe (fromMaybe, isJust)
import Protocols (toSignals)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.TH (testGroupGenerator)

import Bittide.CreditLink
import Bittide.DecodeProcessingElement
import Bittide.TrafficGen

import qualified Data.List as L

type ForwardDelay = 34

vectorWordsC :: Int
vectorWordsC = 4

layersC :: Int
layersC = 2

tokensC :: Int
tokensC = 3

patternInjector, patternRelay :: BitVector 64
patternInjector = 0x1000
patternRelay = 0x2000

expectedWindowAC :: BitVector 64
expectedWindowAC =
  fromIntegral vectorWordsC
    * patternInjector
    + fromIntegral (vectorWordsC * (vectorWordsC - 1) `div` 2)

expectedWindowBC :: BitVector 64
expectedWindowBC =
  fromIntegral vectorWordsC
    * (patternInjector + patternRelay)
    + fromIntegral (vectorWordsC * (vectorWordsC - 1))

transfersPerRun :: Unsigned 32
transfersPerRun = fromIntegral (2 * layersC * tokensC)

{- | The relay's calendar windows sit one hop (delay + one register) after
the injector's.
-}
hopC :: Unsigned 64
hopC = natToNum @ForwardDelay + 1

mkSettings :: PeMode -> Bool -> DecodePeSettings 2
mkSettings mode isInjector =
  DecodePeSettings
    { readLink = Just 0
    , writeLink = Just 1
    , isInjector
    , mode
    , firstCycle = if mode == ModeCalendar && not isInjector then 100 + hopC else 100
    , lapOffset = 2 * (natToNum @ForwardDelay + 1)
    , layerPeriod = 200
    , layersPerToken = fromIntegral layersC
    , -- A multiple of the generator's period, so the decode windows sit at
      -- the same in-period offsets in every layer of every token.
      tokenPeriod = 600
    , tokenCount = fromIntegral tokensC
    , vectorWords = fromIntegral vectorWordsC
    , computeCycles = 0
    , localPattern = if isInjector then patternInjector else patternRelay
    , expectedWindowA = expectedWindowAC
    , expectedWindowB = expectedWindowBC
    , histBase = 0
    , histShift = 0
    }

mkKnobs :: Bool -> CreditLinkKnobs
mkKnobs cutThrough =
  CreditLinkKnobs
    { creditTimeout = 1_000_000
    , creditPulseLen = 4
    , cutThrough
    }

tgOff :: TrafficGenSettings
tgOff =
  TrafficGenSettings
    { tgMode = TgOff
    , tgFirstCycle = 0
    , tgRxFirstCycle = 0
    , tgPeriod = 0
    , tgBurstWords = 4
    , tgBurstsPerPeriod = 0
    , tgOffsets = repeat 0
    , tgBurstCount = 0
    , tgCreditMax = 4
    , tgHistShift = 0
    }

{- | Scheduled generator load: bursts in the decode calendar's per-link idle
gap. Decode occupies its write link around offsets 0 (lap 1) and 70+ (lap 2)
of each 200-cycle layer period, so slots from 110 are collision-free. Each
node's schedule is phased with its own decode calendar; the receive schedule
is the sender's, one wire delay later (the generator has no output
register).
-}
tgScheduled :: Bool -> TrafficGenSettings
tgScheduled isInjector =
  tgOff
    { tgMode = TgScheduled
    , tgFirstCycle = txFirst
    , tgRxFirstCycle = peerTxFirst + natToNum @ForwardDelay
    , tgPeriod = 200
    , tgBurstsPerPeriod = 3
    , tgOffsets = 110 :> 130 :> 150 :> repeat 0
    , tgBurstCount = 24
    }
 where
  txFirst = if isInjector then 100 else 100 + hopC
  peerTxFirst = if isInjector then 100 + hopC else 100

{- | Credit-mode generator load: five 16-word bursts per 200-cycle period
(~43% duty) on a schedule deliberately unaligned with the decode dataflow,
so decode frames genuinely queue behind generator bursts.
-}
tgCredit :: Bool -> TrafficGenSettings
tgCredit _isInjector =
  tgOff
    { tgMode = TgCredit
    , tgFirstCycle = 100
    , tgPeriod = 200
    , tgBurstWords = 16
    , tgBurstsPerPeriod = 5
    , tgOffsets = 0 :> 40 :> 80 :> 120 :> 160 :> repeat 0
    , tgBurstCount = 40
    }

delayBy ::
  forall d.
  (KnownNat d, 1 <= d) =>
  (HiddenClockResetEnable System) =>
  SNat d ->
  Signal System (BitVector 64) ->
  Signal System (BitVector 64)
delayBy SNat = mealy go (repeat 0 :: Vec d (BitVector 64), 0 :: Index d)
 where
  go (st, i) inp = ((replace i inp st, satSucc SatWrap i), st !! i)

{- | The full shared-link node: processing element (both front-ends), credit
link, traffic generator, receive demux and port arbiter — the exact wiring
of the demo user core.
-}
nodeShared ::
  (HiddenClockResetEnable System) =>
  Signal System (DecodePeSettings 2) ->
  Signal System TrafficGenSettings ->
  Signal System CreditLinkKnobs ->
  Signal System Bool ->
  Signal System (Unsigned 64) ->
  Signal System (Vec 2 (BitVector 64)) ->
  ( Signal System (Vec 2 (BitVector 64))
  , Signal System DecodePeStatus
  , Signal System CreditLinkStatus
  , Signal System TrafficGenStatus
  )
nodeShared cfgS tgCfgS knobsS arm cnt rxs = (txs, status, clStatus, tgStat)
 where
  isCredit = (\cfg -> cfg.mode == ModeCredit) <$> cfgS
  tgIsCredit = (\c -> c.tgMode == TgCredit) <$> tgCfgS

  fwdRawRx = liftA2 (\cfg v -> v !! fromMaybe (0 :: Index 2) cfg.readLink) cfgS rxs
  crdRawRx = liftA2 (\cfg v -> v !! fromMaybe (0 :: Index 2) cfg.writeLink) cfgS rxs

  -- Credit-mode frame demux of the shared upstream tap.
  demuxed =
    rxStreamDemux
      hasReset
      arm
      (liftA2 (\cfg tg -> (cfg.vectorWords, tg.tgBurstWords)) cfgS tgCfgS)
      fwdRawRx
  decodeFwdRx = fst <$> demuxed
  tgFwdRx = snd <$> demuxed

  -- Port arbiter for the shared forward direction. Word valids are
  -- registered: they close a loop through the senders and only feed the
  -- collision counter.
  arbOut =
    linkPortArbiter
      hasReset
      arm
      ((.reservePort) <$> clOut)
      ((.portRequest) <$> clOut)
      ((.tgPortReq) <$> tgOut)
      ((.txActive) <$> clOut)
      ((.tgTxActive) <$> tgOut)
      wordValidsR

  clOut =
    creditLink
      hasReset
      cnt
      cfgS
      knobsS
      arm
      decodeFwdRx
      crdRawRx
      ((.arbPortFree) <$> arbOut)
      ((.arbDecodeGrant) <$> arbOut)
      streamOut

  tgOut =
    trafficGen
      hasReset
      cnt
      tgCfgS
      arm
      (mux tgIsCredit tgFwdRx fwdRawRx)
      crdRawRx
      ((.arbTgGrant) <$> arbOut)
      (isJust <$> decodeCrdM)

  fireA = calendarFrontEnd hasReset cnt cfgS arm
  fire = mux isCredit ((.fire) <$> clOut) fireA
  rxWord = mux isCredit ((.coreRxWord) <$> clOut) fwdRawRx
  streamIn = StreamIn <$> fire <*> rxWord
  (_, streamOut) = toSignals (decodeReduceCore hasReset cnt cfgS) (streamIn, ())

  extSample =
    liftA2
      (\cfg r -> if cfg.isInjector || cfg.mode /= ModeCredit then Nothing else r)
      cfgS
      ((.rttSample) <$> clOut)
  status =
    decodeSequencer hasReset cnt cfgS arm fire ((.lapResult) <$> streamOut) extSample
  clStatus = (.status) <$> clOut
  tgStat =
    liftA2
      (\t a -> (t.tgStatus){tgCollisions = a.arbCollisions})
      tgOut
      arbOut

  idle = pack <$> cnt

  -- Forward direction: the decode flow's word, else the generator's, else
  -- idle. Arbitration (credit) or disjoint calendars (scheduled) guarantee
  -- at most one is valid; the collision counter proves it.
  decodeTxM = mux isCredit ((.forwardTx) <$> clOut) ((.txWord) <$> streamOut)
  tgTxM = (.tgTxWord) <$> tgOut
  fwdWord = pick <$> decodeTxM <*> tgTxM <*> idle
  wordValidsR =
    register (False, False) (liftA2 (\d t -> (isJust d, isJust t)) decodeTxM tgTxM)

  -- Reverse direction: single-word credits; the decode flow wins the cycle
  -- and the generator's receiver stalls its pulse (it watches this).
  decodeCrdM = (.creditTx) <$> clOut
  tgCrdM = (.tgCreditTx) <$> tgOut
  crdWord = pick <$> decodeCrdM <*> tgCrdM <*> idle

  pick :: Maybe (BitVector 64) -> Maybe (BitVector 64) -> BitVector 64 -> BitVector 64
  pick d t i = fromMaybe (fromMaybe i t) d

  txs = bundle (crdWord :> fwdWord :> Nil)

-- | Two shared-link nodes in a loop; node 0 injects, node 1 relays.
dutShared ::
  PeMode ->
  (Bool -> TrafficGenSettings) ->
  Bool ->
  Int ->
  ( [DecodePeStatus]
  , [DecodePeStatus]
  , [CreditLinkStatus]
  , [TrafficGenStatus]
  , [TrafficGenStatus]
  )
dutShared mode tgCfg cutThrough nCycles =
  ( sampleN nCycles st0
  , sampleN nCycles st1
  , sampleN nCycles cl0
  , sampleN nCycles tg0
  , sampleN nCycles tg1
  )
 where
  (st0, st1, cl0, tg0, tg1) = withClockResetEnable clockGen resetGen enableGen go
  go ::
    (HiddenClockResetEnable System) =>
    ( Signal System DecodePeStatus
    , Signal System DecodePeStatus
    , Signal System CreditLinkStatus
    , Signal System TrafficGenStatus
    , Signal System TrafficGenStatus
    )
  go = (status0, status1, clStatus0, tgStat0, tgStat1)
   where
    cnt :: Signal System (Unsigned 64)
    cnt = register 0 (cnt + 1)
    arm = (== 20) <$> cnt
    cfg0 = pure (mkSettings mode True)
    cfg1 = pure (mkSettings mode False)
    knobs = pure (mkKnobs cutThrough)
    (txs0, status0, clStatus0, tgStat0) = nodeShared cfg0 (pure (tgCfg True)) knobs arm cnt rxs0
    (txs1, status1, _, tgStat1) = nodeShared cfg1 (pure (tgCfg False)) knobs arm cnt rxs1
    -- Forward traffic lands on the peer's read link (0); credits land on
    -- the peer's write link (1).
    rxs0 =
      bundle
        ( delayBy (SNat @ForwardDelay) ((!! (1 :: Index 2)) <$> txs1)
            :> delayBy (SNat @5) ((!! (0 :: Index 2)) <$> txs1)
            :> Nil
        )
    rxs1 =
      bundle
        ( delayBy (SNat @ForwardDelay) ((!! (1 :: Index 2)) <$> txs0)
            :> delayBy (SNat @5) ((!! (0 :: Index 2)) <$> txs0)
            :> Nil
        )

assertDecodeEqual :: String -> DecodePeStatus -> DecodePeStatus -> Assertion
assertDecodeEqual who a b = do
  assertEqual (who <> " tokens_done") a.tokensDone b.tokensDone
  assertEqual (who <> " checksum_fail_count") a.checksumFailCount b.checksumFailCount
  assertEqual (who <> " min_latency") a.minLatency b.minLatency
  assertEqual (who <> " max_latency") a.maxLatency b.maxLatency
  assertEqual (who <> " done") a.done b.done

assertDecodeClean :: String -> DecodePeStatus -> Assertion
assertDecodeClean who st = do
  assertEqual (who <> " tokens_done") (fromIntegral tokensC) st.tokensDone
  assertEqual (who <> " checksum failures") 0 st.checksumFailCount
  assertBool (who <> " done") st.done

assertCreditClean :: String -> CreditLinkStatus -> Assertion
assertCreditClean who cl = do
  assertEqual (who <> " frames_received") transfersPerRun cl.framesReceived
  assertEqual (who <> " credits_consumed") transfersPerRun cl.creditsConsumed
  assertEqual (who <> " credits_returned") transfersPerRun cl.creditsReturned
  assertEqual (who <> " credits_granted") transfersPerRun cl.creditsGranted
  assertEqual (who <> " header_errors") 0 cl.headerErrors
  assertEqual (who <> " credit_errors") 0 cl.creditErrors
  assertEqual (who <> " no_credit_drops") 0 cl.noCreditDrops
  assertEqual (who <> " timeout_count") 0 cl.timeoutCount

assertTgClean :: String -> TrafficGenSettings -> TrafficGenStatus -> TrafficGenStatus -> Assertion
assertTgClean who cfg sender receiver = do
  assertEqual (who <> " tg sent") cfg.tgBurstCount sender.tgSent
  assertBool (who <> " tg tx done") sender.tgTxDone
  assertEqual (who <> " tg received") cfg.tgBurstCount receiver.tgReceived
  assertEqual (who <> " tg pattern errors") 0 receiver.tgPatternErrors
  assertEqual (who <> " tg collisions") 0 sender.tgCollisions

{- | 0%-duty regression gate: with the generator off, the shared-link node
must reproduce the plain credit-link numbers exactly (cut-through and
store-and-forward), and the calendar node likewise.
-}
case_regressionQuiet :: Assertion
case_regressionQuiet = do
  let
    (ctSts0, ctSts1, ctCls0, _, _) = dutShared ModeCredit (const tgOff) True 1600
    (sfSts0, _, sfCls0, _, _) = dutShared ModeCredit (const tgOff) False 2200
    (calSts0, calSts1, _, _, _) = dutShared ModeCalendar (const tgOff) True 2200
    ctTokenLatency = layersC * (4 * (natToNum @ForwardDelay + 1) + vectorWordsC) + (layersC - 1)
  assertDecodeClean "quiet ct injector" (L.last ctSts0)
  assertDecodeClean "quiet ct relay" (L.last ctSts1)
  assertCreditClean "quiet ct injector" (L.last ctCls0)
  assertEqual "quiet ct min" (fromIntegral ctTokenLatency) (L.last ctSts0).minLatency
  assertEqual "quiet ct max" (fromIntegral ctTokenLatency) (L.last ctSts0).maxLatency
  assertDecodeClean "quiet sf injector" (L.last sfSts0)
  assertCreditClean "quiet sf injector" (L.last sfCls0)
  assertEqual "quiet sf spread" (L.last sfSts0).minLatency (L.last sfSts0).maxLatency
  assertBool "quiet sf > ct" ((L.last sfSts0).maxLatency > (L.last ctSts0).maxLatency)
  assertDecodeClean "quiet cal injector" (L.last calSts0)
  assertDecodeClean "quiet cal relay" (L.last calSts1)
  assertEqual "quiet cal spread" (L.last calSts0).minLatency (L.last calSts0).maxLatency

{- | Variant A_c's shape: decode on the calendar, the generator in disjoint
calendar slots. The decode latency must be bit-identical to the quiet
calendar run — the admission contract, demonstrated literally — while the
generator's traffic verifies cleanly and the collision counter stays zero.
-}
case_scheduledInterleave :: Assertion
case_scheduledInterleave = do
  let
    nCycles = 20 + 100 + 8 * 200 + 400
    (quiet0, quiet1, _, _, _) = dutShared ModeCalendar (const tgOff) True nCycles
    (sts0, sts1, _, tg0, tg1) = dutShared ModeCalendar tgScheduled True nCycles
  assertDecodeClean "a_c injector" (L.last sts0)
  assertDecodeClean "a_c relay" (L.last sts1)
  assertDecodeEqual "a_c vs quiet injector" (L.last quiet0) (L.last sts0)
  assertDecodeEqual "a_c vs quiet relay" (L.last quiet1) (L.last sts1)
  assertTgClean "a_c node0->node1" (tgScheduled True) (L.last tg0) (L.last tg1)
  assertTgClean "a_c node1->node0" (tgScheduled False) (L.last tg1) (L.last tg0)
  -- Scheduled bursts never queue.
  assertEqual "a_c tg max queue" 0 (L.last tg0).tgMaxQueue

{- | Variant B_c's shape: decode behind the credit link (cut-through), the
generator behind its own credits, meeting at the arbiter. Both flows must
complete losslessly with exact accounting; the decode latency shifts and
spreads relative to the quiet run — the queueing the experiment measures.
-}
case_creditContention :: Assertion
case_creditContention = do
  let
    nCycles = 6000
    (quiet0, _, _, _, _) = dutShared ModeCredit (const tgOff) True nCycles
    (sts0, sts1, cls0, tg0, tg1) = dutShared ModeCredit tgCredit True nCycles
    quietLatency = (L.last quiet0).maxLatency
    fin0 = L.last sts0
  assertDecodeClean "b_c injector" fin0
  assertDecodeClean "b_c relay" (L.last sts1)
  assertCreditClean "b_c injector" (L.last cls0)
  assertTgClean "b_c node0->node1" (tgCredit True) (L.last tg0) (L.last tg1)
  assertTgClean "b_c node1->node0" (tgCredit False) (L.last tg1) (L.last tg0)
  -- The competing flow queues (its whole point)...
  assertBool
    ("b_c tg max queue (" <> show (L.last tg0).tgMaxQueue <> ") > 0")
    ((L.last tg0).tgMaxQueue > 0)
  -- ...and the decode latency shifts and spreads.
  assertBool
    ("b_c max (" <> show fin0.maxLatency <> ") > quiet (" <> show quietLatency <> ")")
    (fin0.maxLatency > quietLatency)
  assertBool
    ("b_c spread: min " <> show fin0.minLatency <> " max " <> show fin0.maxLatency)
    (fin0.maxLatency > fin0.minLatency)

tests :: TestTree
tests = $(testGroupGenerator)
