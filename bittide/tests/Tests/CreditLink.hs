-- SPDX-FileCopyrightText: 2026 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

{- | Two-node simulations of the credit link (variant B), in cut-through and
store-and-forward disciplines, plus a credit-path delay sweep and an
A-then-B mode switch on one instance.

Wiring: two links per node. Link 0 is the read link (incoming forward
traffic, outgoing credits), link 1 is the write link (outgoing forward
traffic, incoming credits). Forward paths have a 34-cycle delay; the credit
paths' delay is a parameter.

A note on credit slack, load-bearing for the host driver: in a 2-node ring
the same physical link carries a layer's lap-1 and lap-2 frames only
@2*(delay+1)@ cycles apart, while the credit for the lap-1 frame returns
after roughly @vector_words + credit_delay@ cycles. Cut-through relays DROP
on a missing credit, so cut-through needs the credit back in time — hence the
short credit delay in these tests. On the 8-node ring the gap is 8 hops,
which comfortably hides the credit return for @vector_words@ up to ~200.
Store-and-forward relays block instead of dropping, so they are safe at any
credit delay; the sweep uses them.
-}
module Tests.CreditLink where

import Clash.Prelude

import Data.Maybe (fromMaybe)
import Protocols (toSignals)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.TH (testGroupGenerator)

import Bittide.CreditLink
import Bittide.DecodeProcessingElement

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

-- Per-frame transfers each node makes and receives per run.
transfersPerRun :: Unsigned 32
transfersPerRun = fromIntegral (2 * layersC * tokensC)

mkSettingsB :: PeMode -> Bool -> Unsigned 64 -> DecodePeSettings 2
mkSettingsB mode isInjector firstCycle =
  DecodePeSettings
    { readLink = Just 0
    , writeLink = Just 1
    , isInjector
    , mode
    , firstCycle
    , lapOffset = 2 * (natToNum @ForwardDelay + 1)
    , layerPeriod = 200
    , layersPerToken = fromIntegral layersC
    , tokenPeriod = 512
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

-- | One credit-mode node: credit link + reduce core + sequencer.
nodeB ::
  (HiddenClockResetEnable System) =>
  Signal System (DecodePeSettings 2) ->
  Signal System CreditLinkKnobs ->
  Signal System Bool ->
  Signal System (Unsigned 64) ->
  Signal System (Vec 2 (BitVector 64)) ->
  ( Signal System (Maybe (BitVector 64)) -- forward tx (write link)
  , Signal System (Maybe (BitVector 64)) -- credit tx (read link)
  , Signal System DecodePeStatus
  , Signal System CreditLinkStatus
  )
nodeB cfgS knobsS arm cnt rxs = (fwdTx, crdTx, status, clStatus)
 where
  fwdRx = liftA2 (\cfg v -> v !! fromMaybe (0 :: Index 2) cfg.readLink) cfgS rxs
  crdRx = liftA2 (\cfg v -> v !! fromMaybe (0 :: Index 2) cfg.writeLink) cfgS rxs
  clOut =
    creditLink hasReset cnt cfgS knobsS arm fwdRx crdRx (pure True) (pure True) streamOut
  fire = (.fire) <$> clOut
  streamIn = StreamIn <$> fire <*> ((.coreRxWord) <$> clOut)
  (_, streamOut) = toSignals (decodeReduceCore hasReset cnt cfgS) (streamIn, ())
  -- Relays sample per-transfer credit round-trip times into the histogram;
  -- the injector's histogram carries token latencies.
  extSample =
    liftA2
      (\cfg r -> if cfg.isInjector then Nothing else r)
      cfgS
      ((.rttSample) <$> clOut)
  status =
    decodeSequencer hasReset cnt cfgS arm fire ((.lapResult) <$> streamOut) extSample
  fwdTx = (.forwardTx) <$> clOut
  crdTx = (.creditTx) <$> clOut
  clStatus = (.status) <$> clOut

delayBy ::
  forall d.
  (KnownNat d, 1 <= d) =>
  (HiddenClockResetEnable System) =>
  SNat d ->
  Signal System (BitVector 64) ->
  Signal System (BitVector 64)
delayBy SNat = mealy go (repeat 0 :: Vec d (BitVector 64), 0 :: Index d)
 where
  -- Circular buffer: the slot about to be overwritten is d cycles old.
  go (st, i) inp = ((replace i inp st, satSucc SatWrap i), st !! i)

{- | The two-node credit loop. Arm at cycle 20, injector launch gate at 100.
Returns the sampled statuses of (injector, relay).
-}
dutB ::
  forall creditDelay.
  (KnownNat creditDelay, 1 <= creditDelay) =>
  SNat creditDelay ->
  Bool ->
  Int ->
  ( [DecodePeStatus]
  , [DecodePeStatus]
  , [CreditLinkStatus]
  , [CreditLinkStatus]
  )
dutB creditDelay cutThrough nCycles =
  ( sampleN nCycles st0
  , sampleN nCycles st1
  , sampleN nCycles cl0
  , sampleN nCycles cl1
  )
 where
  (st0, st1, cl0, cl1) = withClockResetEnable clockGen resetGen enableGen go
  go ::
    (HiddenClockResetEnable System) =>
    ( Signal System DecodePeStatus
    , Signal System DecodePeStatus
    , Signal System CreditLinkStatus
    , Signal System CreditLinkStatus
    )
  go = (status0, status1, clStatus0, clStatus1)
   where
    cnt :: Signal System (Unsigned 64)
    cnt = register 0 (cnt + 1)
    arm = (== 20) <$> cnt
    idle = pack <$> cnt
    fill = liftA2 fromMaybe idle
    cfg0 = pure (mkSettingsB ModeCredit True 100)
    cfg1 = pure (mkSettingsB ModeCredit False 100)
    knobs = pure (mkKnobs cutThrough)
    (fwd0, crd0, status0, clStatus0) = nodeB cfg0 knobs arm cnt rxs0
    (fwd1, crd1, status1, clStatus1) = nodeB cfg1 knobs arm cnt rxs1
    -- Forward traffic lands on the peer's read link (0); credits land on
    -- the peer's write link (1).
    rxs0 = bundle (delayBy (SNat @ForwardDelay) (fill fwd1) :> delayBy creditDelay (fill crd1) :> Nil)
    rxs1 = bundle (delayBy (SNat @ForwardDelay) (fill fwd0) :> delayBy creditDelay (fill crd0) :> Nil)

-- Analytic token latency, cut-through: per layer the wavefront crosses the
-- ring twice (4 hops of @delay+1@) plus the sink's V words; layers chain with
-- one request cycle in between.
ctTokenLatencyC :: Int
ctTokenLatencyC = layersC * perLayer + (layersC - 1)
 where
  perLayer = 4 * (natToNum @ForwardDelay + 1) + vectorWordsC

assertRunClean ::
  String ->
  DecodePeStatus ->
  CreditLinkStatus ->
  Assertion
assertRunClean who st cl = do
  assertEqual (who <> " tokens_done") (fromIntegral tokensC) st.tokensDone
  assertEqual (who <> " checksum failures") 0 st.checksumFailCount
  assertBool (who <> " done") st.done
  assertEqual (who <> " frames_received") transfersPerRun cl.framesReceived
  assertEqual (who <> " credits_consumed") transfersPerRun cl.creditsConsumed
  assertEqual (who <> " credits_returned") transfersPerRun cl.creditsReturned
  assertEqual (who <> " credits_granted") transfersPerRun cl.creditsGranted
  assertEqual (who <> " header_errors") 0 cl.headerErrors
  assertEqual (who <> " credit_errors") 0 cl.creditErrors
  assertEqual (who <> " no_credit_drops") 0 cl.noCreditDrops
  assertEqual (who <> " timeout_count") 0 cl.timeoutCount

case_cutThroughTwoNode :: Assertion
case_cutThroughTwoNode = do
  let
    (sts0, sts1, cls0, cls1) = dutB (SNat @5) True 1600
    fin0 = L.last sts0
    fin1 = L.last sts1
  assertRunClean "injector" fin0 (L.last cls0)
  assertRunClean "relay" fin1 (L.last cls1)
  -- Constant to the cycle, at the analytic dataflow latency: the credit
  -- round trip is hidden.
  assertEqual "ct min latency" (fromIntegral ctTokenLatencyC) fin0.minLatency
  assertEqual "ct max latency" (fromIntegral ctTokenLatencyC) fin0.maxLatency
  -- Relay credit RTTs are constant too.
  assertEqual "relay rtt spread" (L.last cls1).minCreditRtt (L.last cls1).maxCreditRtt

case_storeAndForwardTwoNode :: Assertion
case_storeAndForwardTwoNode = do
  let
    (ctSts0, _, _, _) = dutB (SNat @5) True 1600
    (sts0, sts1, cls0, cls1) = dutB (SNat @5) False 2200
    ct = (L.last ctSts0).maxLatency
    fin0 = L.last sts0
    fin1 = L.last sts1
  assertRunClean "sf injector" fin0 (L.last cls0)
  assertRunClean "sf relay" fin1 (L.last cls1)
  -- Still constant to the cycle...
  assertEqual "sf latency constant" fin0.minLatency fin0.maxLatency
  -- ...but the per-hop handshake is now on the critical path.
  assertBool
    ( "sf latency ("
        <> show fin0.maxLatency
        <> ") should exceed cut-through latency ("
        <> show ct
        <> ")"
    )
    (fin0.maxLatency > ct)

{- | Credit-path delay sweep (store-and-forward, which blocks rather than
drops): once the credit return no longer hides inside the dataflow chain,
token latency must grow with the credit delay.
-}
case_creditDelaySweep :: Assertion
case_creditDelaySweep = do
  let
    latencyAt ::
      forall d. (KnownNat d, 1 <= d) => SNat d -> Int -> (Unsigned 32, DecodePeStatus, CreditLinkStatus)
    latencyAt d n =
      let (sts0, _, cls0, _) = dutB d False n
       in ((L.last sts0).maxLatency, L.last sts0, L.last cls0)
    (lat5, st5, cl5) = latencyAt (SNat @5) 2200
    (lat200, st200, cl200) = latencyAt (SNat @200) 5200
    (lat400, st400, cl400) = latencyAt (SNat @400) 9000
  assertRunClean "sweep d5 injector" st5 cl5
  assertRunClean "sweep d200 injector" st200 cl200
  assertRunClean "sweep d400 injector" st400 cl400
  assertBool ("d200 (" <> show lat200 <> ") > d5 (" <> show lat5 <> ")") (lat200 > lat5)
  assertBool ("d400 (" <> show lat400 <> ") > d200 (" <> show lat200 <> ")") (lat400 > lat200)

{- | A node with both front-ends, selected by the mode register — the shape
the demo user core has. Runs variant A (calendar) first, then re-arms into
variant B (credit) without any reset in between.
-}
nodeAB ::
  (HiddenClockResetEnable System) =>
  Signal System (DecodePeSettings 2) ->
  Signal System CreditLinkKnobs ->
  Signal System Bool ->
  Signal System (Unsigned 64) ->
  Signal System (Vec 2 (BitVector 64)) ->
  ( Signal System (Vec 2 (BitVector 64))
  , Signal System DecodePeStatus
  , Signal System CreditLinkStatus
  )
nodeAB cfgS knobsS arm cnt rxs = (txs, status, clStatus)
 where
  isCredit = (\cfg -> cfg.mode == ModeCredit) <$> cfgS
  fireA = calendarFrontEnd hasReset cnt cfgS arm
  fwdRx = liftA2 (\cfg v -> v !! fromMaybe (0 :: Index 2) cfg.readLink) cfgS rxs
  crdRx = liftA2 (\cfg v -> v !! fromMaybe (0 :: Index 2) cfg.writeLink) cfgS rxs
  clOut =
    creditLink hasReset cnt cfgS knobsS arm fwdRx crdRx (pure True) (pure True) streamOut
  fire = mux isCredit ((.fire) <$> clOut) fireA
  rxWord = mux isCredit ((.coreRxWord) <$> clOut) ((!! (0 :: Index 2)) <$> rxs)
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
  idle = pack <$> cnt
  coreTx = liftA2 fromMaybe idle ((.txWord) <$> streamOut)
  link0 = mux isCredit (liftA2 fromMaybe idle ((.creditTx) <$> clOut)) idle
  link1 = mux isCredit (liftA2 fromMaybe idle ((.forwardTx) <$> clOut)) coreTx
  txs = bundle (link0 :> link1 :> Nil)

case_calendarThenCredit :: Assertion
case_calendarThenCredit = do
  let
    nCycles = 4200
    (sts0, sts1) = go
    midIdx = 2200
    mid0 = sts0 L.!! midIdx
    mid1 = sts1 L.!! midIdx
    fin0 = L.last sts0
    fin1 = L.last sts1

    go = (sampleN nCycles st0, sampleN nCycles st1)
     where
      (st0, st1) = withClockResetEnable clockGen resetGen enableGen build
      build ::
        (HiddenClockResetEnable System) =>
        (Signal System DecodePeStatus, Signal System DecodePeStatus)
      build = (status0, status1)
       where
        cnt :: Signal System (Unsigned 64)
        cnt = register 0 (cnt + 1)
        arm = (\c -> c == 20 || c == 2300) <$> cnt
        firstRun = (< 2250) <$> cnt
        hop = natToNum @ForwardDelay + 1
        cfg0 =
          mux
            firstRun
            (pure (mkSettingsB ModeCalendar True 100))
            (pure (mkSettingsB ModeCredit True 2400))
        cfg1 =
          mux
            firstRun
            (pure (mkSettingsB ModeCalendar False (100 + hop)))
            (pure (mkSettingsB ModeCredit False 2400))
        knobs = pure (mkKnobs True)
        (txs0, status0, _) = nodeAB cfg0 knobs arm cnt rxs0
        (txs1, status1, _) = nodeAB cfg1 knobs arm cnt rxs1
        -- Forward: peer's write link (1) lands on our read link (0);
        -- credits: peer's read link (0) lands on our write link (1).
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

  -- Variant A completed before the switch...
  assertEqual "A tokens_done" (fromIntegral tokensC) mid0.tokensDone
  assertEqual "A checksum failures" 0 mid0.checksumFailCount
  assertEqual "A relay tokens_done" (fromIntegral tokensC) mid1.tokensDone
  assertEqual "A relay checksum failures" 0 mid1.checksumFailCount
  -- ...and variant B completed after re-arming, with cleared counters.
  assertEqual "B tokens_done" (fromIntegral tokensC) fin0.tokensDone
  assertEqual "B checksum failures" 0 fin0.checksumFailCount
  assertEqual "B relay tokens_done" (fromIntegral tokensC) fin1.tokensDone
  assertEqual "B relay checksum failures" 0 fin1.checksumFailCount
  assertEqual "B latency" (fromIntegral ctTokenLatencyC) fin0.maxLatency
  assertEqual "B latency constant" fin0.minLatency fin0.maxLatency

tests :: TestTree
tests = $(testGroupGenerator)
