-- SPDX-FileCopyrightText: 2026 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

{- | Two-node simulation of the decode processing element in calendar (variant
A) mode: an injector and one relay connected by 34-cycle delay lines, a
schedule computed by hand the way the host driver computes it from UGNs.

Timeline (D = 34, hop = D + 1 = 35 because of the one-register relay stage,
lap offset = 2 hops = 70, injector first cycle t0):

> t0        injector fires Inject; word 0 leaves at t0+1
> t0+35     relay fires AddRelay (word 0 arrives); word 0 leaves at t0+36
> t0+70     injector fires PassRelay (turnaround)
> t0+105    relay fires PassRelay (lap 2)
> t0+140    injector fires Sink; LapResult at t0+140+V

Token latency at the injector = (layers-1)*layerPeriod + 2*lapOffset + V.
-}
module Tests.DecodeProcessingElement where

import Clash.Prelude

import Data.Maybe (fromMaybe)
import Protocols (toSignals)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.TH (testGroupGenerator)

import Bittide.DecodeProcessingElement

import qualified Data.List as L

type LinkDelay = 34

linkDelay :: Int
linkDelay = natToNum @LinkDelay

hopCycles :: Int
hopCycles = linkDelay + 1

lapOffsetC :: Int
lapOffsetC = 2 * hopCycles

vectorWordsC :: Int
vectorWordsC = 4

layersC :: Int
layersC = 2

tokensC :: Int
tokensC = 3

layerPeriodC :: Int
layerPeriodC = 200

tokenPeriodC :: Int
tokenPeriodC = 512

patternInjector, patternRelay :: BitVector 64
patternInjector = 0x1000
patternRelay = 0x2000

-- Checksum of the injector's stream as seen by the relay in lap 1.
expectedWindowAC :: BitVector 64
expectedWindowAC =
  fromIntegral vectorWordsC
    * patternInjector
    + fromIntegral (vectorWordsC * (vectorWordsC - 1) `div` 2)

-- Checksum of the grand total (both contributions) in lap 2.
expectedWindowBC :: BitVector 64
expectedWindowBC =
  fromIntegral vectorWordsC
    * (patternInjector + patternRelay)
    + fromIntegral (vectorWordsC * (vectorWordsC - 1))

injectorLatencyC :: Int
injectorLatencyC = (layersC - 1) * layerPeriodC + 2 * lapOffsetC + vectorWordsC

relayLatencyC :: Int
relayLatencyC = (layersC - 1) * layerPeriodC + lapOffsetC + vectorWordsC

mkSettings :: Bool -> Unsigned 64 -> DecodePeSettings 1
mkSettings isInjector firstCycle =
  DecodePeSettings
    { readLink = Just 0
    , writeLink = Just 0
    , isInjector
    , mode = ModeCalendar
    , firstCycle
    , lapOffset = fromIntegral lapOffsetC
    , layerPeriod = fromIntegral layerPeriodC
    , layersPerToken = fromIntegral layersC
    , tokenPeriod = fromIntegral tokenPeriodC
    , tokenCount = fromIntegral tokensC
    , vectorWords = fromIntegral vectorWordsC
    , computeCycles = 0
    , localPattern = if isInjector then patternInjector else patternRelay
    , expectedWindowA = expectedWindowAC
    , expectedWindowB = expectedWindowBC
    , histBase =
        fromIntegral
          $ (if isInjector then injectorLatencyC else relayLatencyC)
          - 4
    , histShift = 0
    }

-- | One calendar-mode node: calendar front-end + reduce core + sequencer.
nodeA ::
  (HiddenClockResetEnable System) =>
  Signal System (DecodePeSettings 1) ->
  Signal System Bool ->
  Signal System (Unsigned 64) ->
  Signal System (BitVector 64) ->
  (Signal System (BitVector 64), Signal System DecodePeStatus)
nodeA cfgS arm cnt rx = (tx, status)
 where
  fire = calendarFrontEnd hasReset cnt cfgS arm
  streamIn = StreamIn <$> fire <*> rx
  (_, streamOut) = toSignals (decodeReduceCore hasReset cnt cfgS) (streamIn, ())
  status =
    decodeSequencer hasReset cnt cfgS arm fire ((.lapResult) <$> streamOut) (pure Nothing)
  tx = fromMaybe <$> (pack <$> cnt) <*> ((.txWord) <$> streamOut)

delayLine ::
  (HiddenClockResetEnable System) =>
  Signal System (BitVector 64) ->
  Signal System (BitVector 64)
delayLine = mealy go (repeat 0 :: Vec LinkDelay (BitVector 64))
 where
  go st i = (st <<+ i, head st)

{- | The two-node loop, with an arm at cycle 20 (schedule origin 100) and a
re-arm at cycle 2300 (schedule origin 2500) to check that @arm@ clears all
state and restarts cleanly.
-}
dutA :: ([DecodePeStatus], [DecodePeStatus])
dutA = (sampleN totalCycles st0, sampleN totalCycles st1)
 where
  (st0, st1) = withClockResetEnable clockGen resetGen enableGen go
  go ::
    (HiddenClockResetEnable System) =>
    (Signal System DecodePeStatus, Signal System DecodePeStatus)
  go = (status0, status1)
   where
    cnt :: Signal System (Unsigned 64)
    cnt = register 0 (cnt + 1)
    arm = (\c -> c == 20 || c == 2300) <$> cnt
    firstRun = (< 2250) <$> cnt
    cfg0 = mux firstRun (pure (mkSettings True 100)) (pure (mkSettings True 2500))
    cfg1 =
      mux
        firstRun
        (pure (mkSettings False (100 + fromIntegral hopCycles)))
        (pure (mkSettings False (2500 + fromIntegral hopCycles)))
    (tx0, status0) = nodeA cfg0 arm cnt rx0
    (tx1, status1) = nodeA cfg1 arm cnt rx1
    rx1 = delayLine tx0
    rx0 = delayLine tx1

totalCycles :: Int
totalCycles = 4700

case_calendarTwoNode :: Assertion
case_calendarTwoNode = do
  let
    (sts0, sts1) = dutA
    fin0 = L.last sts0
    fin1 = L.last sts1
    -- Final histograms (the second run cleared the first via arm).
    hist0 = fin0.hist
    hist1 = fin1.hist

  assertEqual "injector tokens_done" (fromIntegral tokensC) fin0.tokensDone
  assertEqual "relay tokens_done" (fromIntegral tokensC) fin1.tokensDone
  assertEqual "injector checksum failures" 0 fin0.checksumFailCount
  assertEqual "relay checksum failures" 0 fin1.checksumFailCount
  assertEqual "injector first_fail_cycle" 0 fin0.firstFailCycle
  assertEqual "relay first_fail_cycle" 0 fin1.firstFailCycle
  assertBool "injector done" fin0.done
  assertBool "relay done" fin1.done

  -- Constant to the cycle: min == max == the analytic latency, in both runs.
  assertEqual "injector min latency" (fromIntegral injectorLatencyC) fin0.minLatency
  assertEqual "injector max latency" (fromIntegral injectorLatencyC) fin0.maxLatency
  assertEqual "relay min latency" (fromIntegral relayLatencyC) fin1.minLatency
  assertEqual "relay max latency" (fromIntegral relayLatencyC) fin1.maxLatency

  -- The re-arm cleared the first run's histogram; the second run's tokens
  -- all land in bin 4 (latency - hist_base = 4).
  assertEqual "injector histogram" (fromIntegral tokensC) (hist0 !! (4 :: Int))
  assertEqual "injector histogram mass" (fromIntegral tokensC) (sum hist0)
  assertEqual "relay histogram" (fromIntegral tokensC) (hist1 !! (4 :: Int))
  assertEqual "relay histogram mass" (fromIntegral tokensC) (sum hist1)

tests :: TestTree
tests = $(testGroupGenerator)
