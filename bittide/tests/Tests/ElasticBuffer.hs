-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# OPTIONS_GHC -Wno-orphans #-}

module Tests.ElasticBuffer where

import Clash.Prelude

import Clash.Hedgehog.Sized.Unsigned
import Data.String.Interpolate (i)
import Hedgehog
import Protocols.Hedgehog
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog

import Bittide.ClockControl (RelDataCount, targetDataCount)
import Bittide.ElasticBuffer

import qualified Data.List as L
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

createDomain vXilinxSystem{vPeriod = hzToPeriod 104e6, vName = "Fast"}
createDomain vXilinxSystem{vPeriod = hzToPeriod 100e6, vName = "Slow"}

tests :: TestTree
tests =
  testGroup
    "Tests.ElasticBuffer"
    [ testGroup
        "xilinxElasticBuffer"
        [ testCase "case_xilinxElasticBufferMaxBound" case_xilinxElasticBufferMaxBound
        , testCase "case_xilinxElasticBufferMinBound" case_xilinxElasticBufferMinBound
        , testCase "case_xilinxElasticBufferEq" case_xilinxElasticBufferEq
        ]
    , testGroup
        "resettableXilinxElasticBuffer"
        [ testCase "case_resettableXilinxElasticBufferEq" case_resettableXilinxElasticBufferEq
        , testCase
            "case_resettableXilinxElasticBufferMaxBound"
            case_resettableXilinxElasticBufferMaxBound
        , testCase
            "case_resettableXilinxElasticBufferMinBound"
            case_resettableXilinxElasticBufferMinBound
        , testCase
            "case_resettableXilinxElasticBufferEbControlOverflow"
            case_resettableXilinxElasticBufferEbControlOverFlow
        , testCase
            "case_resettableXilinxElasticBufferEbControlUnderFlow"
            case_resettableXilinxElasticBufferEbControlUnderFlow
        , testCase "case_resettableXilinxElasticBufferId" case_resettableXilinxElasticBufferId
        ]
    , testGroup
        "safeXpmCdcPulse"
        [ testProperty "prop_safeXpmCdcPulse0" prop_safeXpmCdcPulse0
        , testProperty "prop_safeXpmCdcPulse1" prop_safeXpmCdcPulse1
        , testProperty "prop_safeXpmCdcPulse2" prop_safeXpmCdcPulse2
        ]
    , testGroup
        "xpmCdcHandshakeDf"
        [ testProperty "prop_xpmCdcHandshakeDf" prop_xpmCdcHandshakeDf0
        , testProperty "prop_xpmCdcHandshakeDf" prop_xpmCdcHandshakeDf1
        , testProperty "prop_xpmCdcHandshakeDf" prop_xpmCdcHandshakeDf2
        ]
    ]

{- | When the xilinxElasticBuffer is written to more quickly than it is being read from,
its data count should overflow.
-}
case_xilinxElasticBufferMaxBound :: Assertion
case_xilinxElasticBufferMaxBound = do
  let
    ebControl = fromList $ L.replicate 3 (Just AddFrame) <> L.repeat Nothing
    wData = pure (0 :: Unsigned 8)
    underflows =
      sampleN
        10000
        ( (\(_, under, _, _, _) -> under)
            (xilinxElasticBuffer @6 (clockGen @Slow) (clockGen @Fast) resetGen ebControl wData)
        )
    overflows =
      sampleN
        10000
        ( (\(_, _, over, _, _) -> over)
            (xilinxElasticBuffer @6 (clockGen @Slow) (clockGen @Fast) resetGen ebControl wData)
        )

  assertBool "elastic buffer should overflow" (or overflows)
  assertBool "elastic buffer should not underflow" (not $ or underflows)

{- | When the xilinxElasticBuffer is read from more quickly than it is being written to,
its data count should underflow.
-}
case_xilinxElasticBufferMinBound :: Assertion
case_xilinxElasticBufferMinBound = do
  let
    ebControl = fromList $ L.replicate 32 (Just AddFrame) <> L.repeat Nothing
    wData = pure (0 :: Unsigned 8)
    underflows =
      sampleN
        1024
        ( (\(_, under, _, _, _) -> under)
            (xilinxElasticBuffer @8 (clockGen @Fast) (clockGen @Slow) resetGen ebControl wData)
        )
    overflows =
      sampleN
        1024
        ( (\(_, _, over, _, _) -> over)
            (xilinxElasticBuffer @8 (clockGen @Fast) (clockGen @Slow) resetGen ebControl wData)
        )

  assertBool "elastic buffer should underflow" (or underflows)
  assertBool "elastic buffer should not overflow" (not $ or overflows)

{- | When the xilinxElasticBuffer is written to as quickly to as it is read from, it should
neither overflow nor underflow.
-}
case_xilinxElasticBufferEq :: Assertion
case_xilinxElasticBufferEq = do
  let
    ebControl = fromList $ L.replicate 64 (Just AddFrame) <> L.repeat Nothing
    wData = pure (0 :: Unsigned 8)
    underflows =
      sampleN
        256
        ( (\(_, under, _, _, _) -> under)
            (xilinxElasticBuffer @8 (clockGen @Slow) (clockGen @Slow) resetGen ebControl wData)
        )
    overflows =
      sampleN
        256
        ( (\(_, _, over, _, _) -> over)
            (xilinxElasticBuffer @8 (clockGen @Slow) (clockGen @Slow) resetGen ebControl wData)
        )

  assertBool "elastic buffer should not underflow" (not $ or underflows)
  assertBool "elastic buffer should not overflow" (not $ or overflows)

{- | When the resettableXilinxElasticBuffer is written to as quickly as it is read from, it eventually
 stabalises.
-}
case_resettableXilinxElasticBufferEq :: Assertion
case_resettableXilinxElasticBufferEq = do
  let
    wData = pure (0 :: Unsigned 8)
    ebControl = pure Nothing
    (dataCounts, underflows, overflows, ebReadys, _, _) =
      L.unzip6
        . L.dropWhile (\(_, _, _, ebReady, _, _) -> not ebReady)
        $ sampleN
          256
          ( bundle
              ( resettableXilinxElasticBuffer @5
                  (clockGen @Slow)
                  (clockGen @Slow)
                  resetGen
                  ebControl
                  wData
              )
          )
    dataCountBounds =
      L.all
        ((< 3) . abs . subtract (toInteger (targetDataCount :: RelDataCount 5)) . toInteger)
        dataCounts

  assertBool
    "elastic buffer should get out of its (Just AddFrame) state"
    (L.length ebReadys > 0)
  assertBool "elastic buffer should not overflow after stabalising" (not $ or overflows)
  assertBool "elastic buffer should not underflow after stabalising" (not $ or underflows)
  assertBool "elastic buffer should be in Nothing mode after stabalising" (L.and ebReadys)
  assertBool
    "elastic buffer datacount should be `targetDataCount` (margin 3 elements) after stabalising"
    dataCountBounds

{- | When the xilinxElasticBuffer is written to more quickly than it is being read from,
its data count should overflow. Upon an overflow, the fifo is (Just RemoveFrame)ed and then (Just AddFrame)ed
to half full, after which the cycle repeats.
-}
case_resettableXilinxElasticBufferMaxBound :: Assertion
case_resettableXilinxElasticBufferMaxBound = do
  let
    wData = pure (0 :: Unsigned 8)
    ebControl = pure Nothing
    (_, underflows, overflows, _, _, _) =
      L.unzip6
        . L.dropWhile (\(_, _, _, stable, _, _) -> not stable)
        $ sampleN
          5000
          ( bundle
              ( resettableXilinxElasticBuffer @5
                  (clockGen @Slow)
                  (clockGen @Fast)
                  resetGen
                  ebControl
                  wData
              )
          )
  -- After the fifo overflows, it should (Just RemoveFrame) the buffer, then (Just AddFrame) it to half full and
  -- reset.
  assertBool
    "elastic buffer should reset after an overflow"
    ([True, False] `L.isInfixOf` overflows)
  assertBool
    "elastic buffer should not underflow when written to faster than read from"
    (not $ or underflows)

{- | When the xilinxElasticBuffer is read from more quickly than it is being written to,
its data count should overflow. Upon an overflow, the fifo is (Just RemoveFrame)ed and then (Just AddFrame)ed
to half full, after which the cycle repeats.
-}
case_resettableXilinxElasticBufferMinBound :: Assertion
case_resettableXilinxElasticBufferMinBound = do
  let
    wData = pure (0 :: Unsigned 8)
    ebControl = pure Nothing
    (_, underflows, overflows, _, _, _) =
      L.unzip6
        . L.filter (\(_, _, _, ebReady, _, _) -> ebReady)
        $ sampleN
          512
          ( bundle
              ( resettableXilinxElasticBuffer @5
                  (clockGen @Fast)
                  (clockGen @Slow)
                  resetGen
                  ebControl
                  wData
              )
          )

  -- After the fifo underflows, it should (Just RemoveFrame) for 1 cycle and then (Just AddFrame) it to half
  -- full and reset.
  assertBool
    "elastic buffer should reset after an underflow"
    ([True, False] `L.isInfixOf` underflows)
  assertBool
    "elastic buffer should not overflow when read from faster than written to"
    (not $ or overflows)

case_resettableXilinxElasticBufferEbControlUnderFlow :: Assertion
case_resettableXilinxElasticBufferEbControlUnderFlow = do
  let
    wData = pure (0 :: Unsigned 8)
    ebControl = pure $ Just RemoveFrame
    (_counts, underflows, overflows, _, acks, _) =
      L.unzip6
        . L.dropWhile (\(_, _, _, stable, _, _) -> not stable)
        $ sampleN
          512
          ( bundle
              ( resettableXilinxElasticBuffer @5
                  (clockGen @Slow)
                  (clockGen @Slow)
                  resetGen
                  ebControl
                  wData
              )
          )
    nAcks = L.length $ filter id acks
  -- If we continually set `Just RemoveFrame`, we will eventually underflow, but never overflow
  assertBool
    "elastic buffer should underflow after removing frames"
    (or underflows)
  assertBool
    "elastic buffer should not overflow when removing frames"
    (not $ or overflows)
  assertBool
    "Number of acknowledges should be at least half the buffer size"
    (nAcks >= 16) -- based on 5 bits
  assertBool
    "There are no two consecutive acknowledges"
    (not $ L.isInfixOf [True, True] acks)

case_resettableXilinxElasticBufferEbControlOverFlow :: Assertion
case_resettableXilinxElasticBufferEbControlOverFlow = do
  let
    wData = pure (0 :: Unsigned 8)
    ebControl = pure $ Just AddFrame
    (_, underflows, overflows, _, _, _) =
      L.unzip6
        . L.dropWhile (\(_, _, _, stable, _, _) -> not stable)
        $ sampleN
          256
          ( bundle
              ( resettableXilinxElasticBuffer @5
                  (clockGen @Slow)
                  (clockGen @Slow)
                  resetGen
                  ebControl
                  wData
              )
          )

  -- If we continually set `Just AddFrame`, we will eventually overflow, but never underflow
  assertBool
    "elastic buffer should overflow after adding frames"
    (or overflows)
  assertBool
    "elastic buffer should not underflow when adding frames"
    (not $ or underflows)

case_resettableXilinxElasticBufferId :: Assertion
case_resettableXilinxElasticBufferId = do
  let
    wData = [0 .. maxBound] :: [Unsigned 16]
    ebControl = pure Nothing
    (_, underflows, overflows, stables, _, rData) =
      L.unzip6
        . L.dropWhile (\(_, _, _, stable, _, _) -> not stable)
        $ sampleN
          100
          ( bundle
              ( resettableXilinxElasticBuffer @5
                  (clockGen @Slow)
                  (clockGen @Slow)
                  resetGen
                  ebControl
                  (fromList wData)
              )
          )
  assertBool
    "elastic buffer should not underflow during normal operation"
    (not $ or underflows)
  assertBool
    "elastic buffer should not overflow during normal operation"
    (not $ or overflows)
  assertBool
    "elastic buffer should not drop data during normal operation"
    (L.isInfixOf (L.drop 5 rData) wData)
  assertBool
    "elastic buffer should remain stable during normal operation"
    (and stables)

-- Given a source clock domain and destination clock domain, return an `Assertion`
-- that verifies that `safeXpmCdcPulse` produces as many pulses in the destination
-- domain as it receives in the source domain.
-- Also verify that the minimum gap is enforced by the acknowledgement signal.
makeSafeXpmCdcPulseTest ::
  forall src dst.
  ( KnownDomain src
  , KnownDomain dst
  , 1 <= DomainPeriod src
  ) =>
  Clock src ->
  Clock dst ->
  Property
makeSafeXpmCdcPulseTest clkSrc clkDst = property $ do
  let
    srcPeriod = natToNum @(DomainPeriod src)
    dstPeriod = natToNum @(DomainPeriod dst)
    minGapPeriod = 2 * max srcPeriod dstPeriod
    minGapCycles = (minGapPeriod + srcPeriod - 1) `div` srcPeriod
  simLengthSrc <- forAll $ Gen.int (Range.linear (4 * minGapCycles) (20 * minGapCycles))
  inputPulses0 <-
    forAll $ Gen.list (Range.singleton (simLengthSrc - (4 * minGapCycles))) Gen.bool
  let
    inputPulses1 = False : inputPulses0 <> L.repeat False
    dut = safeXpmCdcPulse clkSrc clkDst $ fromList inputPulses1
    simLengthDst = (simLengthSrc * srcPeriod + dstPeriod - 1) `div` dstPeriod

    outputPulses = sampleN simLengthDst $ fst dut
    acks = sampleN simLengthSrc $ snd dut
    nOutputPulses = L.length $ filter id outputPulses
    nAcks = L.length $ filter id acks
  footnote $ [i|Received #{nOutputPulses} output pulses and #{nAcks} acknowledgements|]
  footnote $ [i|Minimum gap between pulses: #{minGapCycles} cycles|]
  footnote $ [i|Output pulses: #{outputPulses}|]
  footnote $ [i|Input pulses: #{L.take simLengthSrc inputPulses1}|]
  footnote $ [i|Acknowledgements: #{acks}|]
  nAcks === nOutputPulses

prop_safeXpmCdcPulse0 :: Property
prop_safeXpmCdcPulse0 = makeSafeXpmCdcPulseTest (clockGen @Slow) (clockGen @Slow)

prop_safeXpmCdcPulse1 :: Property
prop_safeXpmCdcPulse1 = makeSafeXpmCdcPulseTest (clockGen @Slow) (clockGen @Fast)

prop_safeXpmCdcPulse2 :: Property
prop_safeXpmCdcPulse2 = makeSafeXpmCdcPulseTest (clockGen @Fast) (clockGen @Slow)

make_xpmCdcHandshakeDfTest ::
  forall src dst.
  (KnownDomain src, KnownDomain dst) =>
  Clock src ->
  Clock dst ->
  Property
make_xpmCdcHandshakeDfTest clkSrc clkDst =
  idWithModel defExpectOptions{eoResetCycles = 0} gen id dut
 where
  gen = Gen.list (Range.linear 1 100) $ genUnsigned @8 Range.linearBounded
  dut = xpmCdcHandshakeDf clkSrc clkDst

prop_xpmCdcHandshakeDf0 :: Property
prop_xpmCdcHandshakeDf0 = make_xpmCdcHandshakeDfTest (clockGen @Slow) (clockGen @Slow)

prop_xpmCdcHandshakeDf1 :: Property
prop_xpmCdcHandshakeDf1 = make_xpmCdcHandshakeDfTest (clockGen @Slow) (clockGen @Fast)

prop_xpmCdcHandshakeDf2 :: Property
prop_xpmCdcHandshakeDf2 = make_xpmCdcHandshakeDfTest (clockGen @Fast) (clockGen @Slow)
