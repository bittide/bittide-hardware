-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Tests.Transceiver where

import Clash.Explicit.Prelude hiding (PeriodToCycles)
import Clash.Prelude (withClock)
import Hedgehog

import Bittide.Arithmetic.Time (PeriodToCycles)
import Bittide.SharedTypes (Bytes)
import Clash.Annotations.Primitive (dontTranslate)
import Clash.Cores.Xilinx.GTH (GthCore)
import Clash.Hedgehog.Sized.Index (genIndex)
import Clash.Signal.Internal (Signal ((:-)))
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (Proxy))
import Data.Sequence (Seq ((:<|), (:|>)))
import Test.Tasty (TestTree, adjustOption, testGroup)
import Test.Tasty.Hedgehog (HedgehogTestLimit (HedgehogTestLimit), testPropertyNamed)

import qualified Bittide.Transceiver as Transceiver
import qualified Bittide.Transceiver.ResetManager as ResetManager
import qualified Bittide.Transceiver.WordAlign as WordAlign
import qualified Data.List as List
import qualified Data.Sequence as Seq
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

createDomain vSystem{vName = "RefIsUnused"}

-- Note that these domains are a factor of ~5_000 slower than we use in practise. We
-- do this because timeouts in the transceivers are specified in milliseconds, which
-- makes the simulation simply do "nothing" for a while. Also note the frequencies
-- of domain A and B are slightly different - which is a realistic property of
-- clocks - though the order of difference is very much exaggerated.
createDomain vSystem{vName = "A", vResetKind = Synchronous, vPeriod = hzToPeriod 60e3}
createDomain vSystem{vName = "B", vResetKind = Synchronous, vPeriod = hzToPeriod 61e3}

{- | The free domain should be able to run at any speed, so we specify one that is
much slower than the A/B domains, one that is approximately the same, and one
that is much faster.
-}
createDomain
  vSystem{vName = "FreeSlow", vResetKind = Synchronous, vPeriod = hzToPeriod 10e3}
createDomain vSystem{vName = "Free", vResetKind = Synchronous, vPeriod = hzToPeriod 60e3}
createDomain
  vSystem{vName = "FreeFast", vResetKind = Synchronous, vPeriod = hzToPeriod 120e3}

{- | A signal that changes every cycle. It is used to simulate the RX's data when
it's receiving nothing. In theory this is white noise, in practise its a
rotating bit pattern.
-}
noise :: (KnownDomain dom) => Clock dom -> Signal dom (BitVector 64)
noise clk =
  let c = register clk noReset enableGen 0xdead_beef_ca55_e77e (rotateL <$> c <*> 1)
   in c

-- | Non-translatable, quick-to-simulate chain of \"dflipflops\"s
delaySeqN :: Natural -> a -> Signal dom a -> Signal dom a
delaySeqN 0 _dflt = id
delaySeqN 1 dflt = (dflt :-)
delaySeqN n dflt = go (Seq.fromFunction (fromIntegral n) (const dflt))
 where
  go :: Seq.Seq a -> Signal dom a -> Signal dom a
  go (l :<| rest) (s :- ss) = l :- go (rest :|> s) ss
  go _ _ = error "delaySeqN: impossible"
{-# ANN delaySeqN dontTranslate #-}
{-# OPAQUE delaySeqN #-}

{- | Very simple model of a GTH core. Is modelled as if the serial domains and
the word domains run at the same frequency. I.e.., it doesn't serialize at
all.
-}
gthCoreMock ::
  String ->
  -- | Number of cycles it takes for RX's domain to be stable
  Natural ->
  -- | Number of cycles it takes for TX's domain to be stable
  Natural ->
  -- | Number of cycles it takes for TX to be out of reset
  Natural ->
  -- Offset
  Index 8 ->
  GthCore tx rx ref free tx rx (Maybe (BitVector 64))
gthCoreMock
  _name
  nRxResetCycles
  nTxResetCycles
  nTxDoneCycles
  offset
  _channelName
  _clockPath
  _txOutClkSource
  rxSerial
  _rxSerial
  freeClk
  rstAll
  rstRx
  txWord
  _txCtrl
  _refClk =
    ( txSerial
    , txSerial
    , txClk
    , rxClk
    , rxWord
    , pack <$> txDone
    , pack <$> rxDone
    , pack <$> txActive
    , 0
    , 0
    , 0
    , 0
    )
   where
    rxWord =
      withClock rxClk
        $ WordAlign.aligner WordAlign.dealignLsbFirst (pure False) (pure offset)
        $ fromMaybe
        <$> noise rxClk
        <*> rxSerial

    txSerial = Just <$> txWord

    registerRx = register rxClk rxRstRx enableGen
    registerTx = register txClk txRstAll enableGen

    rxResetCounter = registerRx nRxResetCycles (predSatZeroNatural <$> rxResetCounter)
    txResetCounter = registerTx nTxResetCycles (predSatZeroNatural <$> txResetCounter)
    txDoneCounter = registerTx (nTxResetCycles + nTxDoneCycles) (predSatZeroNatural <$> txDoneCounter)

    rxDone = rxResetCounter .==. 0
    txActive = txResetCounter .==. 0
    txDone = txDoneCounter .==. 0

    txClk = clockGen
    rxClk = clockGen

    txRstAll = unsafeFromActiveHigh (unsafeSynchronizer freeClk txClk (unsafeToActiveHigh rstAll))
    rxRstAll = unsafeFromActiveHigh (unsafeSynchronizer freeClk rxClk (unsafeToActiveHigh rstAll))
    rxRstRx =
      unsafeOrReset
        rxRstAll
        (unsafeFromActiveHigh (unsafeSynchronizer freeClk rxClk (unsafeToActiveHigh rstRx)))

    predSatZeroNatural :: Natural -> Natural
    predSatZeroNatural 0 = 0
    predSatZeroNatural n = n - 1

data Input tx rx = Input
  { dat :: Signal tx (BitVector 64)
  , txReady :: Signal tx Bool
  , rxReady :: Signal rx Bool
  }

dut ::
  forall freeA freeB txA txB ref n.
  ( KnownDomain ref
  , HasSynchronousReset txA
  , HasDefinedInitialValues txA
  , HasSynchronousReset txB
  , HasDefinedInitialValues txB
  , HasSynchronousReset freeA
  , HasDefinedInitialValues freeA
  , HasSynchronousReset freeB
  , HasDefinedInitialValues freeB
  ) =>
  -- | Number of word clock cycles delay from A -> B
  Natural ->
  -- | Number of word clock cycles delay from B -> A
  Natural ->
  ResetManager.Config ->
  GthCore txA txB ref freeA txA txB (Maybe (Bytes n)) ->
  GthCore txB txA ref freeB txB txA (Maybe (Bytes n)) ->
  Clock freeA ->
  Reset freeA ->
  Clock freeB ->
  Reset freeB ->
  Input txA txB ->
  Input txB txA ->
  ( Transceiver.Output txA txB txA freeA (Maybe (Bytes n))
  , Transceiver.Output txB txA txB freeB (Maybe (Bytes n))
  )
dut
  abDelay
  baDelay
  resetManagerConfig
  gthCoreA
  gthCoreB
  freeClkA
  freeRstA
  freeClkB
  freeRstB
  inputA
  inputB = (outputA, outputB)
   where
    outputA =
      Transceiver.transceiverPrbsWith
        gthCoreA
        Transceiver.defConfig{Transceiver.resetManagerConfig}
        Transceiver.Input
          { clock = freeClkA
          , reset = freeRstA
          , refClock = error "A: refClock not used in simulation"
          , transceiverIndex = 0
          , channelName = "A"
          , clockPath = "clkA"
          , rxN = delaySeqN baDelay Nothing outputB.txN
          , rxP = error "A: rxP not used in simulation"
          , txData = inputA.dat
          , txReady = inputA.txReady
          , rxReady = inputA.rxReady
          }

    outputB =
      Transceiver.transceiverPrbsWith
        gthCoreB
        Transceiver.defConfig{Transceiver.resetManagerConfig}
        Transceiver.Input
          { clock = freeClkB
          , reset = freeRstB
          , refClock = error "B: refClock not used in simulation"
          , transceiverIndex = 1
          , channelName = "B"
          , clockPath = "clkB"
          , rxN = delaySeqN abDelay Nothing outputA.txN
          , rxP = error "B: rxP not used in simulation"
          , txData = inputB.dat
          , txReady = inputB.txReady
          , rxReady = inputB.rxReady
          }

type DutTestFunc txA txB free =
  Transceiver.Output txA txB txA free (Maybe (BitVector 64)) ->
  Transceiver.Output txB txA txB free (Maybe (BitVector 64)) ->
  PropertyT IO ()

type InputFunc txA txB free =
  Transceiver.Output txA txB txA free (Maybe (BitVector 64)) ->
  Input txA txB

dutRandomized ::
  forall txA txB free.
  ( HasSynchronousReset txA
  , HasDefinedInitialValues txA
  , HasSynchronousReset txB
  , HasDefinedInitialValues txB
  , HasSynchronousReset free
  , HasDefinedInitialValues free
  ) =>
  DutTestFunc txA txB free ->
  InputFunc txA txB free ->
  InputFunc txB txA free ->
  -- Input txA txB ->
  -- Input txB txA ->
  Proxy (free :: Domain) ->
  Property
dutRandomized f inputA inputB Proxy = property $ do
  -- Note that the maximum timeout should be below 'ResetManager.txTimeoutMs' and
  -- 'ResetManager.rxTimeoutMs'.
  let genTimeoutMax = Gen.word64 (Range.linear 0 (natToNum @(PeriodToCycles A (Microseconds 500))))
  rxStableA <- fromIntegral <$> forAll genTimeoutMax
  txStableA <- fromIntegral <$> forAll genTimeoutMax
  txDoneA <- fromIntegral <$> forAll genTimeoutMax
  rxStableB <- fromIntegral <$> forAll genTimeoutMax
  txStableB <- fromIntegral <$> forAll genTimeoutMax
  txDoneB <- fromIntegral <$> forAll genTimeoutMax

  -- XXX: Note that a single, static offset is generated. This is not realistic:
  --      in practise, the offset is random, and "determined" after resetting the
  --      rx subsystem. We currently don't rely on this behavior, due to the logic
  --      in "Bittide.Transceiver.WordAlign".
  aOffset <- forAll (genIndex Range.constantBounded)
  bOffset <- forAll (genIndex Range.constantBounded)

  -- A cable of 1km "stores" 42 words of 64 bits. In theory these links can be
  -- assymetric, although they rarely are in practise.
  abDelay <- fromIntegral <$> forAll (Gen.word64 (Range.linear 0 42))
  baDelay <- fromIntegral <$> forAll (Gen.word64 (Range.linear 0 42))

  let
    -- XXX: Randomization of the reset manager is possible, but greatly slows down
    --      simulation if done naively, as the two transceiver's reset managers can
    --      end up in a "dance" where they keep resetting each other. We should
    --      investigate how to randomize.
    resetManagerConfig = ResetManager.defConfig

    gthCoreA = gthCoreMock "A" rxStableA txStableA txDoneA aOffset
    gthCoreB = gthCoreMock "B" rxStableB txStableB txDoneB bOffset

  let
    (outputA, outputB) =
      dut
        @free
        @free
        @txA
        @txB
        @RefIsUnused
        @8
        abDelay
        baDelay
        resetManagerConfig
        gthCoreA
        gthCoreB
        (clockGen @free)
        (resetGen @free)
        (clockGen @free)
        (resetGen @free)
        (inputA outputA)
        (inputB outputB)

  f outputA outputB

-- | Tests whether the link is up within 500 milliseconds
testUp500ms :: DutTestFunc txA txB free
testUp500ms outputA outputB =
  case sampledAfterStable of
    [] -> failure
    (List.map snd -> ups) -> do
      let n = 100
      List.take n ups === List.replicate n True
 where
  linksUp = outputA.linkUp .&&. outputB.linkUp
  nCycles = fromIntegral (maxBound :: Index (PeriodToCycles Free (Milliseconds 500)))
  sampledLinksUp = sampleN nCycles linksUp
  sampledAfterStable = List.dropWhile (not . snd) (List.zip [(0 :: Int) ..] sampledLinksUp)

-- | Test whether the link is never up within 500 milliseconds
testNeitherUp500ms :: DutTestFunc txA txB free
testNeitherUp500ms outputA outputB =
  False === or (sampleN nCycles linksUp)
 where
  linksUp = outputA.linkUp .||. outputB.linkUp
  nCycles = fromIntegral (maxBound :: Index (PeriodToCycles Free (Milliseconds 500)))

-- | Test whether the link is never up within 500 milliseconds
testNotBothUp500ms :: DutTestFunc txA txB free
testNotBothUp500ms outputA outputB =
  False === or (sampleN nCycles linksUp)
 where
  linksUp = (outputA.linkUp .==. pure True) .&&. (outputB.linkUp .==. pure True)
  nCycles = fromIntegral (maxBound :: Index (PeriodToCycles Free (Milliseconds 500)))

-- Input that applies no (back)pressure on the link
noPressureInput :: InputFunc txA txB free
noPressureInput _ = Input{dat = complement <$> 0, txReady = pure True, rxReady = pure True}

-- Input that never sets 'txReady' to 'True'
noTxReadyInput :: InputFunc txA txB free
noTxReadyInput i = (noPressureInput i){txReady = pure False}

-- Input that never sets 'rxReady' to 'True'
noRxReadyInput :: InputFunc txA txB free
noRxReadyInput i = (noPressureInput i){rxReady = pure False}

{- | Check whether handshake works when there is no pressure on the link,
specialized to 'A', 'A', 'FreeSlow'.
-}
prop_noPressure_A_A_FreeSlow :: Property
prop_noPressure_A_A_FreeSlow =
  dutRandomized @A @A @FreeSlow testUp500ms noPressureInput noPressureInput Proxy

{- | Check whether handshake works when there is no pressure on the link,
specialized to 'A', 'A', 'Free'.
-}
prop_noPressure_A_A_Free :: Property
prop_noPressure_A_A_Free =
  dutRandomized @A @A @Free testUp500ms noPressureInput noPressureInput Proxy

{- | Check whether handshake works when there is no pressure on the link,
specialized to 'A', 'A', 'FreeFast'.
-}
prop_noPressure_A_A_FreeFast :: Property
prop_noPressure_A_A_FreeFast =
  dutRandomized @A @A @FreeFast testUp500ms noPressureInput noPressureInput Proxy

{- | Check whether handshake works when there is no pressure on the link,
specialized to 'A', 'B', 'Free'.
-}
prop_noPressure_A_B_Free :: Property
prop_noPressure_A_B_Free =
  dutRandomized @A @B @Free testUp500ms noPressureInput noPressureInput Proxy

{- | Check whether handshake works when there is no pressure on the link,
specialized to 'B', 'A', 'Free'.
-}
prop_noPressure_B_A_Free :: Property
prop_noPressure_B_A_Free =
  dutRandomized @B @A @Free testUp500ms noPressureInput noPressureInput Proxy

{- | Check whether neither handshake works when one of the transceivers never
indicates it's ready to the other.
-}
prop_noTxReady :: Property
prop_noTxReady =
  dutRandomized @A @A @Free testNeitherUp500ms noPressureInput noTxReadyInput Proxy

-- | Same as 'prop_noTxReady', but with the transceivers flipped
prop_noTxReadyFlipped :: Property
prop_noTxReadyFlipped =
  dutRandomized @A @A @Free testNeitherUp500ms noTxReadyInput noPressureInput Proxy

{- | Check whether neither node indicates it's up, when it never transitions to
sending user data.
-}
prop_noRxReady :: Property
prop_noRxReady =
  dutRandomized @A @A @Free testNeitherUp500ms noRxReadyInput noRxReadyInput Proxy

-- TODO: Add tests for actual data transmission. This currently only happens in
--       hardware, as we currently don't have the infrastructure to memory-efficiently
--       test multi-domain systems in Clash..

tests :: TestTree
tests =
  -- XXX: The number of tests we run is very low, due to the time it takes to
  --      execute them.
  testGroup
    "Transceiver"
    [ adjustOption (\_ -> HedgehogTestLimit (Just 100))
        $ testGroup
          "Slow tests"
          [ testPropertyNamed
              "prop_noPressure_A_A_FreeSlow"
              "prop_noPressure_A_A_FreeSlow"
              prop_noPressure_A_A_FreeSlow
          , testPropertyNamed
              "prop_noPressure_A_A_Free"
              "prop_noPressure_A_A_Free"
              prop_noPressure_A_A_Free
          , testPropertyNamed
              "prop_noPressure_A_A_FreeFast"
              "prop_noPressure_A_A_FreeFast"
              prop_noPressure_A_A_FreeFast
          , testPropertyNamed
              "prop_noPressure_A_B_Free"
              "prop_noPressure_A_B_Free"
              prop_noPressure_A_B_Free
          , testPropertyNamed
              "prop_noPressure_B_A_Free"
              "prop_noPressure_B_A_Free"
              prop_noPressure_B_A_Free
          ]
    , adjustOption (\_ -> HedgehogTestLimit (Just 10))
        $ testGroup
          "Very slow tests"
          -- These tests are "very slow" because they cannot exit early on some
          -- condition. Instead, they just wait for some deadline, and if they don't
          -- see an errornous condition by then, they pass.
          [ testPropertyNamed "prop_noTxReady" "prop_noTxReady" prop_noTxReady
          , testPropertyNamed "prop_noTxReadyFlipped" "prop_noTxReadyFlipped" prop_noTxReadyFlipped
          , testPropertyNamed "prop_noRxReady" "prop_noRxReady" prop_noRxReady
          ]
    ]
