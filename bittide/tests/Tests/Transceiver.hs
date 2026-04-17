-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Tests.Transceiver where

import Clash.Explicit.Prelude hiding (PeriodToCycles, assert)
import Clash.Prelude (withClock)
import Hedgehog

import Bittide.Arithmetic.Time (PeriodToCycles)
import Clash.Annotations.Primitive (dontTranslate)
import Clash.Cores.Xilinx.Gth (GthCore)
import Clash.Hedgehog.Sized.Index (genIndex)
import Clash.Signal.Internal (Signal ((:-)))
import Data.Proxy (Proxy (Proxy))
import Data.Sequence (Seq ((:<|), (:|>)))
import Data.String (IsString (fromString))
import Test.Tasty (TestTree, adjustOption, testGroup)
import Test.Tasty.Hedgehog (HedgehogTestLimit (HedgehogTestLimit), testPropertyNamed)
import "extra" Data.List.Extra (splitOn)

import qualified Bittide.Transceiver as Transceiver
import qualified Bittide.Transceiver.ResetManager as ResetManager
import qualified Bittide.Transceiver.WordAlign as WordAlign
import qualified Clash.Cores.Xilinx.Gth as Gth
import qualified Data.Sequence as Seq
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Language.Haskell.TH as TH

createDomain vSystem{vName = "RefIsUnused"}

-- Note that these domains are a factor of ~5_000 slower than we use in practice. We
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
it's receiving nothing. In theory this is white noise, in practice its a
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
  GthCore tx tx rx rx ref free tx rx
gthCoreMock
  _name
  nRxResetCycles
  nTxResetCycles
  nTxDoneCycles
  offset
  Gth.CoreInput
    { gthrxIn = rx
    , gtwizResetClkFreerunIn = freeClk
    , gtwizResetAllIn = rstAll
    , gtwizResetTxPllAndDatapathIn = rstTxPllAndDatapath
    , gtwizResetTxDatapathIn = rstTxDatapath
    , gtwizResetRxPllAndDatapathIn = rstRxPllAndDatapath
    , gtwizResetRxDatapathIn = rstRxDatapath
    , gtwizUserdataTxIn = txWord
    , txusrclk2In = tx2Clk
    , rxusrclk2In = rx2Clk
    } =
    Gth.CoreOutput
      { gthtxOut = SimOnly txWord
      , gthtxnOut = pure 0
      , gthtxpOut = pure 0
      , txoutclkOut = txOutClk
      , rxoutclkOut = rxOutClk
      , gtwizUserdataRxOut = rxWord
      , gtwizResetTxDoneOut = pack <$> txDone
      , gtwizResetRxDoneOut = pack <$> rxDone
      , txpmaresetdoneOut = error "txpmaresetdone_out unused in test"
      , rxpmaresetdoneOut = error "rxpmaresetdone_out unused in test"
      , rxctrl0Out = 0
      , rxctrl1Out = 0
      , rxctrl2Out = 0
      , rxctrl3Out = 0
      }
   where
    rxWord =
      withClock rxClk
        $ WordAlign.aligner WordAlign.dealignLsbFirst (pure False) (pure offset)
        $ Gth.unSimOnly rx

    registerRx = register rxClk rxRst enableGen
    registerTx = register txClk txRst enableGen

    rxResetCounter = registerRx nRxResetCycles (predSatZeroNatural <$> rxResetCounter)
    txDoneCounter = registerTx (nTxResetCycles + nTxDoneCycles) (predSatZeroNatural <$> txDoneCounter)

    rxDone = rxResetCounter .==. 0
    txDone = txDoneCounter .==. 0

    txOutClk = clockGen
    rxOutClk = clockGen

    txClk = tx2Clk
    rxClk = rx2Clk

    rxRstAll = unsafeFromActiveHigh (unsafeSynchronizer freeClk rxClk (unsafeToActiveHigh rstAll))
    rxRstRxPllAndDatapath =
      unsafeFromActiveHigh
        (unsafeSynchronizer freeClk rxClk (unsafeToActiveHigh rstRxPllAndDatapath))
    rxRstRxDatapath =
      unsafeFromActiveHigh (unsafeSynchronizer freeClk rxClk (unsafeToActiveHigh rstRxDatapath))
    rxRst = rxRstAll `unsafeOrReset` rxRstRxPllAndDatapath `unsafeOrReset` rxRstRxDatapath

    txRstAll = unsafeFromActiveHigh (unsafeSynchronizer freeClk txClk (unsafeToActiveHigh rstAll))
    txRstTxPllAndDatapath =
      unsafeFromActiveHigh
        (unsafeSynchronizer freeClk txClk (unsafeToActiveHigh rstTxPllAndDatapath))
    txRstTxDatapath =
      unsafeFromActiveHigh (unsafeSynchronizer freeClk txClk (unsafeToActiveHigh rstTxDatapath))
    txRst = txRstAll `unsafeOrReset` txRstTxPllAndDatapath `unsafeOrReset` txRstTxDatapath

    predSatZeroNatural :: Natural -> Natural
    predSatZeroNatural 0 = 0
    predSatZeroNatural n = n - 1

data Input tx free = Input
  { dat :: Signal tx (BitVector 64)
  , channelReset :: Reset free
  }

dut ::
  forall freeA freeB txA txB ref.
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
  GthCore txA txA txB txB ref freeA txA txB ->
  GthCore txB txB txA txA ref freeB txB txA ->
  Clock freeA ->
  Reset freeA ->
  Clock freeB ->
  Reset freeB ->
  Input txA freeA ->
  Input txB freeB ->
  ( Transceiver.Output txA txB txA txB txA freeA
  , Transceiver.Output txB txA txB txA txB freeB
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
          , clockTx1 = clockGen
          , clockTx2 = clockGen
          , txActive = pure 1 -- TODO: a better simulation of this
          , clockRx1 = clockGen
          , clockRx2 = clockGen
          , rxActive = pure 1 -- TODO: a better simulation of this
          , channelName = "A"
          , clockPath = "A clkA"
          , rxSim = delaySeqN baDelay 0 <$> outputB.txSim
          , rxN = error "A: rxN not used in simulation"
          , rxP = error "A: rxP not used in simulation"
          , channelReset = inputA.channelReset
          , txData = inputA.dat
          }

    outputB =
      Transceiver.transceiverPrbsWith
        gthCoreB
        Transceiver.defConfig{Transceiver.resetManagerConfig}
        Transceiver.Input
          { clock = freeClkB
          , reset = freeRstB
          , refClock = error "B: refClock not used in simulation"
          , clockTx1 = clockGen
          , clockTx2 = clockGen
          , txActive = pure 1
          , clockRx1 = clockGen
          , clockRx2 = clockGen
          , rxActive = pure 1
          , channelName = "B"
          , clockPath = "B clkB"
          , rxSim = delaySeqN abDelay 0 <$> outputA.txSim
          , rxN = error "B: rxN not used in simulation"
          , rxP = error "B: rxP not used in simulation"
          , channelReset = inputB.channelReset
          , txData = inputB.dat
          }

type DutTestFunc txA txB free =
  Transceiver.Output txA txB txA txB txA free ->
  Transceiver.Output txB txA txB txA txB free ->
  PropertyT IO ()

type InputFunc txA txB free =
  Transceiver.Output txA txB txA txB txA free ->
  Input txA free

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
  --      in practice, the offset is random, and "determined" after resetting the
  --      rx subsystem. We currently don't rely on this behavior, due to the logic
  --      in "Bittide.Transceiver.WordAlign".
  aOffset <- forAll (genIndex Range.constantBounded)
  bOffset <- forAll (genIndex Range.constantBounded)

  -- A cable of 1km "stores" 42 words of 64 bits. In theory these links can be
  -- asymmetric, although they rarely are in practice.
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

-- | Input with no applied 'channelReset' and arbitrary TX data.
noResetInput :: (KnownDomain free) => InputFunc txA txB free
noResetInput _ = Input{dat = maxBound, channelReset = noReset}

{- | Observation window (in @free@-domain cycles) used by 'testUp500ms' and
'testNeverUp500ms'. 500 ms is comfortably above the link-up latency produced
by 'dutRandomized'.
-}
window500ms :: forall free. (KnownDomain free) => Proxy free -> Int
window500ms Proxy =
  fromIntegral (maxBound :: Index (PeriodToCycles free (Milliseconds 500)))

-- | Input that holds 'channelReset' asserted forever.
heldResetInput :: (KnownDomain free) => InputFunc txA txB free
heldResetInput _ =
  Input{dat = maxBound, channelReset = unsafeFromActiveHigh (pure True)}

-- | Both sides reach @rx/txDataInitDone@ within 500 ms.
testUp500ms ::
  forall txA txB free. (KnownDomain free) => DutTestFunc txA txB free
testUp500ms outputA outputB =
  assert (or (sampleN (window500ms (Proxy @free)) allUp))
 where
  allUp =
    outputA.rxDataInitDoneFree
      .&&. outputB.rxDataInitDoneFree
      .&&. outputA.txDataInitDoneFree
      .&&. outputB.txDataInitDoneFree

-- | Neither side ever reaches @rxDataInitDone@ within 500 ms.
testNeverUp500ms ::
  forall txA txB free. (KnownDomain free) => DutTestFunc txA txB free
testNeverUp500ms outputA outputB = do
  let n = window500ms (Proxy @free)
  assert (not (or (sampleN n outputA.rxDataInitDoneFree)))
  assert (not (or (sampleN n outputB.rxDataInitDoneFree)))

-- | With no 'channelReset' asserted on either side, the link must come up.
prop_bothDeasserted :: Property
prop_bothDeasserted =
  dutRandomized @A @A @Free testUp500ms noResetInput noResetInput Proxy

-- | If A's 'channelReset' is held asserted forever, neither side comes up.
prop_heldOnA :: Property
prop_heldOnA =
  dutRandomized @A @A @Free testNeverUp500ms heldResetInput noResetInput Proxy

-- | If B's 'channelReset' is held asserted forever, neither side comes up.
prop_heldOnB :: Property
prop_heldOnB =
  dutRandomized @A @A @Free testNeverUp500ms noResetInput heldResetInput Proxy

-- | If both sides hold 'channelReset' asserted forever, neither side comes up.
prop_heldOnBoth :: Property
prop_heldOnBoth =
  dutRandomized @A @A @Free testNeverUp500ms heldResetInput heldResetInput Proxy

-- TODO: Add tests for actual data transmission. This currently only happens in
--       hardware, as we currently don't have the infrastructure to memory-efficiently
--       test multi-domain systems in Clash..

testPropertyThName :: TH.Name -> Property -> TestTree
testPropertyThName thName = testPropertyNamed funcName (fromString funcName)
 where
  lastMaybe :: [a] -> Maybe a
  lastMaybe [] = Nothing
  lastMaybe [x] = Just x
  lastMaybe (_ : xs) = lastMaybe xs

  funcName = case lastMaybe (splitOn "." (show thName)) of
    Just x -> x
    Nothing -> show thName

tests :: TestTree
tests =
  -- XXX: The number of tests we run is very low, due to the time it takes to
  --      execute them.
  adjustOption (\_ -> HedgehogTestLimit (Just 10))
    $ testGroup
      "Transceiver"
      [ testPropertyThName 'prop_bothDeasserted prop_bothDeasserted
      , testPropertyThName 'prop_heldOnA prop_heldOnA
      , testPropertyThName 'prop_heldOnB prop_heldOnB
      , testPropertyThName 'prop_heldOnBoth prop_heldOnBoth
      ]
