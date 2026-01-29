-- SPDX-FileCopyrightText: 2026 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE OverloadedStrings #-}

module Tests.Protocols.Df.Extra where

import Clash.Prelude

import Clash.Hedgehog.Sized.BitVector
import Clash.Hedgehog.Sized.Unsigned
import Clash.Hedgehog.Sized.Vector
import Data.Maybe
import Data.String.Interpolate (i)
import Hedgehog (Gen, Property, Range, assert, cover, footnote, forAll, (===))
import Protocols
import Protocols.Df.Extra (skid)
import Protocols.Hedgehog (
  ExpectOptions (..),
  defExpectOptions,
  idWithModelSingleDomain,
  idWithModelSingleDomainT,
  propWithModelSingleDomain,
 )
import Protocols.Internal (circuitMonitor)
import Test.Tasty (TestTree)
import Test.Tasty.Hedgehog (testProperty)
import Test.Tasty.TH (testGroupGenerator)

import qualified Clash.Prelude as C
import qualified Data.List as L
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Protocols.Df as Df
import qualified Protocols.Df.Extra as Df
import qualified Prelude as P

smallInt :: Range Int
smallInt = Range.linear 0 10

genSmallInt :: Gen Int
genSmallInt =
  Gen.frequency
    [ (90, Gen.integral smallInt)
    , (10, Gen.constant (Range.lowerBound 99 smallInt))
    ]

genData :: Gen a -> Gen [a]
genData genA = do
  n <- genSmallInt
  Gen.list (Range.singleton n) genA

-- | Wrapper around 'skid' that discards the Ready signal
skidDropReady ::
  forall dom a.
  (NFDataX a, HiddenClockResetEnable dom) =>
  Circuit (Df dom a) (Df dom a)
skidDropReady = circuit $ \dfIn -> do
  (dfOut, _ready) <- skid -< dfIn
  idC -< dfOut

prop_skid :: Property
prop_skid =
  idWithModelSingleDomain
    @C.System
    defExpectOptions
    (genData genSmallInt)
    (C.exposeClockResetEnable id)
    (C.exposeClockResetEnable skidDropReady)

-- | Merges two BitVectors according to a mask.
mergeWithMask ::
  forall bv m.
  (KnownNat m, KnownNat bv) =>
  BitVector (bv * m) ->
  BitVector (bv * m) ->
  BitVector m ->
  BitVector (bv * m)
mergeWithMask (unpack -> old) (unpack -> new) (unpack -> mask) =
  pack (mux @(Vec m) @(BitVector bv) mask new old)

-- | Simply try reading the initial contents of a blockram
prop_fromBlockram :: Property
prop_fromBlockram =
  idWithModelSingleDomain @System
    defExpectOptions
    (genData (genUnsigned Range.linearBounded))
    (\_ _ _ -> model)
    top
 where
  mem = iterate d16 succ 0 :: Vec 16 Int

  dut :: forall dom. (HiddenClockResetEnable dom) => Circuit (Df dom (Unsigned 4)) (Df dom Int)
  dut = circuit $ \rd -> do
    wr <- Df.empty
    Df.fromBlockram (\ena -> withEnable ena (blockRam mem)) -< (rd, wr)

  top clk rst ena0 = withClockResetEnable @System clk rst ena0 dut
  model = fmap (mem !!)

-- | First write a new configuration to the blockram, then read it back
prop_fromBlockramWrites :: Property
prop_fromBlockramWrites = H.property $ do
  oldMem <- forAll $ genVec @16 $ Gen.integral Range.linearBounded
  newMem <- forAll $ genVec @16 $ Gen.integral Range.linearBounded
  let
    writes = L.zip [0 ..] (toList newMem)
    model = fmap (newMem !!)

    dut :: forall dom. (HiddenClockResetEnable dom) => Circuit (Df dom (Unsigned 4)) (Df dom Int)
    dut = circuit $ \rd0 -> do
      wr <- Df.drive def (fmap Just writes)
      rd1 <- Df.stall def{resetCycles = 0} StallWithNack [100] -< rd0
      Df.fromBlockram (\ena -> withEnable ena (blockRam oldMem)) -< (rd1, wr)

    top clk rst ena0 = withClockResetEnable @System clk rst ena0 dut

  idWithModelSingleDomainT @System
    defExpectOptions
    (genData (genUnsigned Range.linearBounded))
    (\_ _ _ -> model)
    top

-- | Write a configuration to the blockram with byte enables, then read it back
prop_fromBlockramWithMaskWrites :: Property
prop_fromBlockramWithMaskWrites = H.property $ do
  oldMem <- forAll $ genVec @8 genDefinedBitVector
  newValues <- forAll $ genVec genDefinedBitVector
  masks <- forAll $ genVec genDefinedBitVector

  let
    newMem = zipWith3 mergeWithMask oldMem newValues masks

    -- First write old memory with full masks, then write new values with given masks
    writes =
      L.zip3 [0 ..] (L.repeat maxBound) (toList oldMem)
        <> L.zip3 [0 ..] (toList masks) (toList newValues)
    model = fmap (newMem !!)

    dut ::
      forall dom. (HiddenClockResetEnable dom) => Circuit (Df dom (Unsigned 3)) (Df dom (BitVector 32))
    dut = circuit $ \rd0 -> do
      wr <- Df.drive def{resetCycles = 0} (fmap Just writes)
      rd1 <- Df.stall def{resetCycles = 0} StallWithNack [50] -< rd0
      Df.fromBlockramWithMask (exposeEnable $ blockRamByteAddressableU d8) -< (rd1, wr)

    top clk rst ena0 = withClockResetEnable @System clk rst ena0 dut

  idWithModelSingleDomainT @System
    defExpectOptions{eoStopAfterEmpty = Just 100}
    (genData (genUnsigned Range.linearBounded))
    (\_ _ _ -> model)
    top

prop_fromDSignal :: Property
prop_fromDSignal =
  idWithModelSingleDomain @System
    defExpectOptions
    (genData genSmallInt)
    (\_ _ _ -> id)
    dut
 where
  reference clk ena = withClock clk $ withEnable ena $ delayN d10 (0 :: Int)
  dut clk rst ena = Df.fromDSignal clk rst ena (reference clk)

{- | Verify that the circuit always produces less backpressure than it receives
This should check that the circuit can run at without more stalls than strictly necessary
-}
prop_fromDSignalBackpressure :: Property
prop_fromDSignalBackpressure = H.property $ do
  inputData <- forAll $ Gen.list (Range.linear 0 20) $ Gen.maybe $ pure ()
  stalls <- forAll (Gen.list (Range.linear 0 10) (Gen.integral (Range.linear 0 10)))
  let
    reference clk ena = withClock @System clk $ withEnable ena $ delayN d5 ()
    dut clk rst ena = Df.fromDSignal clk rst ena (reference clk)
    top clk rst ena = circuit $ do
      (drive1, driveMonitor) <- circuitMonitor <| driveC def inputData
      (sample1, sampleMonitor) <- circuitMonitor <| dut clk rst ena -< drive1
      withReset rst Df.consume <| Df.stall def{resetCycles = 0} StallCycle stalls -< sample1
      idC -< (driveMonitor, sampleMonitor)

    isStalled (fwd, (Ack bwd)) = isJust fwd && not bwd
    isTransfer (fwd, (Ack bwd)) = isJust fwd && bwd
    isIdle (fwd, _) = isNothing fwd

    getStalls = L.scanl (\acc inps -> if isStalled inps then succ acc else acc) (0 :: Int)
    getTransfers = L.foldl (\acc inps -> if isTransfer inps then succ acc else acc) (0 :: Int)
    getIdles = L.foldl (\acc inps -> if isIdle inps then succ acc else acc) (0 :: Int)
    (driveSignals, sampleSignals) = sampleC def{timeoutAfter = 200} (top clockGen resetGen enableGen)
    driveStalls = getStalls driveSignals
    sampleStalls = getStalls sampleSignals

  assert (getTransfers driveSignals == L.length (catMaybes inputData))
  assert (getTransfers sampleSignals == L.length (catMaybes inputData))
  cover 1 "Idle cycles in driver" (getIdles driveSignals > 0)
  cover 1 "Idle cycles in sampler" (getIdles sampleSignals > 0)

  footnote
    $ [i|Drive stalls: #{show (runLengthEncode driveStalls)} \nSample stalls: #{show (runLengthEncode sampleStalls)}|]
  assert $ and $ L.zipWith (<=) driveStalls sampleStalls

-- | Utility function to run-length encode a list
runLengthEncode :: (Eq a) => [a] -> [(a, Int)]
runLengthEncode = go Nothing
 where
  go (Just (a, n)) (x : xs)
    | a == x = go (Just (a, n + 1)) xs
    | otherwise = (a, n) : go (Just (x, 1)) xs
  go Nothing (x : xs) = go (Just (x, 1)) xs
  go (Just s) [] = [s]
  go Nothing [] = []

prop_iterate :: Property
prop_iterate =
  propWithModelSingleDomain
    defExpectOptions{eoResetCycles = 10}
    gen
    (\_ _ _ -> model)
    dut
    prop
 where
  f = (+ 1) :: Int -> Int
  model = const $ L.take 100 (P.iterate f 0)

  -- After 100 cycles stall comes out of reset and stalls communication to
  -- terminate the simulation.
  dut =
    exposeClockResetEnable
      (Df.stall def{resetCycles = 100} StallCycle [1000] <| Df.iterate f 0 :: Circuit () (Df System Int))
  gen = pure ()
  prop expected actual = do
    let len = L.length actual
    footnote [i|Expected length: Actual length: #{show len}|]
    assert (len > 10)
    L.take len expected === actual

-- Start of shamelessly copied code from bittide

{- | Version of 'blockRamByteAddressable' with undefined initial contents. It is similar
to 'blockRam' with the addition that it takes a byte select signal that controls
which nBytes at the write address are updated.
-}
blockRamByteAddressableU ::
  forall dom memDepth n m addr.
  ( HiddenClockResetEnable dom
  , Enum addr
  , NFDataX addr
  , KnownNat memDepth
  , 1 <= memDepth
  , KnownNat n
  , KnownNat m
  ) =>
  -- | Memory depth
  SNat memDepth ->
  -- | Read address.
  Signal dom addr ->
  -- | Write operation.
  Signal dom (Maybe (addr, BitVector (n * m))) ->
  -- | Byte enables that determine which nBytes get replaced.
  Signal dom (BitVector n) ->
  -- | Data at read address (1 cycle delay).
  Signal dom (BitVector (n * m))
blockRamByteAddressableU SNat readAddr newEntry byteSelect =
  pack <$> readBytes
 where
  writeBytes = unbundle $ splitWriteInBytes <$> newEntry <*> byteSelect
  readBytes = bundle $ ram readAddr <$> writeBytes
  ram = blockRamU NoClearOnReset (SNat @memDepth)

{- | Takes singular write operation (Maybe (Index maxIndex, writeData)) and splits it up
according to a supplied byteselect bitvector into a vector of byte sized write operations
(Maybe (Index maxIndex, Byte)).
-}
splitWriteInBytes ::
  forall addr m n.
  (KnownNat n, KnownNat m) =>
  -- | Incoming write operation.
  Maybe (addr, BitVector (n * m)) ->
  -- | Incoming byte enables.
  BitVector m ->
  -- | Per byte write operation.
  Vec m (Maybe (addr, BitVector n))
splitWriteInBytes (Just (addr, writeData)) byteSelect =
  (\m d -> if m then Just d else Nothing)
    <$> unpack byteSelect
    <*> fmap (addr,) (unpack writeData)
splitWriteInBytes Nothing _ = repeat Nothing

-- End of shamelessly copied code from bittide

tests :: TestTree
tests = $(testGroupGenerator)
