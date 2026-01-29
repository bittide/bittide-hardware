-- SPDX-FileCopyrightText: 2026 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE OverloadedStrings #-}
module Tests.Protocols.Df.Extra where

import Clash.Prelude
import Data.Maybe
import Protocols
import Protocols.Internal(circuitMonitor)
import Protocols.Hedgehog
import Clash.Hedgehog.Sized.Vector
import Clash.Hedgehog.Sized.BitVector
import Hedgehog
import Test.Tasty (TestTree)
import Test.Tasty.Hedgehog (testProperty)
import Test.Tasty.TH (testGroupGenerator)
import Data.String.Interpolate (i)

import qualified Data.List as L
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Clash.Signal.Delayed as D
import qualified Protocols.Df as Df
import qualified Protocols.Df.Extra as Df
import qualified Prelude as P

-- | Merges two BitVectors according to a mask.
mergeWithMask ::
  forall bv m . (KnownNat m, KnownNat bv) =>
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
  genData
  (\_ _ _ -> model)
  top
 where
  mem = iterate d16 succ 0 :: Vec 16 Int

  dut :: forall dom . HiddenClockResetEnable dom => Circuit (Df dom (Unsigned 4)) (Df dom Int)
  dut = circuit $ \rd -> do
    wr <- Df.empty
    Df.fromBlockram (\ ena -> withEnable ena (blockRam mem)) -< (rd, wr)

  top clk rst ena0 = withClockResetEnable @System clk rst ena0 dut

  genData :: Gen [Unsigned 4]
  genData = Gen.list (Range.linear 0 100) $  Gen.integral Range.linearBounded

  model = fmap (mem !!)

-- | First write a new configuration to the blockram, then read it back
prop_fromBlockramWrites :: Property
prop_fromBlockramWrites = property $ do
  (oldMem :: Vec 16 Int) <- forAll $ genVec $ Gen.integral Range.linearBounded
  (newMem :: Vec 16 Int) <- forAll $ genVec $ Gen.integral Range.linearBounded
  let
    writes = L.zip [0..] (toList newMem)
    model = fmap (newMem !!)

    dut :: forall dom . HiddenClockResetEnable dom => Circuit (Df dom (Unsigned 4)) (Df dom Int)
    dut = circuit $ \rd0 -> do
      wr <- Df.drive def (fmap Just writes)
      rd1 <- Df.stall def{resetCycles = 0} StallWithNack [100] -< rd0
      Df.fromBlockram (\ ena -> withEnable ena (blockRam oldMem)) -< (rd1, wr)

    top clk rst ena0 = withClockResetEnable @System clk rst ena0 dut

  idWithModelSingleDomainT @System
    defExpectOptions
    genData
    (\_ _ _ -> model)
    top
 where
  genData :: Gen ([Unsigned 4])
  genData = do
    reads <- Gen.list (Range.linear 0 100) $ do
      addr <- Gen.integral Range.linearBounded
      pure addr
    pure reads

-- | Write a configuration to the blockram with byte enables, then read it back
prop_fromBlockramWithMaskWrites :: Property
prop_fromBlockramWithMaskWrites = property $ do
  (oldMem :: Vec 8 (BitVector 32)) <- forAll $ genVec genDefinedBitVector
  (newValues :: Vec 8 (BitVector 32)) <- forAll $ genVec genDefinedBitVector
  (masks :: Vec 8 (BitVector 4)) <- forAll $ genVec genDefinedBitVector

  let
    newMem = zipWith3 mergeWithMask oldMem newValues masks
    newMemBytes = fmap unpack newMem :: Vec 8 (Vec 4 (BitVector 8))

    -- First write old memory with full masks, then write new values with given masks
    writes =
      L.zip3 [0..] (L.repeat maxBound) (toList oldMem)  <>
      L.zip3 [0..] (toList masks) (toList newValues)
    model = fmap (newMem !!)

    dut :: forall dom . HiddenClockResetEnable dom => Circuit (Df dom (Unsigned 3)) (Df dom (BitVector 32))
    dut = circuit $ \rd0 -> do
      wr <- Df.drive def{resetCycles = 0} (fmap Just writes)
      rd1 <- Df.stall def{resetCycles = 0} StallWithNack [50] -< rd0
      Df.fromBlockramWithMask (exposeEnable $ blockRamByteAddressableU d8) -< (rd1, wr)

    top clk rst ena0 = withClockResetEnable @System clk rst ena0 dut

  idWithModelSingleDomainT @System
    defExpectOptions{eoStopAfterEmpty = Just 100}
    genData
    (\_ _ _ -> model)
    top
 where
  genData :: Gen ([Unsigned 3])
  genData = do
    reads <- Gen.list (Range.linear 0 10) $ do
      addr <- Gen.integral Range.linearBounded
      pure addr
    pure reads

prop_fromDSignal :: Property
prop_fromDSignal =
  idWithModelSingleDomain @System
  defExpectOptions
  gen
  (\_ _ _ -> id)
  dut
 where
  reference clk ena = withClock clk $ withEnable ena $ delayN d10 (0 :: Int)
  dut clk rst ena = Df.fromDSignal clk rst ena (reference clk)
  gen = Gen.list (Range.linear 0 100) (Gen.integral Range.linearBounded)

-- | Verify that the circuit always produces less backpressure than it receives
-- This should check that the circuit can run at without more stalls than strictly necessary
prop_fromDSignalBackpressure :: Property
prop_fromDSignalBackpressure = property $ do
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

    getStalls = L.scanl (\ acc inps -> if isStalled inps then succ acc else acc) (0 :: Int)
    getTransfers = L.foldl (\ acc inps -> if isTransfer inps then succ acc else acc) (0 :: Int)
    getIdles = L.foldl (\ acc inps -> if isIdle inps then succ acc else acc) (0 :: Int)
    (driveSignals, sampleSignals) = sampleC def{timeoutAfter = 200} (top clockGen resetGen enableGen)
    driveStalls = getStalls driveSignals
    sampleStalls = getStalls sampleSignals

  assert (getTransfers driveSignals == L.length (catMaybes inputData))
  assert (getTransfers sampleSignals == L.length (catMaybes inputData))
  cover 1 "Idle cycles in driver" (getIdles driveSignals > 0)
  cover 1 "Idle cycles in sampler" (getIdles sampleSignals > 0)

  footnote $ [i|Drive stalls: #{show (runLengthEncode driveStalls)} \nSample stalls: #{show (runLengthEncode sampleStalls)}|]
  assert $ and $ L.zipWith (<=) driveStalls sampleStalls

-- | Utility function to run-length encode a list
runLengthEncode :: Eq a => [a] -> [(a, Int)]
runLengthEncode = go Nothing
 where
  go (Just (a, n)) (x:xs)
    | a == x = go (Just (a, n + 1)) xs
    | otherwise = (a, n) : go (Just (x, 1)) xs
  go Nothing (x:xs) = go (Just (x, 1)) xs
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
  f = (+1) :: Int -> Int
  model = const $ L.take 100 (P.iterate f 0)

  -- After 100 cycles stall comes out of reset and stalls communication to
  -- terminate the simulation.
  dut = exposeClockResetEnable (Df.stall def{resetCycles = 100} StallCycle [1000] <| Df.iterate f 0 :: Circuit () (Df System Int))
  gen = pure ()
  prop expected actual = do
    let len = L.length actual
    footnote [i|Expected length: Actual length: #{show len}|]
    assert (len > 10)
    L.take len expected === actual

tests :: TestTree
tests = $(testGroupGenerator)

-- Shamelessly copied from bittide

{- | Version of 'blockRamByteAddressable' with undefined initial contents. It is similar
to 'blockRam' with the addition that it takes a byte select signal that controls
which nBytes at the write address are updated.
-}
blockRamByteAddressableU ::
  forall dom memDepth n m addr.
  (HiddenClockResetEnable dom, Enum addr, NFDataX addr, KnownNat memDepth, 1 <= memDepth, KnownNat n, KnownNat m) =>
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
splitWriteInBytes (Just (addr, writeData)) byteSelect = (\m d -> if m then Just d else Nothing) <$> unpack byteSelect <*> fmap (addr,) (unpack writeData)
splitWriteInBytes Nothing _ = repeat Nothing
