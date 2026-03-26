-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE OverloadedStrings #-}

module Tests.Ringbuffer where

import Bittide.DoubleBufferedRam (blockRamByteAddressableU)
import Bittide.Ringbuffer
import Bittide.SharedTypes
import Clash.Hedgehog.Sized.BitVector (genDefinedBitVector)
import Clash.Prelude hiding (delay)
import Hedgehog
import Protocols
import Protocols.Hedgehog (defExpectOptions)
import Test.Tasty
import Test.Tasty.Hedgehog (testProperty)
import Test.Tasty.TH (testGroupGenerator)

import qualified Data.List as L
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Protocols.MemoryMap as Mm
import qualified Protocols.Wishbone as Wb
import qualified Protocols.Wishbone.Standard.Hedgehog as Wb

-- | Tests that we don't get any backpressure when writing to the ringbuffer.
prop_transmitRingbufferTermination :: Property
prop_transmitRingbufferTermination = property $ do
  writes <- forAll genInputs
  let
    prim ena = withEnable ena blockRamByteAddressableU
    dut =
      withClockResetEnable clockGen resetGen enableGen
        $ withLittleEndian
        $ Mm.unMemmap
        $ transmitRingbufferWb @System @6 prim d16
        |> applyC (const ()) (const ())
    results = filter hasBusRequest $ Wb.sampleUnfiltered eOpts (Wb.driveStandard eOpts writes) dut

    -- fold over all results and track that every transaction is immediately terminated
    checkTransaction acc (m2s, s2m)
      | m2s.writeEnable && m2s.addr < 32 = acc && s2m.acknowledge -- In range write always acknowledges
      | m2s.writeEnable = acc && s2m.err -- Out of range write always errors
      | otherwise = acc && s2m.err -- Reads always errors
    checkResult = L.foldl checkTransaction True results

  -- Set some soft boundaries
  cover 50 "Multiple transactions" (not $ null results)
  cover 50 "At least 50% of tests have acknowledged writes"
    $ any (\(m2s, s2m) -> m2s.writeEnable && s2m.acknowledge) results
  cover 50 "At least 50% of tests have errored reads"
    $ any (\(m2s, s2m) -> not m2s.writeEnable && s2m.err) results

  -- Print responses
  footnoteShow $ fmap snd results
  assert checkResult
 where
  eOpts = defExpectOptions

  genInputs :: Gen [(Wb.WishboneMasterRequest 6 4, Int)]
  genInputs = Gen.list (Range.linear 0 32) $ do
    op <-
      Gen.choice
        [ Wb.Read <$> genDefinedBitVector <*> genDefinedBitVector
        , Wb.Write <$> genDefinedBitVector <*> genDefinedBitVector <*> genDefinedBitVector
        ]
    delay <- Gen.integral (Range.linear 0 10)
    pure (op, delay)

-- | Helper function to check if there is an active request on the bus.
hasBusRequest :: (Wb.WishboneM2S addressBits dataBytes, Wb.WishboneS2M dataBytes) -> Bool
hasBusRequest (m2s, _) = Wb.busCycle m2s && Wb.strobe m2s

hasUnterminatedRequest :: (Wb.WishboneM2S addressBits dataBytes, Wb.WishboneS2M dataBytes) -> Bool
hasUnterminatedRequest (m2s, s2m) = m2s.busCycle && m2s.strobe && not (Wb.hasTerminateFlag s2m)

-- | Tests that we don't get any backpressure when reading from the ringbuffer.
prop_receiveRingbufferTermination :: Property
prop_receiveRingbufferTermination = property $ do
  readOps <- forAll genInputs
  let
    -- Wrap blockRamByteAddressableU to match the signature expected by receiveRingbufferWb
    -- (which uses fromBlockRam, not fromBlockRamWithMask)
    memDepth = d16
    prim ena = withEnable ena $ blockRamU NoClearOnReset memDepth
    dut =
      withClockResetEnable clockGen resetGen enableGen
        $ withLittleEndian
        $ Mm.unMemmap
        $ circuit
        $ \wb -> do
          receiveRingbufferWb @System @6 prim memDepth -< (wb, Fwd (pure 0))
    results = filter hasBusRequest $ Wb.sampleUnfiltered eOpts (Wb.driveStandard eOpts readOps) dut

    -- fold over all results and track that every transaction is immediately terminated
    checkTransaction (acc, (dur :: Int)) (m2s, s2m)
      | hasUnterminatedRequest (m2s, s2m) = (acc && dur < 2, dur + 1) -- Transaction is active, increment duration
      | not m2s.writeEnable && m2s.addr < 32 = (acc && s2m.acknowledge, 0) -- In range read always acknowledges
      | not m2s.writeEnable = (acc && s2m.err, 0) -- Out of range read always errors
      | m2s.writeEnable = (acc && s2m.err, 0) -- Writes always error
      | otherwise = (acc, 0) -- No request.
    checkResult = fst $ L.foldl checkTransaction (True, 0) results

  -- Set some soft boundaries
  -- helpers
  let
    unackedRead (m2s, s2m) = not m2s.writeEnable && not (Wb.hasTerminateFlag s2m)
    ackedRead (m2s, s2m) = not m2s.writeEnable && s2m.acknowledge
    erroredWrite (m2s, s2m) = m2s.writeEnable && s2m.err

  -- coverstatements
  cover 50 "Multiple transactions" (not $ null results)
  cover 50 "At least 50% of tests unacknowledged reads" $ any unackedRead results
  cover 50 "At least 50% of tests have acknowledged reads" $ any ackedRead results
  cover 50 "At least 50% of tests have errored writes" $ any erroredWrite results

  -- Print responses
  footnoteShow $ fmap snd results
  assert checkResult
 where
  eOpts = defExpectOptions

  genInputs :: Gen [(Wb.WishboneMasterRequest 6 4, Int)]
  genInputs = Gen.list (Range.linear 0 32) $ do
    op <-
      Gen.choice
        [ Wb.Read <$> genDefinedBitVector <*> genDefinedBitVector
        , Wb.Write <$> genDefinedBitVector <*> genDefinedBitVector <*> genDefinedBitVector
        ]
    delay <- Gen.integral (Range.linear 0 10)
    pure (op, delay)

tests :: TestTree
tests = $(testGroupGenerator)
