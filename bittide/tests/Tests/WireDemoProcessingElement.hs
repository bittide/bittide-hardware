-- SPDX-FileCopyrightText: 2026 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE OverloadedStrings #-}

module Tests.WireDemoProcessingElement where

import Clash.Prelude
import Protocols
import Protocols.Experimental.Simulate (SimulationConfig (..), driveC, sampleC)

import Bittide.WireDemoProcessingElement (WriteHoldCycles, wireDemoPe)

import Clash.Hedgehog.Sized.BitVector (genDefinedBitVector)
import Clash.Hedgehog.Sized.Unsigned (genUnsigned)
import Hedgehog (Property)
import Test.Tasty (TestTree)
import Test.Tasty.Hedgehog (testProperty)
import Test.Tasty.TH (testGroupGenerator)

import qualified Data.List as L
import qualified Data.String.Interpolate as Str
import qualified Hedgehog as H
import qualified Hedgehog.Range as Range

prop_WireDemoPe :: Property
prop_WireDemoPe = H.property $ do
  peResetCycle <- H.forAll $ genUnsigned (Range.linear 0 80)
  dna <- H.forAll $ genDefinedBitVector

  let
    dut ::
      Circuit
        (CSignal System (BitVector 64))
        (CSignal System (BitVector 64), CSignal System (Maybe (BitVector 64)))
    dut =
      withClock clockGen
        $ circuit
        $ \cntr -> do
          let
            maybeDna = pure (Just dna)
            readIndex = pure (Just 0)
            writeIndex = pure (Just 0)
            peReset = unsafeFromActiveHigh $ (pure peResetCycle) .>. (unpack <$> c)

          Fwd c <- idC -< cntr
          (Fwd linksOut, writtenData) <-
            wireDemoPe @_ @1 peReset maybeDna (pure 0)
              -< (Fwd (repeat c), Fwd readIndex, Fwd writeIndex)
          -- Since 'linkCount' is 1 there is only one link in the vector
          idC -< (Fwd (head linksOut), writtenData)

    counter = L.iterate (+ 1) 0
    (outLink, writtenData) = sampleC conf $ dut <| driveC conf counter

    writeStart = fromIntegral (peResetCycle + 1) :: Int
    writeHoldCycles = fromIntegral (natToNum @WriteHoldCycles :: Word) :: Int

    -- The value written: link input at readCycle XOR dna.
    -- The link input at cycle peResetCycle is pack(peResetCycle).
    expectedOutput = resize dna `xor` (pack peResetCycle)

    -- Partition outputs into three regions:
    --   before the Write window, during the Write window, and after.
    (linkBefore, linkRest) = L.splitAt writeStart outLink
    (linkDuring, linkAfter) = L.splitAt writeHoldCycles linkRest

    (wDataBefore, wDataRest) = L.splitAt writeStart writtenData
    (wDataDuring, wDataAfter) = L.splitAt writeHoldCycles wDataRest

  H.footnote
    [Str.i|writeStart=#{writeStart}, writeHoldCycles=#{writeHoldCycles}, expectedOutput=#{expectedOutput}|]

  -- Before the Write window: link carries the counter (localCounter=0 → 0)
  H.assert (L.all (== 0) linkBefore)
  H.assert (L.all (== Nothing) wDataBefore)

  -- During the Write window (WriteHoldCycles cycles): value is present on all cycles
  H.assert (L.all (== expectedOutput) linkDuring)
  H.assert (L.all (== Just expectedOutput) wDataDuring)

  -- After the Write window: back to Idle, link carries counter again (0)
  H.assert (L.all (== 0) linkAfter)
  H.assert (L.all (== Nothing) wDataAfter)
 where
  simLength = 100
  -- We set the reset inside 'dut' based on the input counter so we don't use it here
  conf = def{timeoutAfter = simLength, resetCycles = 0}

tests :: TestTree
tests = $(testGroupGenerator)
