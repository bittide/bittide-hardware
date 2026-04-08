-- SPDX-FileCopyrightText: 2026 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE OverloadedStrings #-}

module Tests.WireDemoProcessingElement where

import Clash.Prelude
import Protocols

import Bittide.WireDemoProcessingElement (wireDemoPe)

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

splitAtIndex :: Int -> [a] -> ([a], a, [a])
splitAtIndex n xs =
  let (before, rest) = L.splitAt n xs
   in case rest of
        (x : after) -> (before, x, after)
        [] -> error "Index out of bounds"

prop_WireDemoPe :: Property
prop_WireDemoPe = H.property $ do
  peResetCycle <- H.forAll $ genUnsigned (Range.linear 0 90)
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

    readCycle = peResetCycle
    writeCycle = peResetCycle + 1
    expectedOutput = resize dna `xor` (pack readCycle)

    (linkBeforeActive, linkAtWriteCycle, linkAfterActive) = splitAtIndex (fromIntegral writeCycle) outLink
    (wDataBeforeActive, wDataAtWriteCycle, wDataAfterActive) = splitAtIndex (fromIntegral writeCycle) writtenData

    interestingCycles :: [Unsigned 64]
    interestingCycles = L.map unpack $ L.take 10 (L.drop (fromIntegral writeCycle - 5) outLink)

    firstNonZeroIndex = L.findIndex (/= 0) outLink

  H.footnote [Str.i|Interesting cycles (around reset): #{interestingCycles}|]
  H.footnote [Str.i|Expected output at cycle #{writeCycle}, but got it at cycle #{firstNonZeroIndex}|]

  H.assert (L.all (== 0) linkBeforeActive)
  linkAtWriteCycle H.=== expectedOutput
  H.assert (L.all (== 0) linkAfterActive)

  H.assert (L.all (== Nothing) wDataBeforeActive)
  wDataAtWriteCycle H.=== Just expectedOutput
  H.assert (L.all (== Nothing) wDataAfterActive)
 where
  simLength = 100
  -- We set the reset inside 'dut' based on the input counter so we don't use it here
  conf = def{timeoutAfter = simLength, resetCycles = 0}

tests :: TestTree
tests = $(testGroupGenerator)
