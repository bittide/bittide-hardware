-- SPDX-FileCopyrightText: 2026 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE OverloadedStrings #-}

module Tests.ProgrammableMux where

import Clash.Prelude
import Protocols

import Bittide.ProgrammableMux (programmableMux)
import Bittide.SharedTypes (withByteOrder)
import Clash.Class.BitPackC (ByteOrder (..))
import Clash.Class.BitPackC.Padding (packWordC)
import Data.Maybe (fromJust)
import Protocols.Hedgehog (defExpectOptions)
import Protocols.MemoryMap
import Protocols.Wishbone
import Protocols.Wishbone.Standard.Hedgehog (WishboneMasterRequest (..))

import Clash.Hedgehog.Sized.Unsigned (genUnsigned)
import Hedgehog (Property)
import Test.Tasty
import Test.Tasty.Hedgehog (testProperty)
import Test.Tasty.TH (testGroupGenerator)

import qualified Data.List as L
import qualified Data.Map as Map
import qualified Data.String.Interpolate as Str
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Protocols.Wishbone.Standard.Hedgehog as Wb

{- | Differentiate between the data from the management unit and processing element in the
constructor. The Unsigned 64 is the cycle number at which the data is generated, so we can
verify the exact cycle the programmable mux switches.
-}
data LinkData = Mu (Unsigned 64) | Pe (Unsigned 64)
  deriving (Show, ShowX, Eq, Generic, NFDataX, BitPack)

isMu :: LinkData -> Bool
isMu (Mu _) = True
isMu _ = False

isPe :: LinkData -> Bool
isPe (Pe _) = True
isPe _ = False

prop_ProgrammableMux :: Property
prop_ProgrammableMux = H.property $ do
  -- The minimum of 100 cycles is to make sure the registers are written to before that
  -- the counter reaches 'firstBCycle'.
  firstBCycle <- H.forAll $ genUnsigned @64 (Range.linear 100 (fromIntegral simLength - 20))
  byteOrder <- H.forAll $ Gen.element [BigEndian, LittleEndian]
  let
    -- Notice that the data width is 8 bytes, which is larger than the usual 4 bytes in
    -- bittide. We do this so we can write the full 64 bits of 'firstBCycle' in one
    -- request.
    dut ::
      Circuit
        (ToConstBwd Mm, Wishbone System 'Standard 32 8)
        (CSignal System Bool, CSignal System LinkData)
    dut =
      withByteOrder byteOrder
        $ withClockResetEnable @System clockGen resetGen enableGen
        $ circuit
        $ \wb -> do
          let
            counter = register 0 (counter + 1)
            muLinks = fmap Mu counter
            peLinks = fmap Pe counter
          (Fwd rst, out) <- programmableMux counter -< (wb, Fwd muLinks, Fwd peLinks)
          idC -< (Fwd (unsafeToActiveHigh rst), out)

    deviceName = "ProgrammableMux"
    defs = (((getMMAny dut).deviceDefs) Map.! deviceName)

    firstBCycleLoc = L.find (\loc -> loc.name.name == "first_b_cycle") defs.registers
    armLoc = L.find (\loc -> loc.name.name == "arm") defs.registers

    firstBCycleAddr = fromIntegral (fromJust firstBCycleLoc).value.address `div` 8
    armAddr = fromIntegral (fromJust armLoc).value.address `div` 8

    firstBCycleBv = pack $ packWordC @8 byteOrder firstBCycle
    armBv = pack $ packWordC @8 byteOrder True
    requests = fmap (,0) [Write firstBCycleAddr maxBound firstBCycleBv, Write armAddr maxBound armBv]

    (resets, outLink) =
      sampleC
        def{timeoutAfter = simLength}
        (unMemmap dut <| Wb.driveStandard defExpectOptions requests)
    resetsBeforeSwitch = L.length $ L.takeWhile id resets
    resetsAfterSwitch = L.length (L.takeWhile (== False) (L.dropWhile (== True) resets))

    outLinkBeforeSwitch = L.length $ L.takeWhile isMu outLink
    outLinkAfterSwitch = L.length $ L.takeWhile isPe (L.dropWhile isMu outLink)

  H.footnote [Str.i|Asserted Cycles: #{resetsBeforeSwitch}|]
  H.footnote [Str.i|Deasserted Cycles: #{resetsAfterSwitch}|]
  let interestingResetCycles = L.take 10 (L.drop (fromIntegral firstBCycle - 5) resets)
  let interestingLinkCycles = L.take 10 (L.drop (fromIntegral firstBCycle - 5) outLink)
  H.footnote [Str.i|Interesting reset cycles (around the switch point): \n#{interestingResetCycles}|]
  H.footnote [Str.i|Interesting link cycles (around the switch point): \n#{interestingLinkCycles}|]

  -- Check that the reset is asserted before 'firstBCycle', and deasserted after. The +-1
  -- is to account for the single cycle the dut is in reset because of 'resetGen'.
  resetsBeforeSwitch H.=== fromIntegral firstBCycle + 1
  resetsAfterSwitch H.=== simLength - fromIntegral firstBCycle - 1

  -- Check that the out link is from the MU before 'firstBCycle', and from the PE after.
  outLinkBeforeSwitch H.=== fromIntegral firstBCycle + 1
  outLinkAfterSwitch H.=== simLength - fromIntegral firstBCycle - 1
 where
  simLength = 200

tests :: TestTree
tests = $(testGroupGenerator)
