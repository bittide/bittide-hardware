-- SPDX-FileCopyrightText: 2026 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE OverloadedStrings #-}

{- | Tests for 'Bittide.CaptureUgn.captureUgns'. The interesting property compared to
the single-link case is that each link captures independently: link @k@ latches its
remote counter (and the shared local counter) on the first cycle it produces @Just@
data, and different links may reach that point on different cycles.
-}
module Tests.CaptureUgn where

import Clash.Prelude hiding (sample)

import Clash.Hedgehog.Sized.Vector (genVec)
import Data.String.Interpolate (i)
import Hedgehog
import Hedgehog.Gen.Extra (genSmallInt)
import Protocols
import Protocols.Experimental.Hedgehog (
  ExpectOptions (eoResetCycles, eoSampleMax),
  defExpectOptions,
 )
import Protocols.Experimental.Wishbone
import Protocols.Experimental.Wishbone.Standard.Hedgehog (wishbonePropWithModel)
import Protocols.MemoryMap
import Protocols.MemoryMap.Mask (Mask)
import Test.Tasty
import Test.Tasty.Hedgehog

import Bittide.CaptureUgn (captureUgns)
import Bittide.SharedTypes (withLittleEndian)

import qualified Data.List as L
import qualified Data.Map as Map
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Protocols.Experimental.Wishbone.Standard.Hedgehog as WB
import qualified Protocols.MemoryMap.Mask as Mask

tests :: TestTree
tests =
  testGroup
    "Tests.CaptureUgn"
    [ testPropertyNamed
        "Each link captures its own remote counter independently"
        "prop_captureUgnsIndependent"
        prop_captureUgnsIndependent
    ]

type AddressWidth = 7
type NumLinks = 4

{- | Number of cycles the DUT is held in reset. 'captureUgns' applies a 'dflipflop' to
its link input that has an undefined power-up value, so a reset is required to bring
the capture logic up cleanly.
-}
type ResetCycles = 10

resetCycles :: Int
resetCycles = natToNum @ResetCycles

{- | The cycle (relative to the first simulated cycle) on which each link first produces
@Just@ data. Distinct and interleaved so that capture order differs from link order: this
only passes if every link captures on its own schedule rather than sharing one moment.
-}
captureCycles :: Vec NumLinks Int
captureCycles = 2 :> 4 :> 3 :> 5 :> Nil

-- | Distinct remote counter values, one per link.
remotes :: Vec NumLinks (BitVector 64)
remotes =
  0x1111_1111_2222_2222
    :> 0x3333_3333_4444_4444
    :> 0x5555_5555_6666_6666
    :> 0x7777_7777_8888_8888
    :> Nil

-- | Link input to be fed for a specific cycle
linkInputs :: Int -> Vec NumLinks (Maybe (BitVector 64))
linkInputs cycleNr = zipWith go captureCycles remotes
 where
  go start remote
    | cycleNr < start = Nothing
    | cycleNr == start = Just remote
    | otherwise = Just 0xDDEE_AADD_BBEE_EEFF

data ModelState = ModelState
  { lastMask :: Mask NumLinks
  }

emptyModelState :: ModelState
emptyModelState =
  ModelState
    { lastMask = Mask.fromBitVector 0
    }

data AddressClass
  = LocalCounter {link :: Index NumLinks, high :: Bool}
  | RemoteCounter {link :: Index NumLinks, high :: Bool}
  | HasCaptured
  | OtherAddress

{- | Drives @NumLinks@ links that come up on different cycles, then reads back each link's
@remote_counter@ entry (two 32-bit words per 64-bit value) and the @has_captured@ bitmask,
checking they match the values the links were given.
-}
prop_captureUgnsIndependent :: Property
prop_captureUgnsIndependent = property $ do
  -- Offset every link's start past the reset window so capture is triggered by the
  -- link's own 'Nothing'->'Just' edge (seen through 'captureUgns''s input 'dflipflop')
  -- rather than artificially on reset release. This keeps the captured local counter
  -- equal to 'start + 1' (see 'checkCounter').
  starts <- forAll $ fmap (resetCycles + 1 +) <$> genVec genSmallInt

  let
    deviceName = "CaptureUgns"
    defs = ((getMMAny dutMm).deviceDefs) Map.! deviceName

    -- Register addresses in the memory map are byte addresses; the Wishbone bus is
    -- word-addressed, so divide by the word size in bytes.
    regWordAddr name = case [r.value.address | r <- defs.registers, r.name.name == name] of
      (addr : _) -> fromIntegral addr `div` natToNum @4
      [] -> error ("CaptureUgns has no register named " <> name)

    localCounterBase = regWordAddr "local_counter" :: BitVector AddressWidth
    remoteCounterBase = regWordAddr "remote_counter" :: BitVector AddressWidth
    hasCapturedBase = regWordAddr "has_captured" :: BitVector AddressWidth

    garbage = L.repeat (Just 0xDDEE_AADD_BBEE_EEFF)
    mkLink start remote = fromList $ L.replicate start Nothing <> (Just remote : garbage)
    links = mkLink <$> starts <*> remotes

    clk = clockGen
    rst = resetGenN (SNat @ResetCycles)
    ena = enableGen

    localCounter = fromList [0 ..]

    -- Each 64-bit vector element occupies two consecutive 32-bit words: the low word
    -- at @base + 2 * link@ and the high word at @base + 2 * link + 1@.
    classifyVecAddr mk base addr
      | addr >= base && wordIx < natToNum @(2 * NumLinks) =
          Just (mk (fromIntegral (wordIx `div` 2)) (odd wordIx))
      | otherwise = Nothing
     where
      wordIx = addr - base

    classifyAddress :: BitVector AddressWidth -> AddressClass
    classifyAddress addr
      | Just c <- classifyVecAddr LocalCounter localCounterBase addr = c
      | Just c <- classifyVecAddr RemoteCounter remoteCounterBase addr = c
      | addr == hasCapturedBase = HasCaptured
      | otherwise = OtherAddress

    dutMm :: Circuit (ToConstBwd Mm, Wishbone System Standard AddressWidth 4) ()
    dutMm =
      withLittleEndian $ withClockResetEnable clk rst ena $ circuit $ \bus -> do
        _fwd <- captureUgns localCounter (bundle links) -< bus
        idC -< ()

    -- Each 64-bit register value is read back as two 32-bit words; pick the requested
    -- half.
    word64 :: Bool -> BitVector 64 -> BitVector 32
    word64 hi v = if hi then resize (v `shiftR` 32) else resize v

    -- Whether the model has observed (via a 'has_captured' read) that link @k@ has
    -- captured. Bit @k@ of the mask corresponds to link @k@ (LSB-first).
    captured :: Index NumLinks -> Mask NumLinks -> Bool
    captured k mask = testBit (Mask.toBitVector mask) (fromEnum k)

    -- Validate a read of a counter word against an expected captured value, but only
    -- once the link is known to have captured. Before that, the register may hold
    -- either its reset value (0) or the just-captured value, so we cannot conclude.
    checkCounter ::
      String ->
      Index NumLinks ->
      Bool ->
      BitVector 64 ->
      WishboneS2M 4 ->
      ModelState ->
      Either String ModelState
    checkCounter what k hi expected resp st
      | not (captured k st.lastMask) = Right st
      | resp.acknowledge && resp.readData == expectedWord = Right st
      | otherwise =
          Left [i|#{what} link #{k} (high=#{hi}): expected 0x#{expectedWord}, got #{resp}|]
     where
      expectedWord = word64 hi expected

    model ::
      WB.WishboneMasterRequest AddressWidth 4 ->
      WishboneS2M 4 ->
      ModelState ->
      Either String ModelState
    -- Writes are not part of what this property checks (the only writable register is
    -- 'elastic_buffer_delta'); accept them without inspecting the response.
    model (WB.Write{}) _resp st = Right st
    model (WB.Read addr _) resp st = case classifyAddress addr of
      -- 'has_captured' is a bitmask, one bit per link. Bits may flip from 0 to 1 as
      -- links come up, but never back to 0; enforce that monotonicity.
      HasCaptured
        | not resp.acknowledge -> Left [i|has_captured read not acknowledged: #{resp}|]
        | newBits .&. oldBits /= oldBits ->
            Left [i|has_captured regressed: was 0x#{oldBits}, now 0x#{newBits}|]
        | otherwise -> Right st{lastMask = newMask}
       where
        -- 'has_captured' holds a 'Mask NumLinks'; it occupies the low 'NumLinks' bits
        -- of the 32-bit read word.
        newBits = resize resp.readData :: BitVector NumLinks
        newMask = Mask.fromBitVector newBits
        oldBits = Mask.toBitVector st.lastMask
      -- Local counter captured by link @k@: 'starts !! k' plus one, accounting for the
      -- 'dflipflop' on the link input (see the single-link self test).
      LocalCounter{link = k, high = hi} ->
        checkCounter "local_counter" k hi (fromIntegral (starts !! k) + 1) resp st
      RemoteCounter{link = k, high = hi} ->
        checkCounter "remote_counter" k hi (remotes !! k) resp st
      OtherAddress -> Right st

  let
    genAddr :: Gen (BitVector AddressWidth)
    genAddr =
      Gen.frequency
        [ (10, Gen.constant hasCapturedBase)
        , (30, Gen.integral (Range.constant localCounterBase remoteCounterBase))
        , (30, Gen.integral (Range.constant remoteCounterBase hasCapturedBase))
        , (30, Gen.integral (Range.constant 0 maxBound))
        ]

    -- We don't care too much about generating data: the only writable register isn't
    -- interesting for this test.
    genData = Gen.integral (Range.constant 0 maxBound) :: Gen (BitVector 32)

    genTransaction =
      Gen.frequency
        [ (90, WB.Read <$> genAddr <*> pure maxBound)
        , (10, WB.Write <$> genAddr <*> pure maxBound <*> genData)
        ]

    expectOptions =
      defExpectOptions
        { eoResetCycles = resetCycles
        , eoSampleMax = 2000
        }

  withClockResetEnable clk rst ena
    $ wishbonePropWithModel
      expectOptions
      model
      (unMemmap dutMm)
      (Gen.list (Range.linear 1 (5 * maximum starts)) genTransaction)
      emptyModelState
