-- SPDX-FileCopyrightText: 2023-2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE GADTs #-}

-- | A pseudo-random bit sequence (PRBS) generator and checker. These functions
-- are used to test the signal integrity of transceivers. The generator generates
-- a PRBS stream, while the checker checks whether the received stream is the
-- same as the generated stream. Note that the checker is "self synchronizing",
-- meaning that it will synchronize with the generator after /polyLength/ cycles.
module Bittide.Transceiver.Prbs where

import Clash.Explicit.Prelude

-- | Configuration for a PRBS generator or checker. Note that this can only
-- specify PRBS streams with two taps: the first tap is always the MSB
-- (@polyLength@), and the second tap is the @polyTap@-th bit.
--
-- See https://en.wikipedia.org/wiki/Pseudorandom_binary_sequence for more information.
data Config polyLength polyTap nBits where
  Config ::
    ( KnownNat polyLength
    , KnownNat polyTap
    , KnownNat nBits

    , 1 <= nBits
    , 1 <= polyTap
    , (polyTap + 1) <= polyLength

    -- Same constraints, but written differently for type checking purposes:
    , (_n0 + 1) ~ nBits
    , (polyTap + _n1) ~ polyLength
    , polyTap ~ (_n2 + 1)
    , _n1 ~ (_n3 + 1)
    ) =>
    Config polyLength polyTap nBits


-- | PRBS31: @x^31 + x^28 + 1@
conf31 :: forall n . (31 <= n, KnownNat n) => Config 31 28 n
conf31 = leToPlus @31 @n Config

-- | PRBS generator, see module documentation.
generator ::
  forall dom polyLength polyTap nBits .
  KnownDomain dom =>
  Clock dom -> Reset dom -> Enable dom ->
  Config polyLength polyTap nBits ->
  Signal dom (BitVector nBits)
generator clk rst ena Config =
  moore clk rst ena go snd (maxBound, maxBound) (pure ())
 where
  go ::
    (BitVector polyLength, BitVector nBits) ->
    () ->
    (BitVector polyLength, BitVector nBits)
  go (prbsReg, _) _ =
    ( last prbs
    , pack (reverse $ map msb prbs) )
   where
     prbs :: Vec nBits (BitVector polyLength)
     prbs = unfoldrI goPrbs prbsReg

     goPrbs :: BitVector polyLength -> (BitVector polyLength, BitVector polyLength)
     goPrbs bv = (o,o)
      where
       o = newBit +>>. bv
       tap = SNat @(polyLength - polyTap)
       newBit = xor (lsb bv) (unpack $ slice tap tap bv)


-- | PRBS checker, see module documentation.
checker ::
  forall dom polyLength polyTap nBits .
  KnownDomain dom =>
  Clock dom -> Reset dom -> Enable dom ->
  Config polyLength polyTap nBits ->
  Signal dom (BitVector nBits) ->
  Signal dom (BitVector nBits)
checker clk rst ena Config = mealy clk rst ena go (maxBound, maxBound)
 where
  go ::
    (BitVector polyLength, BitVector nBits) ->
    BitVector nBits ->
    ((BitVector polyLength, BitVector nBits), BitVector nBits)
  go (prbsReg, prbsOutPrev) prbsIn =
    ( (prbsState, pack $ reverse prbsOut)
    , prbsOutPrev
    )
   where
     prbsOut :: Vec nBits Bit
     prbsState :: BitVector polyLength
     (prbsState, prbsOut) = mapAccumL goPrbs prbsReg (reverse $ unpack prbsIn)

     goPrbs :: BitVector polyLength -> Bit -> (BitVector polyLength, Bit)
     goPrbs bv newBit = (o, bitErr)
      where
       o = newBit +>>. bv
       tap = SNat @(polyLength - polyTap)
       bitErr = xor newBit (xor (lsb bv) (unpack $ slice tap tap bv))

data TrackerState
  = Down (Index 8096)
  -- ^ Link is considered down. Needs 127 cycles of \"good\" input to transition
  -- to 'Up'.
  | Up
  -- ^ Link has not seen errors in at least 127 cycles.
  deriving (Eq, Show, Generic, NFDataX)

-- | Small state machine tracking whether a link is stable. A link is considered
-- stable, if no errors were detected for a number of cycles (see "PrbsTrackerState").
-- Whenever a bit error is detected, it immediately deasserts its output.
tracker ::
  KnownDomain dom =>
  Clock dom ->
  Reset dom ->
  Signal dom Bool ->
  -- ^ PRBS error detected
  Signal dom Bool
  -- ^ Link OK
tracker clk rst =
  mealy clk rst enableGen go initSt
 where
  initSt = Down maxBound

  go :: TrackerState -> Bool -> (TrackerState, Bool)
  go _ True = (initSt, False)
  go st False =
    case st of
      Down 0 -> (Up, False)
      Down n -> (Down (n - 1), False)
      Up     -> (Up, True)
