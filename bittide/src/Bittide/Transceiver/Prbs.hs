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
data PrbsConfig polyLength polyTap nBits where
  PrbsConfig ::
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
    PrbsConfig polyLength polyTap nBits


-- | PRBS31: @x^31 + x^28 + 1@
prbsConf31 :: forall n . (31 <= n, KnownNat n) => PrbsConfig 31 28 n
prbsConf31 = leToPlus @31 @n $ PrbsConfig


prbsGen ::
  forall dom polyLength polyTap nBits .
  KnownDomain dom =>
  Clock dom -> Reset dom -> Enable dom ->
  PrbsConfig polyLength polyTap nBits ->
  Signal dom (BitVector nBits)
prbsGen clk rst ena PrbsConfig =
  moore clk rst ena go snd (maxBound, maxBound) (pure ())
 where
  go ::
    (BitVector polyLength, BitVector nBits) ->
    () ->
    (BitVector polyLength, BitVector nBits)
  go (prbs_reg, _) _ =
    ( last prbs
    , pack (reverse $ map msb prbs) )
   where
     prbs :: Vec nBits (BitVector polyLength)
     prbs = unfoldrI goPrbs prbs_reg

     goPrbs :: BitVector polyLength -> (BitVector polyLength, BitVector polyLength)
     goPrbs bv = (o,o)
      where
       o = newBit +>>. bv
       tap = SNat @(polyLength - polyTap)
       newBit = xor (lsb bv) (unpack $ slice tap tap bv)


prbsChecker ::
  forall dom polyLength polyTap nBits .
  KnownDomain dom =>
  Clock dom -> Reset dom -> Enable dom ->
  PrbsConfig polyLength polyTap nBits ->
  Signal dom (BitVector nBits) ->
  Signal dom (BitVector nBits)
prbsChecker clk rst ena PrbsConfig =
  moore clk rst ena go snd (maxBound, maxBound)
 where
  go ::
    (BitVector polyLength, BitVector nBits) ->
    BitVector nBits ->
    (BitVector polyLength, BitVector nBits)
  go (prbs_reg, _) prbsIn = (prbs_state, pack $ reverse prbs_out)
   where
     prbs_out :: Vec nBits Bit
     prbs_state :: BitVector polyLength
     (prbs_state, prbs_out) = mapAccumL goPrbs prbs_reg (reverse $ unpack prbsIn)

     goPrbs :: BitVector polyLength -> Bit -> (BitVector polyLength, Bit)
     goPrbs bv newBit = (o, bitErr)
      where
       o = newBit +>>. bv
       tap = SNat @(polyLength - polyTap)
       bitErr = xor newBit (xor (lsb bv) (unpack $ slice tap tap bv))
