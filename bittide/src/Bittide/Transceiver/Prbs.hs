-- SPDX-FileCopyrightText: 2023-2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE GADTs #-}

module Bittide.Transceiver.Prbs where

import Clash.Explicit.Prelude

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


-- | PRBS configuration where we use the full 64 data bits for the PRBS.
prbsConf31w64 :: PrbsConfig 31 28 64
prbsConf31w64 = PrbsConfig


prbsGen ::
  forall dom polyLength polyTap nBits .
  KnownDomain dom =>
  Clock dom -> Reset dom -> Enable dom ->
  PrbsConfig polyLength polyTap nBits ->
  Signal dom (BitVector nBits)
prbsGen clk rst ena PrbsConfig =
  mealy clk rst ena go (maxBound, maxBound) (pure ())
 where
  go ::
    (BitVector polyLength, BitVector nBits) ->
    () ->
    ((BitVector polyLength, BitVector nBits), BitVector nBits)
  go (prbs_reg, prbs_out_prev) _ =
    ( ( last prbs
      , pack (reverse $ map msb prbs))
    , prbs_out_prev
    )
   where
     prbs :: Vec nBits (BitVector polyLength)
     prbs = unfoldrI goPrbs prbs_reg

     goPrbs :: BitVector polyLength -> (BitVector polyLength, BitVector polyLength)
     goPrbs bv = (o,o)
      where
       o = nb +>>. bv
       tap = SNat @(polyLength - polyTap)
       nb = xor (lsb bv) (unpack $ slice tap tap bv)


prbsChecker ::
  forall dom polyLength polyTap nBits .
  KnownDomain dom =>
  Clock dom -> Reset dom -> Enable dom ->
  PrbsConfig polyLength polyTap nBits ->
  Signal dom (BitVector nBits) ->
  Signal dom (BitVector nBits)
prbsChecker clk rst ena PrbsConfig = mealy clk rst ena go (maxBound, maxBound)
 where
  go ::
    (BitVector polyLength, BitVector nBits) ->
    BitVector nBits ->
    ((BitVector polyLength, BitVector nBits), BitVector nBits)
  go (prbs_reg, prbs_out_prev) prbsIn =
    ( (prbs_state, pack $ reverse prbs_out)
    , prbs_out_prev
    )
   where
     prbs_out :: Vec nBits Bit
     prbs_state :: BitVector polyLength
     (prbs_state, prbs_out) = mapAccumL goPrbs prbs_reg (reverse $ unpack prbsIn)

     goPrbs :: BitVector polyLength -> Bit -> (BitVector polyLength, Bit)
     goPrbs bv inp = (o, bitErr)
      where
       o = inp +>>. bv
       tap = SNat @(polyLength - polyTap)
       bitErr = xor inp (xor (lsb bv) (unpack $ slice tap tap bv))
