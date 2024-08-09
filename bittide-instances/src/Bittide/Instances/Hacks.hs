-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Bittide.Instances.Hacks where

import qualified Clash.Explicit.Prelude as E
import Clash.Prelude

{- | Chaotically distributes a single incoming bit to input bits of the given
function. Similarly, it chaotically reduces the all output bits of a function
into a single bit. Both the input and output of the function will be directly
connected to a register after applying 'reducePins'.

This function is useful for synthesis analysis: although it feeds the circuit
with garbage values, it only needs one input and one output pin. Because all
the in and output pins of the circuit under test are directly connected to
a register, synthesis timing results will (somewhat) realistically reflect
timing capabilities.

See 'reducePins' for a 'BitPack' version of this function.
-}
reducePins# ::
  forall dom m n.
  (HiddenClock dom, KnownNat m, KnownNat n) =>
  (Signal dom (BitVector m) -> Signal dom (BitVector n)) ->
  Signal dom Bit ->
  Signal dom Bit
reducePins# f pin = out
 where
  -- Keeps a shift register of n+m bits, where at each clock cycle the output
  -- of this function and the input of this function are shifted in. The upper
  -- @m@ bits of the shift register are fed to the given function, while the
  -- lower @n@ bits are 'xor'd with the output of the given function, and
  -- 'reduceXor'd to a single bit.
  --
  -- This should be enough to throw off any optimization a synthesis tool might
  -- try to do.
  --
  shiftRegIn = liftA2 (.<<+) shiftReg (xor <$> pin <*> out)
  shiftReg = E.delay @dom @(BitVector (m + n)) clk ena 0 shiftRegIn
  out = go <$> outs <*> E.delay clk ena 0 (f ins)
  (ins, outs) = unbundle (split @_ @m @n <$> shiftReg)
  go a b = reduceXor (a `xor` b)

  clk = hasClock
  ena = enableGen

{- | Chaotically distributes a single incoming bit to input bits of the given
function. Similarly, it chaotically reduces the all output bits of a function
into a single bit. Both the input and output of the function will be directly
connected to a register after applying 'reducePins'.

This function is useful for synthesis analysis: although it feeds the circuit
with garbage values, it only needs one input and one output pin. Because all
the in and output pins of the circuit under test are directly connected to
a register, synthesis timing results will (somewhat) realistically reflect
timing capabilities.
-}
reducePins ::
  forall dom a b.
  (HiddenClock dom, BitPack a, BitPack b) =>
  (Signal dom a -> Signal dom b) ->
  Signal dom Bit ->
  Signal dom Bit
reducePins f = reducePins# (fmap pack . f . fmap unpack)

-- | Bundled version of 'reducePins'.
reducePinsB ::
  forall dom a b.
  (HiddenClock dom, Bundle a, BitPack a, BitPack b, Bundle b) =>
  (Unbundled dom a -> Unbundled dom b) ->
  Signal dom Bit ->
  Signal dom Bit
reducePinsB f = reducePins (bundle . f . unbundle)
