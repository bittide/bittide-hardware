-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Clash.Sized.Extra where

import Clash.Prelude

{- $setup
>>> import Clash.Prelude
-}

-- | Safe 'Unsigned' to 'Signed' conversion
unsignedToSigned :: forall n. (KnownNat n) => Unsigned n -> Signed (n + 1)
unsignedToSigned n = bitCoerce (zeroExtend n)

{- | Combine two 'Unsigned's by concatenating them together. I.e., the first
argument is prepended to the second.

>>> pack (concatUnsigneds (0b1100 :: Unsigned 4) (0b1111 :: Unsigned 4))
0b1100_1111
-}
concatUnsigneds ::
  forall a b.
  (KnownNat a, KnownNat b) =>
  -- | Most significant bits of result
  Unsigned a ->
  -- | Least significant bits of result
  Unsigned b ->
  -- | First and second argument concatenated
  Unsigned (a + b)
concatUnsigneds a b =
  shiftL (extend a) (natToNum @b) .|. extend b
