-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE GADTs #-}
module Tests.Shared where

import Clash.Prelude
import qualified Hedgehog.Range as Range
import Hedgehog
import Clash.Hedgehog.Sized.Unsigned

data IsInBounds a b c where
  InBounds :: (a <= b, b <= c) => IsInBounds a b c
  NotInBounds :: IsInBounds a b c

deriving instance Show (IsInBounds a b c)

-- | Returns 'InBounds' when a <= b <= c, otherwise returns 'NotInBounds'.
isInBounds :: SNat a -> SNat b -> SNat c -> IsInBounds a b c
isInBounds a b c = case (compareSNat a b, compareSNat b c) of
  (SNatLE, SNatLE) -> InBounds
  _ -> NotInBounds

-- | We use a custom generator for BitVector's because the current Clash implementation
-- uses genVec which is slow.
genDefinedBitVector :: forall n m . (MonadGen m, KnownNat n) => m (BitVector n)
genDefinedBitVector = pack <$> genUnsigned Range.constantBounded
