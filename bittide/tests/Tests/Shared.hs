-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE GADTs #-}
module Tests.Shared where

import Clash.Prelude

import Clash.Hedgehog.Sized.Unsigned
import Data.Constraint (Dict(Dict))
import Data.Constraint.Nat.Extra (timesNDivRU'')
import GHC.Stack (HasCallStack)
import Hedgehog
import Protocols (toSignals)
import Protocols.Wishbone (WishboneM2S, WishboneS2M)
import Protocols.Wishbone.Standard.Hedgehog (validatorCircuit)

import Bittide.SharedTypes (Bytes)

import qualified Hedgehog.Range as Range


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

validateWb ::
  forall dom aw bs.
  (HasCallStack, HiddenClockResetEnable dom, KnownNat aw, KnownNat bs) =>
  Signal dom (WishboneM2S aw bs (Bytes bs)) ->
  Signal dom (WishboneS2M (Bytes bs)) ->
  (Signal dom (WishboneM2S aw bs (Bytes bs)), Signal dom (WishboneS2M (Bytes bs)))
validateWb m2s0 s2m0 = (m2s1, s2m1)
 where
  validate = toSignals $ validatorCircuit @dom @aw @(Bytes bs)
  (s2m1, m2s1) =
    case timesNDivRU'' @bs @8 of
      Dict ->
        validate (m2s0, s2m0)
