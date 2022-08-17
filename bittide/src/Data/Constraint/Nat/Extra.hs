-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-
NOTE [constraint solver addition]

The functions in this module enable us introduce trivial constraints that are not
solved by the constraint solver.
-}

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Data.Constraint.Nat.Extra where


import Clash.Promoted.Nat
import Data.Constraint
import Data.Type.Equality
import GHC.TypeLits.Extra
import GHC.TypeNats
import Prelude
import Unsafe.Coerce
import qualified Clash.Util.Interpolate as I

-- | b <= ceiling(b/a)*a
timesDivRU :: forall a b . Dict (b <= (Div (b + (a - 1)) a * a))
timesDivRU = unsafeCoerce (Dict :: Dict ())

clog2axiom :: CLog 2 (n * 2) :~: (CLog 2 n + 1)
clog2axiom = unsafeCoerce Refl

-- | if (c <= a) or (c <= b), then c <= Max a b
lessThanMax :: forall a b c . (KnownNat a, KnownNat b, KnownNat c) => Dict (c <= Max a b)
lessThanMax = case (compareSNat (SNat @c) (SNat @b), compareSNat (SNat @c) (SNat @b)) of
  (SNatLE, _) -> unsafeCoerce (Dict :: Dict ())
  (_, SNatLE) -> unsafeCoerce (Dict :: Dict ())
  (_,_) -> error [I.i|
              Data.Constraint.Nat.Extra.lessThanMax:
                Could not deduce c <= Max a b from (c <= a) or (c <= b) from
                a: #{a}
                b: #{b}
                c: #{c}
          |]

 where
  a = natToInteger @a
  b = natToInteger @b
  c = natToInteger @c

-- | if (1 <= a) and (1 <= b) then (1 <= DivRU a b)
oneLTdivRU :: forall a b . (1 <= a, 1 <= b) => Dict (1 <= DivRU a b)
oneLTdivRU = unsafeCoerce (Dict :: Dict ())
