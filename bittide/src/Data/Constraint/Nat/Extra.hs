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


import Data.Constraint
import Data.Type.Equality
import GHC.TypeLits.Extra
import GHC.TypeNats
import Unsafe.Coerce

-- | b <= ceiling(b/a)*a
timesDivRU :: forall a b . (1 <= a) => Dict (b <= (Div (b + (a - 1)) a * a))
timesDivRU = unsafeCoerce (Dict :: Dict ())

clog2axiom :: (1 <= n) => CLog 2 (n * 2) :~: (CLog 2 n + 1)
clog2axiom = unsafeCoerce Refl

timesNDivRU :: forall a b . (1 <= b) => Dict (DivRU (a * b) b ~ a)
timesNDivRU = unsafeCoerce (Dict :: Dict ())

divWithRemainder ::
  forall a b c.
  (1 <= b, c <= (b - 1)) =>
  Dict (Div ((a * b) + c) b ~ a)
divWithRemainder = unsafeCoerce (Dict :: Dict ())

leMaxLeft :: forall a b c. Dict (a <= Max (a + c) b)
leMaxLeft = unsafeCoerce (Dict :: Dict ())

leMaxRight :: forall a b c. Dict (b <= Max a (b + c))
leMaxRight = unsafeCoerce (Dict :: Dict ())

-- | if (1 <= a) and (1 <= b) then (1 <= DivRU a b)
oneLTdivRU :: forall a b . (1 <= a, 1 <= b) => Dict (1 <= DivRU a b)
oneLTdivRU = unsafeCoerce (Dict :: Dict ())
