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

-- | Implements logarithmic product rule. Currently hardcoded for specific
-- constants, which we might relax in the future.
clogProductRule :: (1 <= n) => CLog 2 (n * 2) :~: (CLog 2 n + 1)
clogProductRule = unsafeCoerce Refl

-- | Postulates that multiplying some number /a/ by some consant /b/, and
-- subsequently dividing that result by /b/ equals /a/.
cancelMulDiv :: forall a b . (1 <= b) => Dict (DivRU (a * b) b ~ a)
cancelMulDiv = unsafeCoerce (Dict :: Dict ())

-- | Postulates that adding a constant less than the denominator does not
-- change the result (for the given specific context).
divWithRemainder ::
  forall a b c.
  (1 <= b, c <= (b - 1)) =>
  Dict (Div ((a * b) + c) b ~ a)
divWithRemainder = unsafeCoerce (Dict :: Dict ())

-- | Postulates that a part is less than or equal to a sum parts, in context
-- of 'Max's left argument.
leMaxLeft :: forall a b c. Dict (a <= Max (a + c) b)
leMaxLeft = unsafeCoerce (Dict :: Dict ())

-- | Postulates that a part is less than or equal to a sum parts, in context
-- of 'Max's right argument.
leMaxRight :: forall a b c. Dict (b <= Max a (b + c))
leMaxRight = unsafeCoerce (Dict :: Dict ())

-- | if (1 <= a) and (1 <= b) then (1 <= DivRU a b)
strictlyPositiveDivRu :: forall a b . (1 <= a, 1 <= b) => Dict (1 <= DivRU a b)
strictlyPositiveDivRu = unsafeCoerce (Dict :: Dict ())

-- | Euclid's third axiom: /If equals be subtracted from equals, the remainders
-- are equal/.
euclid3 :: forall a b c . (a + b <= c) => Dict (a <= c - b)
euclid3 = unsafeCoerce (Dict :: Dict ())
