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

module Data.Constraint.Nat.Extra
  ( module Data.Constraint.Nat.Extra
  , Data.Constraint.Dict(..)
  ) where

import Data.Constraint
import Data.Type.Equality
import GHC.TypeLits.Extra
import GHC.TypeNats
import Unsafe.Coerce

-- | b <= ceiling(b/a)*a
timesDivRU :: forall a b . (1 <= a) => Dict (b <= (Div (b + (a - 1)) a * a))
timesDivRU = unsafeCoerce (Dict :: Dict (1 <= a))
{-# NOINLINE timesDivRU #-} -- https://github.com/clash-lang/clash-compiler/issues/2376

-- | Implements logarithmic product rule. Currently hardcoded for specific
-- constants, which we might relax in the future.
clogProductRule :: (1 <= n) => CLog 2 (n * 2) :~: (CLog 2 n + 1)
clogProductRule = unsafeCoerce Refl
{-# NOINLINE clogProductRule #-} -- https://github.com/clash-lang/clash-compiler/issues/2376

-- | Postulates that multiplying some number /a/ by some constant /b/, and
-- subsequently dividing that result by /b/ equals /a/.
cancelMulDiv :: forall a b . (1 <= b) => Dict (DivRU (a * b) b ~ a)
cancelMulDiv = unsafeCoerce (Dict :: Dict (1 <= b))
{-# NOINLINE cancelMulDiv #-} -- https://github.com/clash-lang/clash-compiler/issues/2376

-- | Postulates that adding a constant less than the denominator does not
-- change the result (for the given specific context).
divWithRemainder ::
  forall a b c.
  (1 <= b, c <= (b - 1)) =>
  Dict (Div ((a * b) + c) b ~ a)
divWithRemainder = unsafeCoerce (Dict :: Dict (1 <= b, c <= (b - 1)))
{-# NOINLINE divWithRemainder #-} -- https://github.com/clash-lang/clash-compiler/issues/2376

-- | Postulates that a part is less than or equal to a sum parts, in context
-- of 'Max's left argument.
leMaxLeft :: forall a b c. Dict (a <= Max (a + c) b)
leMaxLeft = unsafeCoerce (Dict :: Dict (a ~ a))
{-# NOINLINE leMaxLeft #-} -- https://github.com/clash-lang/clash-compiler/issues/2376

-- | If @c <= a@ and @c <= b@, then @c <= Max a b@
lessThanMax :: forall a b c . (c <= a, c <= b) => Dict (c <= Max a b)
lessThanMax = unsafeCoerce (Dict :: Dict (c <= a, c <= b))

-- | Postulates that a part is less than or equal to a sum parts, in context
-- of 'Max's right argument.
leMaxRight :: forall a b c. Dict (b <= Max a (b + c))
leMaxRight = unsafeCoerce (Dict :: Dict (a ~ a))
{-# NOINLINE leMaxRight #-} -- https://github.com/clash-lang/clash-compiler/issues/2376

-- | if (1 <= a) and (1 <= b) then (1 <= DivRU a b)
strictlyPositiveDivRu :: forall a b . (1 <= a, 1 <= b) => Dict (1 <= DivRU a b)
strictlyPositiveDivRu = unsafeCoerce (Dict :: Dict (1 <= a, 1 <= b))
{-# NOINLINE strictlyPositiveDivRu #-} -- https://github.com/clash-lang/clash-compiler/issues/2376

-- | Euclid's third axiom: /If equals be subtracted from equals, the remainders
-- are equal/.
euclid3 :: forall a b c . (a + b <= c) => Dict (a <= c - b)
euclid3 = unsafeCoerce (Dict :: Dict (a + b <= c))
{-# NOINLINE euclid3 #-} -- https://github.com/clash-lang/clash-compiler/issues/2376

-- | if (2 <= n) holds, then (1 <= CLog 2 n) also holds.
oneLeCLog2n :: forall n . (2 <= n) => Dict (1 <= CLog 2 n)
oneLeCLog2n = unsafeCoerce unsafeCoerce (Dict :: Dict (2 <= n))
{-# NOINLINE oneLeCLog2n #-} -- https://github.com/clash-lang/clash-compiler/issues/2376

-- | If @1 <= m@ and @n + m <= u@, then @1 + n <= u@
useLowerLimit :: forall n m u . (1 <= m, n + m <= u) => Dict (1 + n <= u)
useLowerLimit = unsafeCoerce (Dict :: Dict (1 <= m, n + m <= u))
{-# NOINLINE useLowerLimit #-} -- https://github.com/clash-lang/clash-compiler/issues/2376
