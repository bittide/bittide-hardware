-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

{- | The functions in this module enable us introduce trivial constraints that are
not solved by the constraint solver.

Machine verifiable Agda proofs of the properties claimed in this file can be
found in @data/proofs/TypeNatProofs.agda@. All proofs that are not proven in
Agda have a corresponding Hedgehog test.
-}
module Data.Constraint.Nat.Lemmas where

import Data.Constraint (Dict (..))
import Data.Type.Equality ((:~:) (Refl))
import GHC.TypeLits.Extra (CLog, CLogWZ, DivRU, Max, Min)
import GHC.TypeNats (Div, type (*), type (+), type (-), type (<=))
import Unsafe.Coerce (unsafeCoerce)

-- | b <= ceiling(b/a)*a
timesDivRU :: forall a b. (1 <= a) => Dict (b <= (Div (b + (a - 1)) a * a))
timesDivRU = unsafeCoerce (Dict :: Dict (0 <= 0))

{- | Implements logarithmic product rule. Currently hardcoded for specific
constants, which we might relax in the future.
-}
clogProductRule :: (1 <= n) => CLog 2 (n * 2) :~: (CLog 2 n + 1)
clogProductRule = unsafeCoerce Refl

{- | Implements logarithmic product rule. Currently hardcoded for specific
constants, which we might relax in the future.
-}
clogWZProductRule :: (1 <= n) => CLogWZ 2 (n * 2) 0 :~: (CLogWZ 2 n 0 + 1)
clogWZProductRule = unsafeCoerce Refl

{- | Postulates that multiplying some number /a/ by some constant /b/, and
subsequently dividing that result by /b/ equals /a/.
-}
cancelMulDiv :: forall a b. (1 <= b) => Dict (DivRU (a * b) b ~ a)
cancelMulDiv = unsafeCoerce (Dict :: Dict (0 ~ 0))

{- | Postulates that adding a constant less than the denominator does not
change the result (for the given specific context).
-}
divWithRemainder ::
  forall a b c.
  (1 <= b, c <= (b - 1)) =>
  Dict (Div ((a * b) + c) b ~ a)
divWithRemainder = unsafeCoerce (Dict :: Dict (0 ~ 0))

{- | Postulates that a part is less than or equal to a sum parts, in context
of 'Max's left argument.
-}
leMaxLeft :: forall a b c. Dict (a <= Max (a + c) b)
leMaxLeft = unsafeCoerce (Dict :: Dict (0 <= 0))

-- | If @c <= a@ and @c <= b@, then @c <= Max a b@
lessThanMax :: forall a b c. (c <= a, c <= b) => Dict (c <= Max a b)
lessThanMax = unsafeCoerce (Dict :: Dict (0 <= 0))

{- | Postulates that a part is less than or equal to a sum parts, in context
of 'Max's right argument.
-}
leMaxRight :: forall a b c. Dict (b <= Max a (b + c))
leMaxRight = unsafeCoerce (Dict :: Dict (0 <= 0))

-- | if (1 <= a) and (1 <= b) then (1 <= DivRU a b)
strictlyPositiveDivRu :: forall a b. (1 <= a, 1 <= b) => Dict (1 <= DivRU a b)
strictlyPositiveDivRu = unsafeCoerce (Dict :: Dict (0 <= 0))

{- | Euclid's third axiom: /If equals be subtracted from equals, the remainders
are equal/.
-}
euclid3 :: forall a b c. (a + b <= c) => Dict (a <= c - b)
euclid3 = unsafeCoerce (Dict :: Dict (0 <= 0))

-- | if (2 <= n) holds, then (1 <= CLog 2 n) also holds.
oneLeCLog2n :: forall n. (2 <= n) => Dict (1 <= CLog 2 n)
oneLeCLog2n = unsafeCoerce (Dict :: Dict (0 <= 0))

-- | If @1 <= m@ and @n + m <= u@, then @1 + n <= u@
useLowerLimit :: forall n m u. (1 <= m, n + m <= u) => Dict (1 + n <= u)
useLowerLimit = unsafeCoerce (Dict :: Dict (0 <= 0))

-- | Postulates that the minimum of a and b can't be larger than b
minLeq :: forall a b. Dict (Min a b <= b)
minLeq = unsafeCoerce (Dict :: Dict (0 <= 0))

-- | Postulates that the minimum of a and b can't be larger than b
maxGeqPlus :: forall a b c. Dict (a <= Max a b + c)
maxGeqPlus = unsafeCoerce (Dict :: Dict (0 <= 0))

{- | Postulates that multiplying two numbers that are greater than 1 will
result in a number that is greater than 1.
-}
leMult :: forall a b. (1 <= a, 1 <= b) => Dict (1 <= a * b)
leMult = unsafeCoerce (Dict :: Dict (0 <= 0))

-- | if (n <= 0) then n ~ 0
nLe0Eq0 :: forall n. (n <= 0) => Dict (n ~ 0)
nLe0Eq0 = unsafeCoerce (Dict :: Dict (0 ~ 0))
