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
timesDivRU :: forall a b . Dict (b <= (Div (b + (a - 1)) a * a))
timesDivRU = unsafeCoerce (Dict :: Dict ())

clog2axiom :: CLog 2 (n * 2) :~: (CLog 2 n + 1)
clog2axiom = unsafeCoerce Refl
