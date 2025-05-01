-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-
NOTE [constraint solver addition]

The functions in this module enable us introduce trivial constraints that are not
solved by the constraint solver.

Machine verifiable Agda proofs of the properties claimed in this file
can be found in @bittide/proofs/TypeNatProofs.agda@
-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Constraint.Nat.Extra (
  module Data.Constraint.Nat.Extra,
  Data.Constraint.Dict (..),
) where

import Data.Constraint
import Data.Ord ((<=))
import Data.Proxy
import Data.Type.Bool (If)
import Data.Type.Equality
import GHC.Num ((-))
import GHC.TypeLits.Extra
import GHC.TypeLits.KnownNat
import GHC.TypeNats
import Unsafe.Coerce

type family OneMore (n :: Nat) :: Nat where
  OneMore 0 = 0
  OneMore _ = 1

instance (KnownNat n) => KnownNat1 $(nameToSymbol ''OneMore) n where
  natSing1 = case natVal (Proxy @n) of
    0 -> SNatKn 0
    _ -> SNatKn 1
  {-# INLINE natSing1 #-}

-- Note that the first case is redundant, but required as described in this FAQ:
-- https://hackage.haskell.org/package/ghc-typelits-knownnat-0.7.10/docs/GHC-TypeLits-KnownNat.html
type family SatSubZero (a :: Nat) (b :: Nat) :: Nat where
  SatSubZero 0 b = 0
  SatSubZero a b = If (a <=? b) 0 (a - b)

instance (KnownNat a, KnownNat b) => KnownNat2 $(nameToSymbol ''SatSubZero) a b where
  natSing2 =
    let
      a = natVal (Proxy @a)
      b = natVal (Proxy @b)
      z = if a <= b then 0 else a - b
     in
      SNatKn z
  {-# INLINE natSing2 #-}

-- | If @1 <= n@ and @1 <= m@, then @1 <= Div n m + OneMore (Mod n m)@
oneMore :: forall n m. (1 <= n, 1 <= m) => Dict (1 <= Div n m + OneMore (Mod n m))
oneMore = unsafeCoerce (Dict :: Dict (0 <= 0))

-- | If @1 <= n@ and @n <= m@, then @Div n m + OneMore (Mod n m) == 1@
isOne :: forall n m. (1 <= n, n <= m) => Dict (Div n m + OneMore (Mod n m) ~ 1)
isOne = unsafeCoerce (Dict :: Dict (0 ~ 0))

-- | Postulates that @SatSubZero a b + Min a b == a@
satSubZeroMin :: forall a b. Dict (SatSubZero a b + Min a b ~ a)
satSubZeroMin = unsafeCoerce (Dict :: Dict (0 ~ 0))
