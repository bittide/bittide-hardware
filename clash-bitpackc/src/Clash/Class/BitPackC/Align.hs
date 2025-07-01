-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Clash.Class.BitPackC.Align where

import Clash.Prelude
import Data.Type.Bool (type If)
import GHC.TypeLits.KnownNat (
  KnownNat2,
  SNatKn (SNatKn),
  nameToSymbol,
  natSing2,
 )

{- | Align calculates the number of bytes needed to align to a given boundary. It
is the equivalent of:

> type Padding start boundary = (boundary - (start `Mod` boundary)) `Mod` boundary

I.e., it calculates how many bytes to insert to arrive at a multiple of @boundary@
starting from position @start@. Note that writing it directly this way would make
the type checker insert spurious @b <= Mod a b@ constraints everywhere.
-}
type family Padding (start :: Nat) (boundary :: Nat) :: Nat where
  Padding _ 1 = 0
  Padding 0 _ = 0
  Padding s b =
    If
      ((s `Mod` b) <=? b)
      ((b - (s `Mod` b)) `Mod` b)
      -- Can't happen, but if we insert a 'TypeError' GHC trips up..
      0

{- | Term level implementation of 'Padding', used by the type checkers to insert
proofs. We need this because the type checker plugins cannot evaluate type families,
as it cannot access their guts.
-}
instance (KnownNat start, KnownNat boundary) => KnownNat2 $(nameToSymbol ''Padding) start boundary where
  natSing2 = SNatKn $ (b - (s `mod` b)) `mod` b
   where
    s = natToNum @start
    b = natToNum @boundary
  {-# INLINE natSing2 #-}

type family MultipleOf (a :: Nat) (b :: Nat) where
  MultipleOf a 0 = TypeError ('Text "MultipleOf: b must be non-zero")
  MultipleOf a b = DivRU a b * b

{- | Term level implementation of 'MultipleOf', used by the type checkers to insert
proofs. We need this because the type checker plugins cannot evaluate type families,
as it cannot access their guts.
-}
instance (KnownNat a, KnownNat b) => KnownNat2 $(nameToSymbol ''MultipleOf) a b where
  natSing2 =
    case d1 `compareSNat` SNat @b of
      SNatLE -> SNatKn $ natToNum @(DivRU a b * b)
      _ -> error "MultipleOf: b must be non-zero"
  {-# INLINE natSing2 #-}
