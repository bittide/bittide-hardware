-- SPDX-FileCopyrightText: 2022-2023 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilyDependencies #-}

-- Purpose of this module
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module VexRiscv.VecToTuple (VecToTuple(..)) where

import Clash.Prelude

import Data.Tagged (Tagged(..))

#if MIN_VERSION_base(4,18,0)
import Data.Tuple (Solo(MkSolo))
#elif MIN_VERSION_base(4,16,0)
import Data.Tuple (Solo(Solo))
#endif

class VecToTuple a where
  type TupType a = r | r -> a
  vecToTuple ::  a -> TupType a

-- | Silly instance
instance VecToTuple (Vec 0 a) where
  type TupType (Vec 0 a) = Tagged a ()
  vecToTuple Nil = Tagged ()

#if MIN_VERSION_base(4,18,0)
instance VecToTuple (Vec 1 a) where
  type TupType (Vec 1 a) = Solo a
  vecToTuple (a0 :> Nil) = MkSolo a0
#elif MIN_VERSION_base(4,16,0)
instance VecToTuple (Vec 1 a) where
  type TupType (Vec 1 a) = Solo a
  vecToTuple (a0 :> Nil) = Solo a0
#endif

instance VecToTuple (Vec 2 a) where
  type TupType (Vec 2 a) = (a, a)
  vecToTuple (a0 :> a1 :> Nil) = (a0, a1)

instance VecToTuple (Vec 3 a) where
  type TupType (Vec 3 a) = (a, a, a)
  vecToTuple (a0 :> a1 :> a2 :> Nil) = (a0, a1, a2)

instance VecToTuple (Vec 4 a) where
  type TupType (Vec 4 a) = (a, a, a, a)
  vecToTuple (a0 :> a1 :> a2 :> a3 :> Nil) = (a0, a1, a2, a3)

instance VecToTuple (Vec 5 a) where
  type TupType (Vec 5 a) = (a, a, a, a, a)
  vecToTuple (a0 :> a1 :> a2 :> a3 :> a4 :> Nil) = (a0, a1, a2, a3, a4)

instance VecToTuple (Vec 6 a) where
  type TupType (Vec 6 a) = (a, a, a, a, a, a)
  vecToTuple (a0 :> a1 :> a2 :> a3 :> a4 :> a5 :> Nil) = (a0, a1, a2, a3, a4, a5)

instance VecToTuple (Vec 7 a) where
  type TupType (Vec 7 a) = (a, a, a, a, a, a, a)
  vecToTuple (a0 :> a1 :> a2 :> a3 :> a4 :> a5 :> a6 :> Nil) = (a0, a1, a2, a3, a4, a5, a6)

instance VecToTuple (Vec 8 a) where
  type TupType (Vec 8 a) = (a, a, a, a, a, a, a, a)
  vecToTuple (a0 :> a1 :> a2 :> a3 :> a4 :> a5 :> a6 :> a7 :> Nil) = (a0, a1, a2, a3, a4, a5, a6, a7)

instance VecToTuple (Vec 9 a) where
  type TupType (Vec 9 a) = (a, a, a, a, a, a, a, a, a)
  vecToTuple (a0 :> a1 :> a2 :> a3 :> a4 :> a5 :> a6 :> a7 :> a8 :> Nil) = (a0, a1, a2, a3, a4, a5, a6, a7, a8)

instance VecToTuple (Vec 10 a) where
  type TupType (Vec 10 a) = (a, a, a, a, a, a, a, a, a, a)
  vecToTuple (a0 :> a1 :> a2 :> a3 :> a4 :> a5 :> a6 :> a7 :> a8 :> a9 :> Nil) = (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9)

instance VecToTuple (Vec 11 a) where
  type TupType (Vec 11 a) = (a, a, a, a, a, a, a, a, a, a, a)
  vecToTuple (a0 :> a1 :> a2 :> a3 :> a4 :> a5 :> a6 :> a7 :> a8 :> a9 :> a10 :> Nil) = (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)

instance VecToTuple (Vec 12 a) where
  type TupType (Vec 12 a) = (a, a, a, a, a, a, a, a, a, a, a, a)
  vecToTuple (a0 :> a1 :> a2 :> a3 :> a4 :> a5 :> a6 :> a7 :> a8 :> a9 :> a10 :> a11 :> Nil) = (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11)

instance VecToTuple (Vec 13 a) where
  type TupType (Vec 13 a) = (a, a, a, a, a, a, a, a, a, a, a, a, a)
  vecToTuple (a0 :> a1 :> a2 :> a3 :> a4 :> a5 :> a6 :> a7 :> a8 :> a9 :> a10 :> a11 :> a12 :> Nil) = (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12)

instance VecToTuple (Vec 14 a) where
  type TupType (Vec 14 a) = (a, a, a, a, a, a, a, a, a, a, a, a, a, a)
  vecToTuple (a0 :> a1 :> a2 :> a3 :> a4 :> a5 :> a6 :> a7 :> a8 :> a9 :> a10 :> a11 :> a12 :> a13 :> Nil) = (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13)

instance VecToTuple (Vec 15 a) where
  type TupType (Vec 15 a) = (a, a, a, a, a, a, a, a, a, a, a, a, a, a, a)
  vecToTuple (a0 :> a1 :> a2 :> a3 :> a4 :> a5 :> a6 :> a7 :> a8 :> a9 :> a10 :> a11 :> a12 :> a13 :> a14 :> Nil) = (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14)

instance VecToTuple (Vec 16 a) where
  type TupType (Vec 16 a) = (a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a)
  vecToTuple (a0 :> a1 :> a2 :> a3 :> a4 :> a5 :> a6 :> a7 :> a8 :> a9 :> a10 :> a11 :> a12 :> a13 :> a14 :> a15 :> Nil) = (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15)

instance VecToTuple (Vec 17 a) where
  type TupType (Vec 17 a) = (a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a)
  vecToTuple (a0 :> a1 :> a2 :> a3 :> a4 :> a5 :> a6 :> a7 :> a8 :> a9 :> a10 :> a11 :> a12 :> a13 :> a14 :> a15 :> a16 :> Nil) = (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16)

instance VecToTuple (Vec 18 a) where
  type TupType (Vec 18 a) = (a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a)
  vecToTuple (a0 :> a1 :> a2 :> a3 :> a4 :> a5 :> a6 :> a7 :> a8 :> a9 :> a10 :> a11 :> a12 :> a13 :> a14 :> a15 :> a16 :> a17 :> Nil) = (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17)

instance VecToTuple (Vec 19 a) where
  type TupType (Vec 19 a) = (a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a)
  vecToTuple (a0 :> a1 :> a2 :> a3 :> a4 :> a5 :> a6 :> a7 :> a8 :> a9 :> a10 :> a11 :> a12 :> a13 :> a14 :> a15 :> a16 :> a17 :> a18 :> Nil) = (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18)

instance VecToTuple (Vec 20 a) where
  type TupType (Vec 20 a) = (a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a)
  vecToTuple (a0 :> a1 :> a2 :> a3 :> a4 :> a5 :> a6 :> a7 :> a8 :> a9 :> a10 :> a11 :> a12 :> a13 :> a14 :> a15 :> a16 :> a17 :> a18 :> a19 :> Nil) = (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19)

instance VecToTuple (Vec 21 a) where
  type TupType (Vec 21 a) = (a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a)
  vecToTuple (a0 :> a1 :> a2 :> a3 :> a4 :> a5 :> a6 :> a7 :> a8 :> a9 :> a10 :> a11 :> a12 :> a13 :> a14 :> a15 :> a16 :> a17 :> a18 :> a19 :> a20 :> Nil) = (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20)

instance VecToTuple (Vec 22 a) where
  type TupType (Vec 22 a) = (a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a)
  vecToTuple (a0 :> a1 :> a2 :> a3 :> a4 :> a5 :> a6 :> a7 :> a8 :> a9 :> a10 :> a11 :> a12 :> a13 :> a14 :> a15 :> a16 :> a17 :> a18 :> a19 :> a20 :> a21 :> Nil) = (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21)

instance VecToTuple (Vec 23 a) where
  type TupType (Vec 23 a) = (a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a)
  vecToTuple (a0 :> a1 :> a2 :> a3 :> a4 :> a5 :> a6 :> a7 :> a8 :> a9 :> a10 :> a11 :> a12 :> a13 :> a14 :> a15 :> a16 :> a17 :> a18 :> a19 :> a20 :> a21 :> a22 :> Nil) = (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22)

instance VecToTuple (Vec 24 a) where
  type TupType (Vec 24 a) = (a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a)
  vecToTuple (a0 :> a1 :> a2 :> a3 :> a4 :> a5 :> a6 :> a7 :> a8 :> a9 :> a10 :> a11 :> a12 :> a13 :> a14 :> a15 :> a16 :> a17 :> a18 :> a19 :> a20 :> a21 :> a22 :> a23 :> Nil) = (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23)

instance VecToTuple (Vec 25 a) where
  type TupType (Vec 25 a) = (a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a)
  vecToTuple (a0 :> a1 :> a2 :> a3 :> a4 :> a5 :> a6 :> a7 :> a8 :> a9 :> a10 :> a11 :> a12 :> a13 :> a14 :> a15 :> a16 :> a17 :> a18 :> a19 :> a20 :> a21 :> a22 :> a23 :> a24 :> Nil) = (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24)

instance VecToTuple (Vec 26 a) where
  type TupType (Vec 26 a) = (a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a)
  vecToTuple (a0 :> a1 :> a2 :> a3 :> a4 :> a5 :> a6 :> a7 :> a8 :> a9 :> a10 :> a11 :> a12 :> a13 :> a14 :> a15 :> a16 :> a17 :> a18 :> a19 :> a20 :> a21 :> a22 :> a23 :> a24 :> a25 :> Nil) = (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25)

instance VecToTuple (Vec 27 a) where
  type TupType (Vec 27 a) = (a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a)
  vecToTuple (a0 :> a1 :> a2 :> a3 :> a4 :> a5 :> a6 :> a7 :> a8 :> a9 :> a10 :> a11 :> a12 :> a13 :> a14 :> a15 :> a16 :> a17 :> a18 :> a19 :> a20 :> a21 :> a22 :> a23 :> a24 :> a25 :> a26 :> Nil) = (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26)

instance VecToTuple (Vec 28 a) where
  type TupType (Vec 28 a) = (a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a)
  vecToTuple (a0 :> a1 :> a2 :> a3 :> a4 :> a5 :> a6 :> a7 :> a8 :> a9 :> a10 :> a11 :> a12 :> a13 :> a14 :> a15 :> a16 :> a17 :> a18 :> a19 :> a20 :> a21 :> a22 :> a23 :> a24 :> a25 :> a26 :> a27 :> Nil) = (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27)

instance VecToTuple (Vec 29 a) where
  type TupType (Vec 29 a) = (a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a)
  vecToTuple (a0 :> a1 :> a2 :> a3 :> a4 :> a5 :> a6 :> a7 :> a8 :> a9 :> a10 :> a11 :> a12 :> a13 :> a14 :> a15 :> a16 :> a17 :> a18 :> a19 :> a20 :> a21 :> a22 :> a23 :> a24 :> a25 :> a26 :> a27 :> a28 :> Nil) = (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28)

instance VecToTuple (Vec 30 a) where
  type TupType (Vec 30 a) = (a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a)
  vecToTuple (a0 :> a1 :> a2 :> a3 :> a4 :> a5 :> a6 :> a7 :> a8 :> a9 :> a10 :> a11 :> a12 :> a13 :> a14 :> a15 :> a16 :> a17 :> a18 :> a19 :> a20 :> a21 :> a22 :> a23 :> a24 :> a25 :> a26 :> a27 :> a28 :> a29 :> Nil) = (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, a29)

instance VecToTuple (Vec 31 a) where
  type TupType (Vec 31 a) = (a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a)
  vecToTuple (a0 :> a1 :> a2 :> a3 :> a4 :> a5 :> a6 :> a7 :> a8 :> a9 :> a10 :> a11 :> a12 :> a13 :> a14 :> a15 :> a16 :> a17 :> a18 :> a19 :> a20 :> a21 :> a22 :> a23 :> a24 :> a25 :> a26 :> a27 :> a28 :> a29 :> a30 :> Nil) = (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, a29, a30)

instance VecToTuple (Vec 32 a) where
  type TupType (Vec 32 a) = (a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a)
  vecToTuple (a0 :> a1 :> a2 :> a3 :> a4 :> a5 :> a6 :> a7 :> a8 :> a9 :> a10 :> a11 :> a12 :> a13 :> a14 :> a15 :> a16 :> a17 :> a18 :> a19 :> a20 :> a21 :> a22 :> a23 :> a24 :> a25 :> a26 :> a27 :> a28 :> a29 :> a30 :> a31 :> Nil) = (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, a29, a30, a31)

instance VecToTuple (Vec 33 a) where
  type TupType (Vec 33 a) = (a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a)
  vecToTuple (a0 :> a1 :> a2 :> a3 :> a4 :> a5 :> a6 :> a7 :> a8 :> a9 :> a10 :> a11 :> a12 :> a13 :> a14 :> a15 :> a16 :> a17 :> a18 :> a19 :> a20 :> a21 :> a22 :> a23 :> a24 :> a25 :> a26 :> a27 :> a28 :> a29 :> a30 :> a31 :> a32 :> Nil) = (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, a29, a30, a31, a32)

instance VecToTuple (Vec 34 a) where
  type TupType (Vec 34 a) = (a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a)
  vecToTuple (a0 :> a1 :> a2 :> a3 :> a4 :> a5 :> a6 :> a7 :> a8 :> a9 :> a10 :> a11 :> a12 :> a13 :> a14 :> a15 :> a16 :> a17 :> a18 :> a19 :> a20 :> a21 :> a22 :> a23 :> a24 :> a25 :> a26 :> a27 :> a28 :> a29 :> a30 :> a31 :> a32 :> a33 :> Nil) = (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, a29, a30, a31, a32, a33)

instance VecToTuple (Vec 35 a) where
  type TupType (Vec 35 a) = (a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a)
  vecToTuple (a0 :> a1 :> a2 :> a3 :> a4 :> a5 :> a6 :> a7 :> a8 :> a9 :> a10 :> a11 :> a12 :> a13 :> a14 :> a15 :> a16 :> a17 :> a18 :> a19 :> a20 :> a21 :> a22 :> a23 :> a24 :> a25 :> a26 :> a27 :> a28 :> a29 :> a30 :> a31 :> a32 :> a33 :> a34 :> Nil) = (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, a29, a30, a31, a32, a33, a34)

instance VecToTuple (Vec 36 a) where
  type TupType (Vec 36 a) = (a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a)
  vecToTuple (a0 :> a1 :> a2 :> a3 :> a4 :> a5 :> a6 :> a7 :> a8 :> a9 :> a10 :> a11 :> a12 :> a13 :> a14 :> a15 :> a16 :> a17 :> a18 :> a19 :> a20 :> a21 :> a22 :> a23 :> a24 :> a25 :> a26 :> a27 :> a28 :> a29 :> a30 :> a31 :> a32 :> a33 :> a34 :> a35 :> Nil) = (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, a29, a30, a31, a32, a33, a34, a35)

instance VecToTuple (Vec 37 a) where
  type TupType (Vec 37 a) = (a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a)
  vecToTuple (a0 :> a1 :> a2 :> a3 :> a4 :> a5 :> a6 :> a7 :> a8 :> a9 :> a10 :> a11 :> a12 :> a13 :> a14 :> a15 :> a16 :> a17 :> a18 :> a19 :> a20 :> a21 :> a22 :> a23 :> a24 :> a25 :> a26 :> a27 :> a28 :> a29 :> a30 :> a31 :> a32 :> a33 :> a34 :> a35 :> a36 :> Nil) = (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, a29, a30, a31, a32, a33, a34, a35, a36)

instance VecToTuple (Vec 38 a) where
  type TupType (Vec 38 a) = (a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a)
  vecToTuple (a0 :> a1 :> a2 :> a3 :> a4 :> a5 :> a6 :> a7 :> a8 :> a9 :> a10 :> a11 :> a12 :> a13 :> a14 :> a15 :> a16 :> a17 :> a18 :> a19 :> a20 :> a21 :> a22 :> a23 :> a24 :> a25 :> a26 :> a27 :> a28 :> a29 :> a30 :> a31 :> a32 :> a33 :> a34 :> a35 :> a36 :> a37 :> Nil) = (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, a29, a30, a31, a32, a33, a34, a35, a36, a37)

instance VecToTuple (Vec 39 a) where
  type TupType (Vec 39 a) = (a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a)
  vecToTuple (a0 :> a1 :> a2 :> a3 :> a4 :> a5 :> a6 :> a7 :> a8 :> a9 :> a10 :> a11 :> a12 :> a13 :> a14 :> a15 :> a16 :> a17 :> a18 :> a19 :> a20 :> a21 :> a22 :> a23 :> a24 :> a25 :> a26 :> a27 :> a28 :> a29 :> a30 :> a31 :> a32 :> a33 :> a34 :> a35 :> a36 :> a37 :> a38 :> Nil) = (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, a29, a30, a31, a32, a33, a34, a35, a36, a37, a38)

instance VecToTuple (Vec 40 a) where
  type TupType (Vec 40 a) = (a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a)
  vecToTuple (a0 :> a1 :> a2 :> a3 :> a4 :> a5 :> a6 :> a7 :> a8 :> a9 :> a10 :> a11 :> a12 :> a13 :> a14 :> a15 :> a16 :> a17 :> a18 :> a19 :> a20 :> a21 :> a22 :> a23 :> a24 :> a25 :> a26 :> a27 :> a28 :> a29 :> a30 :> a31 :> a32 :> a33 :> a34 :> a35 :> a36 :> a37 :> a38 :> a39 :> Nil) = (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, a29, a30, a31, a32, a33, a34, a35, a36, a37, a38, a39)

instance VecToTuple (Vec 41 a) where
  type TupType (Vec 41 a) = (a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a)
  vecToTuple (a0 :> a1 :> a2 :> a3 :> a4 :> a5 :> a6 :> a7 :> a8 :> a9 :> a10 :> a11 :> a12 :> a13 :> a14 :> a15 :> a16 :> a17 :> a18 :> a19 :> a20 :> a21 :> a22 :> a23 :> a24 :> a25 :> a26 :> a27 :> a28 :> a29 :> a30 :> a31 :> a32 :> a33 :> a34 :> a35 :> a36 :> a37 :> a38 :> a39 :> a40 :> Nil) = (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, a29, a30, a31, a32, a33, a34, a35, a36, a37, a38, a39, a40)

instance VecToTuple (Vec 42 a) where
  type TupType (Vec 42 a) = (a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a)
  vecToTuple (a0 :> a1 :> a2 :> a3 :> a4 :> a5 :> a6 :> a7 :> a8 :> a9 :> a10 :> a11 :> a12 :> a13 :> a14 :> a15 :> a16 :> a17 :> a18 :> a19 :> a20 :> a21 :> a22 :> a23 :> a24 :> a25 :> a26 :> a27 :> a28 :> a29 :> a30 :> a31 :> a32 :> a33 :> a34 :> a35 :> a36 :> a37 :> a38 :> a39 :> a40 :> a41 :> Nil) = (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, a29, a30, a31, a32, a33, a34, a35, a36, a37, a38, a39, a40, a41)

instance VecToTuple (Vec 43 a) where
  type TupType (Vec 43 a) = (a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a)
  vecToTuple (a0 :> a1 :> a2 :> a3 :> a4 :> a5 :> a6 :> a7 :> a8 :> a9 :> a10 :> a11 :> a12 :> a13 :> a14 :> a15 :> a16 :> a17 :> a18 :> a19 :> a20 :> a21 :> a22 :> a23 :> a24 :> a25 :> a26 :> a27 :> a28 :> a29 :> a30 :> a31 :> a32 :> a33 :> a34 :> a35 :> a36 :> a37 :> a38 :> a39 :> a40 :> a41 :> a42 :> Nil) = (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, a29, a30, a31, a32, a33, a34, a35, a36, a37, a38, a39, a40, a41, a42)

instance VecToTuple (Vec 44 a) where
  type TupType (Vec 44 a) = (a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a)
  vecToTuple (a0 :> a1 :> a2 :> a3 :> a4 :> a5 :> a6 :> a7 :> a8 :> a9 :> a10 :> a11 :> a12 :> a13 :> a14 :> a15 :> a16 :> a17 :> a18 :> a19 :> a20 :> a21 :> a22 :> a23 :> a24 :> a25 :> a26 :> a27 :> a28 :> a29 :> a30 :> a31 :> a32 :> a33 :> a34 :> a35 :> a36 :> a37 :> a38 :> a39 :> a40 :> a41 :> a42 :> a43 :> Nil) = (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, a29, a30, a31, a32, a33, a34, a35, a36, a37, a38, a39, a40, a41, a42, a43)

instance VecToTuple (Vec 45 a) where
  type TupType (Vec 45 a) = (a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a)
  vecToTuple (a0 :> a1 :> a2 :> a3 :> a4 :> a5 :> a6 :> a7 :> a8 :> a9 :> a10 :> a11 :> a12 :> a13 :> a14 :> a15 :> a16 :> a17 :> a18 :> a19 :> a20 :> a21 :> a22 :> a23 :> a24 :> a25 :> a26 :> a27 :> a28 :> a29 :> a30 :> a31 :> a32 :> a33 :> a34 :> a35 :> a36 :> a37 :> a38 :> a39 :> a40 :> a41 :> a42 :> a43 :> a44 :> Nil) = (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, a29, a30, a31, a32, a33, a34, a35, a36, a37, a38, a39, a40, a41, a42, a43, a44)

instance VecToTuple (Vec 46 a) where
  type TupType (Vec 46 a) = (a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a)
  vecToTuple (a0 :> a1 :> a2 :> a3 :> a4 :> a5 :> a6 :> a7 :> a8 :> a9 :> a10 :> a11 :> a12 :> a13 :> a14 :> a15 :> a16 :> a17 :> a18 :> a19 :> a20 :> a21 :> a22 :> a23 :> a24 :> a25 :> a26 :> a27 :> a28 :> a29 :> a30 :> a31 :> a32 :> a33 :> a34 :> a35 :> a36 :> a37 :> a38 :> a39 :> a40 :> a41 :> a42 :> a43 :> a44 :> a45 :> Nil) = (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, a29, a30, a31, a32, a33, a34, a35, a36, a37, a38, a39, a40, a41, a42, a43, a44, a45)

instance VecToTuple (Vec 47 a) where
  type TupType (Vec 47 a) = (a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a)
  vecToTuple (a0 :> a1 :> a2 :> a3 :> a4 :> a5 :> a6 :> a7 :> a8 :> a9 :> a10 :> a11 :> a12 :> a13 :> a14 :> a15 :> a16 :> a17 :> a18 :> a19 :> a20 :> a21 :> a22 :> a23 :> a24 :> a25 :> a26 :> a27 :> a28 :> a29 :> a30 :> a31 :> a32 :> a33 :> a34 :> a35 :> a36 :> a37 :> a38 :> a39 :> a40 :> a41 :> a42 :> a43 :> a44 :> a45 :> a46 :> Nil) = (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, a29, a30, a31, a32, a33, a34, a35, a36, a37, a38, a39, a40, a41, a42, a43, a44, a45, a46)

instance VecToTuple (Vec 48 a) where
  type TupType (Vec 48 a) = (a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a)
  vecToTuple (a0 :> a1 :> a2 :> a3 :> a4 :> a5 :> a6 :> a7 :> a8 :> a9 :> a10 :> a11 :> a12 :> a13 :> a14 :> a15 :> a16 :> a17 :> a18 :> a19 :> a20 :> a21 :> a22 :> a23 :> a24 :> a25 :> a26 :> a27 :> a28 :> a29 :> a30 :> a31 :> a32 :> a33 :> a34 :> a35 :> a36 :> a37 :> a38 :> a39 :> a40 :> a41 :> a42 :> a43 :> a44 :> a45 :> a46 :> a47 :> Nil) = (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, a29, a30, a31, a32, a33, a34, a35, a36, a37, a38, a39, a40, a41, a42, a43, a44, a45, a46, a47)

instance VecToTuple (Vec 49 a) where
  type TupType (Vec 49 a) = (a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a)
  vecToTuple (a0 :> a1 :> a2 :> a3 :> a4 :> a5 :> a6 :> a7 :> a8 :> a9 :> a10 :> a11 :> a12 :> a13 :> a14 :> a15 :> a16 :> a17 :> a18 :> a19 :> a20 :> a21 :> a22 :> a23 :> a24 :> a25 :> a26 :> a27 :> a28 :> a29 :> a30 :> a31 :> a32 :> a33 :> a34 :> a35 :> a36 :> a37 :> a38 :> a39 :> a40 :> a41 :> a42 :> a43 :> a44 :> a45 :> a46 :> a47 :> a48 :> Nil) = (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, a29, a30, a31, a32, a33, a34, a35, a36, a37, a38, a39, a40, a41, a42, a43, a44, a45, a46, a47, a48)

instance VecToTuple (Vec 50 a) where
  type TupType (Vec 50 a) = (a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a)
  vecToTuple (a0 :> a1 :> a2 :> a3 :> a4 :> a5 :> a6 :> a7 :> a8 :> a9 :> a10 :> a11 :> a12 :> a13 :> a14 :> a15 :> a16 :> a17 :> a18 :> a19 :> a20 :> a21 :> a22 :> a23 :> a24 :> a25 :> a26 :> a27 :> a28 :> a29 :> a30 :> a31 :> a32 :> a33 :> a34 :> a35 :> a36 :> a37 :> a38 :> a39 :> a40 :> a41 :> a42 :> a43 :> a44 :> a45 :> a46 :> a47 :> a48 :> a49 :> Nil) = (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, a29, a30, a31, a32, a33, a34, a35, a36, a37, a38, a39, a40, a41, a42, a43, a44, a45, a46, a47, a48, a49)

instance VecToTuple (Vec 51 a) where
  type TupType (Vec 51 a) = (a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a)
  vecToTuple (a0 :> a1 :> a2 :> a3 :> a4 :> a5 :> a6 :> a7 :> a8 :> a9 :> a10 :> a11 :> a12 :> a13 :> a14 :> a15 :> a16 :> a17 :> a18 :> a19 :> a20 :> a21 :> a22 :> a23 :> a24 :> a25 :> a26 :> a27 :> a28 :> a29 :> a30 :> a31 :> a32 :> a33 :> a34 :> a35 :> a36 :> a37 :> a38 :> a39 :> a40 :> a41 :> a42 :> a43 :> a44 :> a45 :> a46 :> a47 :> a48 :> a49 :> a50 :> Nil) = (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, a29, a30, a31, a32, a33, a34, a35, a36, a37, a38, a39, a40, a41, a42, a43, a44, a45, a46, a47, a48, a49, a50)

instance VecToTuple (Vec 52 a) where
  type TupType (Vec 52 a) = (a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a)
  vecToTuple (a0 :> a1 :> a2 :> a3 :> a4 :> a5 :> a6 :> a7 :> a8 :> a9 :> a10 :> a11 :> a12 :> a13 :> a14 :> a15 :> a16 :> a17 :> a18 :> a19 :> a20 :> a21 :> a22 :> a23 :> a24 :> a25 :> a26 :> a27 :> a28 :> a29 :> a30 :> a31 :> a32 :> a33 :> a34 :> a35 :> a36 :> a37 :> a38 :> a39 :> a40 :> a41 :> a42 :> a43 :> a44 :> a45 :> a46 :> a47 :> a48 :> a49 :> a50 :> a51 :> Nil) = (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, a29, a30, a31, a32, a33, a34, a35, a36, a37, a38, a39, a40, a41, a42, a43, a44, a45, a46, a47, a48, a49, a50, a51)

instance VecToTuple (Vec 53 a) where
  type TupType (Vec 53 a) = (a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a)
  vecToTuple (a0 :> a1 :> a2 :> a3 :> a4 :> a5 :> a6 :> a7 :> a8 :> a9 :> a10 :> a11 :> a12 :> a13 :> a14 :> a15 :> a16 :> a17 :> a18 :> a19 :> a20 :> a21 :> a22 :> a23 :> a24 :> a25 :> a26 :> a27 :> a28 :> a29 :> a30 :> a31 :> a32 :> a33 :> a34 :> a35 :> a36 :> a37 :> a38 :> a39 :> a40 :> a41 :> a42 :> a43 :> a44 :> a45 :> a46 :> a47 :> a48 :> a49 :> a50 :> a51 :> a52 :> Nil) = (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, a29, a30, a31, a32, a33, a34, a35, a36, a37, a38, a39, a40, a41, a42, a43, a44, a45, a46, a47, a48, a49, a50, a51, a52)

instance VecToTuple (Vec 54 a) where
  type TupType (Vec 54 a) = (a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a)
  vecToTuple (a0 :> a1 :> a2 :> a3 :> a4 :> a5 :> a6 :> a7 :> a8 :> a9 :> a10 :> a11 :> a12 :> a13 :> a14 :> a15 :> a16 :> a17 :> a18 :> a19 :> a20 :> a21 :> a22 :> a23 :> a24 :> a25 :> a26 :> a27 :> a28 :> a29 :> a30 :> a31 :> a32 :> a33 :> a34 :> a35 :> a36 :> a37 :> a38 :> a39 :> a40 :> a41 :> a42 :> a43 :> a44 :> a45 :> a46 :> a47 :> a48 :> a49 :> a50 :> a51 :> a52 :> a53 :> Nil) = (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, a29, a30, a31, a32, a33, a34, a35, a36, a37, a38, a39, a40, a41, a42, a43, a44, a45, a46, a47, a48, a49, a50, a51, a52, a53)

instance VecToTuple (Vec 55 a) where
  type TupType (Vec 55 a) = (a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a)
  vecToTuple (a0 :> a1 :> a2 :> a3 :> a4 :> a5 :> a6 :> a7 :> a8 :> a9 :> a10 :> a11 :> a12 :> a13 :> a14 :> a15 :> a16 :> a17 :> a18 :> a19 :> a20 :> a21 :> a22 :> a23 :> a24 :> a25 :> a26 :> a27 :> a28 :> a29 :> a30 :> a31 :> a32 :> a33 :> a34 :> a35 :> a36 :> a37 :> a38 :> a39 :> a40 :> a41 :> a42 :> a43 :> a44 :> a45 :> a46 :> a47 :> a48 :> a49 :> a50 :> a51 :> a52 :> a53 :> a54 :> Nil) = (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, a29, a30, a31, a32, a33, a34, a35, a36, a37, a38, a39, a40, a41, a42, a43, a44, a45, a46, a47, a48, a49, a50, a51, a52, a53, a54)

instance VecToTuple (Vec 56 a) where
  type TupType (Vec 56 a) = (a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a)
  vecToTuple (a0 :> a1 :> a2 :> a3 :> a4 :> a5 :> a6 :> a7 :> a8 :> a9 :> a10 :> a11 :> a12 :> a13 :> a14 :> a15 :> a16 :> a17 :> a18 :> a19 :> a20 :> a21 :> a22 :> a23 :> a24 :> a25 :> a26 :> a27 :> a28 :> a29 :> a30 :> a31 :> a32 :> a33 :> a34 :> a35 :> a36 :> a37 :> a38 :> a39 :> a40 :> a41 :> a42 :> a43 :> a44 :> a45 :> a46 :> a47 :> a48 :> a49 :> a50 :> a51 :> a52 :> a53 :> a54 :> a55 :> Nil) = (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, a29, a30, a31, a32, a33, a34, a35, a36, a37, a38, a39, a40, a41, a42, a43, a44, a45, a46, a47, a48, a49, a50, a51, a52, a53, a54, a55)

instance VecToTuple (Vec 57 a) where
  type TupType (Vec 57 a) = (a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a)
  vecToTuple (a0 :> a1 :> a2 :> a3 :> a4 :> a5 :> a6 :> a7 :> a8 :> a9 :> a10 :> a11 :> a12 :> a13 :> a14 :> a15 :> a16 :> a17 :> a18 :> a19 :> a20 :> a21 :> a22 :> a23 :> a24 :> a25 :> a26 :> a27 :> a28 :> a29 :> a30 :> a31 :> a32 :> a33 :> a34 :> a35 :> a36 :> a37 :> a38 :> a39 :> a40 :> a41 :> a42 :> a43 :> a44 :> a45 :> a46 :> a47 :> a48 :> a49 :> a50 :> a51 :> a52 :> a53 :> a54 :> a55 :> a56 :> Nil) = (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, a29, a30, a31, a32, a33, a34, a35, a36, a37, a38, a39, a40, a41, a42, a43, a44, a45, a46, a47, a48, a49, a50, a51, a52, a53, a54, a55, a56)

instance VecToTuple (Vec 58 a) where
  type TupType (Vec 58 a) = (a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a)
  vecToTuple (a0 :> a1 :> a2 :> a3 :> a4 :> a5 :> a6 :> a7 :> a8 :> a9 :> a10 :> a11 :> a12 :> a13 :> a14 :> a15 :> a16 :> a17 :> a18 :> a19 :> a20 :> a21 :> a22 :> a23 :> a24 :> a25 :> a26 :> a27 :> a28 :> a29 :> a30 :> a31 :> a32 :> a33 :> a34 :> a35 :> a36 :> a37 :> a38 :> a39 :> a40 :> a41 :> a42 :> a43 :> a44 :> a45 :> a46 :> a47 :> a48 :> a49 :> a50 :> a51 :> a52 :> a53 :> a54 :> a55 :> a56 :> a57 :> Nil) = (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, a29, a30, a31, a32, a33, a34, a35, a36, a37, a38, a39, a40, a41, a42, a43, a44, a45, a46, a47, a48, a49, a50, a51, a52, a53, a54, a55, a56, a57)

instance VecToTuple (Vec 59 a) where
  type TupType (Vec 59 a) = (a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a)
  vecToTuple (a0 :> a1 :> a2 :> a3 :> a4 :> a5 :> a6 :> a7 :> a8 :> a9 :> a10 :> a11 :> a12 :> a13 :> a14 :> a15 :> a16 :> a17 :> a18 :> a19 :> a20 :> a21 :> a22 :> a23 :> a24 :> a25 :> a26 :> a27 :> a28 :> a29 :> a30 :> a31 :> a32 :> a33 :> a34 :> a35 :> a36 :> a37 :> a38 :> a39 :> a40 :> a41 :> a42 :> a43 :> a44 :> a45 :> a46 :> a47 :> a48 :> a49 :> a50 :> a51 :> a52 :> a53 :> a54 :> a55 :> a56 :> a57 :> a58 :> Nil) = (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, a29, a30, a31, a32, a33, a34, a35, a36, a37, a38, a39, a40, a41, a42, a43, a44, a45, a46, a47, a48, a49, a50, a51, a52, a53, a54, a55, a56, a57, a58)

instance VecToTuple (Vec 60 a) where
  type TupType (Vec 60 a) = (a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a)
  vecToTuple (a0 :> a1 :> a2 :> a3 :> a4 :> a5 :> a6 :> a7 :> a8 :> a9 :> a10 :> a11 :> a12 :> a13 :> a14 :> a15 :> a16 :> a17 :> a18 :> a19 :> a20 :> a21 :> a22 :> a23 :> a24 :> a25 :> a26 :> a27 :> a28 :> a29 :> a30 :> a31 :> a32 :> a33 :> a34 :> a35 :> a36 :> a37 :> a38 :> a39 :> a40 :> a41 :> a42 :> a43 :> a44 :> a45 :> a46 :> a47 :> a48 :> a49 :> a50 :> a51 :> a52 :> a53 :> a54 :> a55 :> a56 :> a57 :> a58 :> a59 :> Nil) = (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, a29, a30, a31, a32, a33, a34, a35, a36, a37, a38, a39, a40, a41, a42, a43, a44, a45, a46, a47, a48, a49, a50, a51, a52, a53, a54, a55, a56, a57, a58, a59)

instance VecToTuple (Vec 61 a) where
  type TupType (Vec 61 a) = (a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a)
  vecToTuple (a0 :> a1 :> a2 :> a3 :> a4 :> a5 :> a6 :> a7 :> a8 :> a9 :> a10 :> a11 :> a12 :> a13 :> a14 :> a15 :> a16 :> a17 :> a18 :> a19 :> a20 :> a21 :> a22 :> a23 :> a24 :> a25 :> a26 :> a27 :> a28 :> a29 :> a30 :> a31 :> a32 :> a33 :> a34 :> a35 :> a36 :> a37 :> a38 :> a39 :> a40 :> a41 :> a42 :> a43 :> a44 :> a45 :> a46 :> a47 :> a48 :> a49 :> a50 :> a51 :> a52 :> a53 :> a54 :> a55 :> a56 :> a57 :> a58 :> a59 :> a60 :> Nil) = (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, a29, a30, a31, a32, a33, a34, a35, a36, a37, a38, a39, a40, a41, a42, a43, a44, a45, a46, a47, a48, a49, a50, a51, a52, a53, a54, a55, a56, a57, a58, a59, a60)

instance VecToTuple (Vec 62 a) where
  type TupType (Vec 62 a) = (a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a)
  vecToTuple (a0 :> a1 :> a2 :> a3 :> a4 :> a5 :> a6 :> a7 :> a8 :> a9 :> a10 :> a11 :> a12 :> a13 :> a14 :> a15 :> a16 :> a17 :> a18 :> a19 :> a20 :> a21 :> a22 :> a23 :> a24 :> a25 :> a26 :> a27 :> a28 :> a29 :> a30 :> a31 :> a32 :> a33 :> a34 :> a35 :> a36 :> a37 :> a38 :> a39 :> a40 :> a41 :> a42 :> a43 :> a44 :> a45 :> a46 :> a47 :> a48 :> a49 :> a50 :> a51 :> a52 :> a53 :> a54 :> a55 :> a56 :> a57 :> a58 :> a59 :> a60 :> a61 :> Nil) = (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, a29, a30, a31, a32, a33, a34, a35, a36, a37, a38, a39, a40, a41, a42, a43, a44, a45, a46, a47, a48, a49, a50, a51, a52, a53, a54, a55, a56, a57, a58, a59, a60, a61)
