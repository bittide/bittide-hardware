-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Clash.Sized.Vector.Extra where

import Clash.Explicit.Prelude
import Control.Monad.Extra (guarded)
import Data.Maybe (fromJust, fromMaybe)
import GHC.Stack (HasCallStack)

{- $setup
>>> import Clash.Prelude
-}

{- | Finds first element in given vector matching the predicate. Returns
'Nothing' if no element satisfied the predicate.
-}
find :: (KnownNat n) => (a -> Bool) -> Vec n a -> Maybe a
find f = foldl (<|>) Nothing . map go
 where
  go a
    | f a = Just a
    | otherwise = Nothing

{- | Finds first element in given vector matching the predicate. Returns a
default element (the first argument) if no element satisfied the predicate.
-}
findWithDefault :: (KnownNat n) => a -> (a -> Bool) -> Vec n a -> a
findWithDefault a f = fromMaybe a . find f

{- | Generates a vector of incrementing numbers, skipping values that are in the blacklist.
Throws an error if it can't produce enough unique values.
>>> incrementWithBlacklist (1 :> 3 :> Nil) :: Vec 3 (Unsigned 8)
0 :> 2 :> 4 :> Nil
>>> incrementWithBlacklist Nil :: Vec 4 (Unsigned 2)
0 :> 1 :> 2 :> 3 :> Nil
>>> incrementWithBlacklist (0 :> Nil) :: Vec 3 (Unsigned 2)
1 :> 2 :> 3 :> Nil
-}
incrementWithBlacklist ::
  forall n m x.
  (HasCallStack, KnownNat m, KnownNat n, KnownNat x) =>
  Vec m (Unsigned x) ->
  Vec n (Unsigned x)
incrementWithBlacklist blackList
  | fromIntegral (maxBound :: Unsigned x) < (natToNum @(n + m) - 1 :: Integer) = err
  | otherwise = fmap fromJust $ takeI $ packVec results
 where
  err = clashCompileError "incrementWithBlacklist: Not enough unique values possible"
  candidates = iterateI @(n + m) go (Just minBound)

  go (Just n) = if n == maxBound then Nothing else Just (n + 1)
  go Nothing = Nothing

  results = fmap (>>= (guarded (`notElem` blackList))) candidates

  packVec = foldr f (repeat @(n + m) Nothing)
   where
    f (Just a) acc = Just a +>> acc
    f Nothing acc = acc

-- XXX: We need a bunch of zip functions due to an unfortunate coinciding bugs:
--
--        https://github.com/clash-lang/clash-compiler/issues/2723
--        https://github.com/clash-lang/clash-compiler/issues/2722
--

-- | Like 'zip', but for 8 vectors
zip8 ::
  Vec n a0 ->
  Vec n a1 ->
  Vec n a2 ->
  Vec n a3 ->
  Vec n a4 ->
  Vec n a5 ->
  Vec n a6 ->
  Vec n a7 ->
  Vec n (a0, a1, a2, a3, a4, a5, a6, a7)
zip8 = zipWith8 (,,,,,,,)
{-# INLINE zip8 #-}

-- | Like 'zipWith', but for 8 vectors
zipWith8 ::
  (a0 -> a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> a8) ->
  Vec n a0 ->
  Vec n a1 ->
  Vec n a2 ->
  Vec n a3 ->
  Vec n a4 ->
  Vec n a5 ->
  Vec n a6 ->
  Vec n a7 ->
  Vec n a8
zipWith8 f a0s a1s a2s a3s a4s a5s a6s a7s =
  zipWith
    (\a0 (a1, a2, a3, a4, a5, a6, a7) -> f a0 a1 a2 a3 a4 a5 a6 a7)
    a0s
    (zip7 a1s a2s a3s a4s a5s a6s a7s)
{-# INLINE zipWith8 #-}

-- | Like 'zip', but for 9 vectors
zip9 ::
  Vec n a0 ->
  Vec n a1 ->
  Vec n a2 ->
  Vec n a3 ->
  Vec n a4 ->
  Vec n a5 ->
  Vec n a6 ->
  Vec n a7 ->
  Vec n a8 ->
  Vec n (a0, a1, a2, a3, a4, a5, a6, a7, a8)
zip9 = zipWith9 (,,,,,,,,)
{-# INLINE zip9 #-}

-- | Like 'zipWith', but for 9 vectors
zipWith9 ::
  (a0 -> a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> a8 -> a9) ->
  Vec n a0 ->
  Vec n a1 ->
  Vec n a2 ->
  Vec n a3 ->
  Vec n a4 ->
  Vec n a5 ->
  Vec n a6 ->
  Vec n a7 ->
  Vec n a8 ->
  Vec n a9
zipWith9 f a0s a1s a2s a3s a4s a5s a6s a7s a8s =
  zipWith
    (\a0 (a1, a2, a3, a4, a5, a6, a7, a8) -> f a0 a1 a2 a3 a4 a5 a6 a7 a8)
    a0s
    (zip8 a1s a2s a3s a4s a5s a6s a7s a8s)
{-# INLINE zipWith9 #-}

-- | Like 'zip', but for 10 vectors
zip10 ::
  Vec n a0 ->
  Vec n a1 ->
  Vec n a2 ->
  Vec n a3 ->
  Vec n a4 ->
  Vec n a5 ->
  Vec n a6 ->
  Vec n a7 ->
  Vec n a8 ->
  Vec n a9 ->
  Vec n (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9)
zip10 = zipWith10 (,,,,,,,,,)
{-# INLINE zip10 #-}

-- | Like 'zipWith', but for 10 vectors
zipWith10 ::
  (a0 -> a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> a8 -> a9 -> a10) ->
  Vec n a0 ->
  Vec n a1 ->
  Vec n a2 ->
  Vec n a3 ->
  Vec n a4 ->
  Vec n a5 ->
  Vec n a6 ->
  Vec n a7 ->
  Vec n a8 ->
  Vec n a9 ->
  Vec n a10
zipWith10 f a0s a1s a2s a3s a4s a5s a6s a7s a8s a9s =
  zipWith
    (\a0 (a1, a2, a3, a4, a5, a6, a7, a8, a9) -> f a0 a1 a2 a3 a4 a5 a6 a7 a8 a9)
    a0s
    (zip9 a1s a2s a3s a4s a5s a6s a7s a8s a9s)
{-# INLINE zipWith10 #-}

-- | Like 'zip', but for 11 vectors
zip11 ::
  Vec n a0 ->
  Vec n a1 ->
  Vec n a2 ->
  Vec n a3 ->
  Vec n a4 ->
  Vec n a5 ->
  Vec n a6 ->
  Vec n a7 ->
  Vec n a8 ->
  Vec n a9 ->
  Vec n a10 ->
  Vec n (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)
zip11 = zipWith11 (,,,,,,,,,,)
{-# INLINE zip11 #-}

-- | Like 'zipWith', but for 11 vectors
zipWith11 ::
  (a0 -> a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> a8 -> a9 -> a10 -> a11) ->
  Vec n a0 ->
  Vec n a1 ->
  Vec n a2 ->
  Vec n a3 ->
  Vec n a4 ->
  Vec n a5 ->
  Vec n a6 ->
  Vec n a7 ->
  Vec n a8 ->
  Vec n a9 ->
  Vec n a10 ->
  Vec n a11
zipWith11 f a0s a1s a2s a3s a4s a5s a6s a7s a8s a9s a10s =
  zipWith
    (\a0 (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) -> f a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10)
    a0s
    (zip10 a1s a2s a3s a4s a5s a6s a7s a8s a9s a10s)
{-# INLINE zipWith11 #-}

-- | Like 'zip', but for 12 vectors
zip12 ::
  Vec n a0 ->
  Vec n a1 ->
  Vec n a2 ->
  Vec n a3 ->
  Vec n a4 ->
  Vec n a5 ->
  Vec n a6 ->
  Vec n a7 ->
  Vec n a8 ->
  Vec n a9 ->
  Vec n a10 ->
  Vec n a11 ->
  Vec n (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11)
zip12 = zipWith12 (,,,,,,,,,,,)
{-# INLINE zip12 #-}

-- | Like 'zipWith', but for 12 vectors
zipWith12 ::
  (a0 -> a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> a8 -> a9 -> a10 -> a11 -> a12) ->
  Vec n a0 ->
  Vec n a1 ->
  Vec n a2 ->
  Vec n a3 ->
  Vec n a4 ->
  Vec n a5 ->
  Vec n a6 ->
  Vec n a7 ->
  Vec n a8 ->
  Vec n a9 ->
  Vec n a10 ->
  Vec n a11 ->
  Vec n a12
zipWith12 f a0s a1s a2s a3s a4s a5s a6s a7s a8s a9s a10s a11s =
  zipWith
    ( \a0 (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11) -> f a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11
    )
    a0s
    (zip11 a1s a2s a3s a4s a5s a6s a7s a8s a9s a10s a11s)
{-# INLINE zipWith12 #-}

-- | Like 'zip', but for 13 vectors
zip13 ::
  Vec n a0 ->
  Vec n a1 ->
  Vec n a2 ->
  Vec n a3 ->
  Vec n a4 ->
  Vec n a5 ->
  Vec n a6 ->
  Vec n a7 ->
  Vec n a8 ->
  Vec n a9 ->
  Vec n a10 ->
  Vec n a11 ->
  Vec n a12 ->
  Vec n (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12)
zip13 = zipWith13 (,,,,,,,,,,,,)
{-# INLINE zip13 #-}

-- | Like 'zipWith', but for 13 vectors
zipWith13 ::
  (a0 -> a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> a8 -> a9 -> a10 -> a11 -> a12 -> a13) ->
  Vec n a0 ->
  Vec n a1 ->
  Vec n a2 ->
  Vec n a3 ->
  Vec n a4 ->
  Vec n a5 ->
  Vec n a6 ->
  Vec n a7 ->
  Vec n a8 ->
  Vec n a9 ->
  Vec n a10 ->
  Vec n a11 ->
  Vec n a12 ->
  Vec n a13
zipWith13 f a0s a1s a2s a3s a4s a5s a6s a7s a8s a9s a10s a11s a12s =
  zipWith
    ( \a0 (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) -> f a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12
    )
    a0s
    (zip12 a1s a2s a3s a4s a5s a6s a7s a8s a9s a10s a11s a12s)
{-# INLINE zipWith13 #-}

-- | Like 'zip', but for 14 vectors
zip14 ::
  Vec n a0 ->
  Vec n a1 ->
  Vec n a2 ->
  Vec n a3 ->
  Vec n a4 ->
  Vec n a5 ->
  Vec n a6 ->
  Vec n a7 ->
  Vec n a8 ->
  Vec n a9 ->
  Vec n a10 ->
  Vec n a11 ->
  Vec n a12 ->
  Vec n a13 ->
  Vec n (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13)
zip14 = zipWith14 (,,,,,,,,,,,,,)
{-# INLINE zip14 #-}

-- | Like 'zipWith', but for 14 vectors
zipWith14 ::
  ( a0 ->
    a1 ->
    a2 ->
    a3 ->
    a4 ->
    a5 ->
    a6 ->
    a7 ->
    a8 ->
    a9 ->
    a10 ->
    a11 ->
    a12 ->
    a13 ->
    a14
  ) ->
  Vec n a0 ->
  Vec n a1 ->
  Vec n a2 ->
  Vec n a3 ->
  Vec n a4 ->
  Vec n a5 ->
  Vec n a6 ->
  Vec n a7 ->
  Vec n a8 ->
  Vec n a9 ->
  Vec n a10 ->
  Vec n a11 ->
  Vec n a12 ->
  Vec n a13 ->
  Vec n a14
zipWith14 f a0s a1s a2s a3s a4s a5s a6s a7s a8s a9s a10s a11s a12s a13s =
  zipWith
    ( \a0 (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13) -> f a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13
    )
    a0s
    (zip13 a1s a2s a3s a4s a5s a6s a7s a8s a9s a10s a11s a12s a13s)
{-# INLINE zipWith14 #-}

-- | Like 'zip', but for 15 vectors
zip15 ::
  Vec n a0 ->
  Vec n a1 ->
  Vec n a2 ->
  Vec n a3 ->
  Vec n a4 ->
  Vec n a5 ->
  Vec n a6 ->
  Vec n a7 ->
  Vec n a8 ->
  Vec n a9 ->
  Vec n a10 ->
  Vec n a11 ->
  Vec n a12 ->
  Vec n a13 ->
  Vec n a14 ->
  Vec n (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14)
zip15 = zipWith15 (,,,,,,,,,,,,,,)
{-# INLINE zip15 #-}

-- | Like 'zipWith', but for 15 vectors
zipWith15 ::
  ( a0 ->
    a1 ->
    a2 ->
    a3 ->
    a4 ->
    a5 ->
    a6 ->
    a7 ->
    a8 ->
    a9 ->
    a10 ->
    a11 ->
    a12 ->
    a13 ->
    a14 ->
    a15
  ) ->
  Vec n a0 ->
  Vec n a1 ->
  Vec n a2 ->
  Vec n a3 ->
  Vec n a4 ->
  Vec n a5 ->
  Vec n a6 ->
  Vec n a7 ->
  Vec n a8 ->
  Vec n a9 ->
  Vec n a10 ->
  Vec n a11 ->
  Vec n a12 ->
  Vec n a13 ->
  Vec n a14 ->
  Vec n a15
zipWith15 f a0s a1s a2s a3s a4s a5s a6s a7s a8s a9s a10s a11s a12s a13s a14s =
  zipWith
    ( \a0 (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14) -> f a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14
    )
    a0s
    (zip14 a1s a2s a3s a4s a5s a6s a7s a8s a9s a10s a11s a12s a13s a14s)
{-# INLINE zipWith15 #-}

-- | Like 'zip', but for 16 vectors
zip16 ::
  Vec n a0 ->
  Vec n a1 ->
  Vec n a2 ->
  Vec n a3 ->
  Vec n a4 ->
  Vec n a5 ->
  Vec n a6 ->
  Vec n a7 ->
  Vec n a8 ->
  Vec n a9 ->
  Vec n a10 ->
  Vec n a11 ->
  Vec n a12 ->
  Vec n a13 ->
  Vec n a14 ->
  Vec n a15 ->
  Vec n (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15)
zip16 = zipWith16 (,,,,,,,,,,,,,,,)
{-# INLINE zip16 #-}

-- | Like 'zipWith', but for 16 vectors
zipWith16 ::
  ( a0 ->
    a1 ->
    a2 ->
    a3 ->
    a4 ->
    a5 ->
    a6 ->
    a7 ->
    a8 ->
    a9 ->
    a10 ->
    a11 ->
    a12 ->
    a13 ->
    a14 ->
    a15 ->
    a16
  ) ->
  Vec n a0 ->
  Vec n a1 ->
  Vec n a2 ->
  Vec n a3 ->
  Vec n a4 ->
  Vec n a5 ->
  Vec n a6 ->
  Vec n a7 ->
  Vec n a8 ->
  Vec n a9 ->
  Vec n a10 ->
  Vec n a11 ->
  Vec n a12 ->
  Vec n a13 ->
  Vec n a14 ->
  Vec n a15 ->
  Vec n a16
zipWith16 f a0s a1s a2s a3s a4s a5s a6s a7s a8s a9s a10s a11s a12s a13s a14s a15s =
  zipWith
    ( \a0 (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15) -> f a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15
    )
    a0s
    (zip15 a1s a2s a3s a4s a5s a6s a7s a8s a9s a10s a11s a12s a13s a14s a15s)
{-# INLINE zipWith16 #-}

-- | Like 'zip', but for 17 vectors
zip17 ::
  Vec n a0 ->
  Vec n a1 ->
  Vec n a2 ->
  Vec n a3 ->
  Vec n a4 ->
  Vec n a5 ->
  Vec n a6 ->
  Vec n a7 ->
  Vec n a8 ->
  Vec n a9 ->
  Vec n a10 ->
  Vec n a11 ->
  Vec n a12 ->
  Vec n a13 ->
  Vec n a14 ->
  Vec n a15 ->
  Vec n a16 ->
  Vec n (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16)
zip17 = zipWith17 (,,,,,,,,,,,,,,,,)
{-# INLINE zip17 #-}

-- | Like 'zipWith', but for 17 vectors
zipWith17 ::
  ( a0 ->
    a1 ->
    a2 ->
    a3 ->
    a4 ->
    a5 ->
    a6 ->
    a7 ->
    a8 ->
    a9 ->
    a10 ->
    a11 ->
    a12 ->
    a13 ->
    a14 ->
    a15 ->
    a16 ->
    a17
  ) ->
  Vec n a0 ->
  Vec n a1 ->
  Vec n a2 ->
  Vec n a3 ->
  Vec n a4 ->
  Vec n a5 ->
  Vec n a6 ->
  Vec n a7 ->
  Vec n a8 ->
  Vec n a9 ->
  Vec n a10 ->
  Vec n a11 ->
  Vec n a12 ->
  Vec n a13 ->
  Vec n a14 ->
  Vec n a15 ->
  Vec n a16 ->
  Vec n a17
zipWith17 f a0s a1s a2s a3s a4s a5s a6s a7s a8s a9s a10s a11s a12s a13s a14s a15s a16s =
  zipWith
    ( \a0 (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16) -> f a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16
    )
    a0s
    (zip16 a1s a2s a3s a4s a5s a6s a7s a8s a9s a10s a11s a12s a13s a14s a15s a16s)
{-# INLINE zipWith17 #-}

-- | Like 'zip', but for 18 vectors
zip18 ::
  Vec n a0 ->
  Vec n a1 ->
  Vec n a2 ->
  Vec n a3 ->
  Vec n a4 ->
  Vec n a5 ->
  Vec n a6 ->
  Vec n a7 ->
  Vec n a8 ->
  Vec n a9 ->
  Vec n a10 ->
  Vec n a11 ->
  Vec n a12 ->
  Vec n a13 ->
  Vec n a14 ->
  Vec n a15 ->
  Vec n a16 ->
  Vec n a17 ->
  Vec n (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17)
zip18 = zipWith18 (,,,,,,,,,,,,,,,,,)
{-# INLINE zip18 #-}

-- | Like 'zipWith', but for 18 vectors
zipWith18 ::
  ( a0 ->
    a1 ->
    a2 ->
    a3 ->
    a4 ->
    a5 ->
    a6 ->
    a7 ->
    a8 ->
    a9 ->
    a10 ->
    a11 ->
    a12 ->
    a13 ->
    a14 ->
    a15 ->
    a16 ->
    a17 ->
    a18
  ) ->
  Vec n a0 ->
  Vec n a1 ->
  Vec n a2 ->
  Vec n a3 ->
  Vec n a4 ->
  Vec n a5 ->
  Vec n a6 ->
  Vec n a7 ->
  Vec n a8 ->
  Vec n a9 ->
  Vec n a10 ->
  Vec n a11 ->
  Vec n a12 ->
  Vec n a13 ->
  Vec n a14 ->
  Vec n a15 ->
  Vec n a16 ->
  Vec n a17 ->
  Vec n a18
zipWith18 f a0s a1s a2s a3s a4s a5s a6s a7s a8s a9s a10s a11s a12s a13s a14s a15s a16s a17s =
  zipWith
    ( \a0 (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17) -> f a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17
    )
    a0s
    (zip17 a1s a2s a3s a4s a5s a6s a7s a8s a9s a10s a11s a12s a13s a14s a15s a16s a17s)
{-# INLINE zipWith18 #-}

-- | Like 'zip', but for 19 vectors
zip19 ::
  Vec n a0 ->
  Vec n a1 ->
  Vec n a2 ->
  Vec n a3 ->
  Vec n a4 ->
  Vec n a5 ->
  Vec n a6 ->
  Vec n a7 ->
  Vec n a8 ->
  Vec n a9 ->
  Vec n a10 ->
  Vec n a11 ->
  Vec n a12 ->
  Vec n a13 ->
  Vec n a14 ->
  Vec n a15 ->
  Vec n a16 ->
  Vec n a17 ->
  Vec n a18 ->
  Vec
    n
    (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18)
zip19 = zipWith19 (,,,,,,,,,,,,,,,,,,)
{-# INLINE zip19 #-}

-- | Like 'zipWith', but for 19 vectors
zipWith19 ::
  ( a0 ->
    a1 ->
    a2 ->
    a3 ->
    a4 ->
    a5 ->
    a6 ->
    a7 ->
    a8 ->
    a9 ->
    a10 ->
    a11 ->
    a12 ->
    a13 ->
    a14 ->
    a15 ->
    a16 ->
    a17 ->
    a18 ->
    a19
  ) ->
  Vec n a0 ->
  Vec n a1 ->
  Vec n a2 ->
  Vec n a3 ->
  Vec n a4 ->
  Vec n a5 ->
  Vec n a6 ->
  Vec n a7 ->
  Vec n a8 ->
  Vec n a9 ->
  Vec n a10 ->
  Vec n a11 ->
  Vec n a12 ->
  Vec n a13 ->
  Vec n a14 ->
  Vec n a15 ->
  Vec n a16 ->
  Vec n a17 ->
  Vec n a18 ->
  Vec n a19
zipWith19 f a0s a1s a2s a3s a4s a5s a6s a7s a8s a9s a10s a11s a12s a13s a14s a15s a16s a17s a18s =
  zipWith
    ( \a0 (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18) -> f a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18
    )
    a0s
    (zip18 a1s a2s a3s a4s a5s a6s a7s a8s a9s a10s a11s a12s a13s a14s a15s a16s a17s a18s)
{-# INLINE zipWith19 #-}

-- | Like 'zip', but for 20 vectors
zip20 ::
  Vec n a0 ->
  Vec n a1 ->
  Vec n a2 ->
  Vec n a3 ->
  Vec n a4 ->
  Vec n a5 ->
  Vec n a6 ->
  Vec n a7 ->
  Vec n a8 ->
  Vec n a9 ->
  Vec n a10 ->
  Vec n a11 ->
  Vec n a12 ->
  Vec n a13 ->
  Vec n a14 ->
  Vec n a15 ->
  Vec n a16 ->
  Vec n a17 ->
  Vec n a18 ->
  Vec n a19 ->
  Vec
    n
    (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19)
zip20 = zipWith20 (,,,,,,,,,,,,,,,,,,,)
{-# INLINE zip20 #-}

-- | Like 'zipWith', but for 20 vectors
zipWith20 ::
  ( a0 ->
    a1 ->
    a2 ->
    a3 ->
    a4 ->
    a5 ->
    a6 ->
    a7 ->
    a8 ->
    a9 ->
    a10 ->
    a11 ->
    a12 ->
    a13 ->
    a14 ->
    a15 ->
    a16 ->
    a17 ->
    a18 ->
    a19 ->
    a20
  ) ->
  Vec n a0 ->
  Vec n a1 ->
  Vec n a2 ->
  Vec n a3 ->
  Vec n a4 ->
  Vec n a5 ->
  Vec n a6 ->
  Vec n a7 ->
  Vec n a8 ->
  Vec n a9 ->
  Vec n a10 ->
  Vec n a11 ->
  Vec n a12 ->
  Vec n a13 ->
  Vec n a14 ->
  Vec n a15 ->
  Vec n a16 ->
  Vec n a17 ->
  Vec n a18 ->
  Vec n a19 ->
  Vec n a20
zipWith20 f a0s a1s a2s a3s a4s a5s a6s a7s a8s a9s a10s a11s a12s a13s a14s a15s a16s a17s a18s a19s =
  zipWith
    ( \a0 (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19) -> f a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19
    )
    a0s
    ( zip19
        a1s
        a2s
        a3s
        a4s
        a5s
        a6s
        a7s
        a8s
        a9s
        a10s
        a11s
        a12s
        a13s
        a14s
        a15s
        a16s
        a17s
        a18s
        a19s
    )
{-# INLINE zipWith20 #-}

-- | Like 'zip', but for 21 vectors
zip21 ::
  Vec n a0 ->
  Vec n a1 ->
  Vec n a2 ->
  Vec n a3 ->
  Vec n a4 ->
  Vec n a5 ->
  Vec n a6 ->
  Vec n a7 ->
  Vec n a8 ->
  Vec n a9 ->
  Vec n a10 ->
  Vec n a11 ->
  Vec n a12 ->
  Vec n a13 ->
  Vec n a14 ->
  Vec n a15 ->
  Vec n a16 ->
  Vec n a17 ->
  Vec n a18 ->
  Vec n a19 ->
  Vec n a20 ->
  Vec
    n
    ( a0
    , a1
    , a2
    , a3
    , a4
    , a5
    , a6
    , a7
    , a8
    , a9
    , a10
    , a11
    , a12
    , a13
    , a14
    , a15
    , a16
    , a17
    , a18
    , a19
    , a20
    )
zip21 = zipWith21 (,,,,,,,,,,,,,,,,,,,,)
{-# INLINE zip21 #-}

-- | Like 'zipWith', but for 21 vectors
zipWith21 ::
  ( a0 ->
    a1 ->
    a2 ->
    a3 ->
    a4 ->
    a5 ->
    a6 ->
    a7 ->
    a8 ->
    a9 ->
    a10 ->
    a11 ->
    a12 ->
    a13 ->
    a14 ->
    a15 ->
    a16 ->
    a17 ->
    a18 ->
    a19 ->
    a20 ->
    a21
  ) ->
  Vec n a0 ->
  Vec n a1 ->
  Vec n a2 ->
  Vec n a3 ->
  Vec n a4 ->
  Vec n a5 ->
  Vec n a6 ->
  Vec n a7 ->
  Vec n a8 ->
  Vec n a9 ->
  Vec n a10 ->
  Vec n a11 ->
  Vec n a12 ->
  Vec n a13 ->
  Vec n a14 ->
  Vec n a15 ->
  Vec n a16 ->
  Vec n a17 ->
  Vec n a18 ->
  Vec n a19 ->
  Vec n a20 ->
  Vec n a21
zipWith21 f a0s a1s a2s a3s a4s a5s a6s a7s a8s a9s a10s a11s a12s a13s a14s a15s a16s a17s a18s a19s a20s =
  zipWith
    ( \a0
       ( a1
        , a2
        , a3
        , a4
        , a5
        , a6
        , a7
        , a8
        , a9
        , a10
        , a11
        , a12
        , a13
        , a14
        , a15
        , a16
        , a17
        , a18
        , a19
        , a20
        ) -> f a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20
    )
    a0s
    ( zip20
        a1s
        a2s
        a3s
        a4s
        a5s
        a6s
        a7s
        a8s
        a9s
        a10s
        a11s
        a12s
        a13s
        a14s
        a15s
        a16s
        a17s
        a18s
        a19s
        a20s
    )
{-# INLINE zipWith21 #-}

-- | Like 'zip', but for 22 vectors
zip22 ::
  Vec n a0 ->
  Vec n a1 ->
  Vec n a2 ->
  Vec n a3 ->
  Vec n a4 ->
  Vec n a5 ->
  Vec n a6 ->
  Vec n a7 ->
  Vec n a8 ->
  Vec n a9 ->
  Vec n a10 ->
  Vec n a11 ->
  Vec n a12 ->
  Vec n a13 ->
  Vec n a14 ->
  Vec n a15 ->
  Vec n a16 ->
  Vec n a17 ->
  Vec n a18 ->
  Vec n a19 ->
  Vec n a20 ->
  Vec n a21 ->
  Vec
    n
    ( a0
    , a1
    , a2
    , a3
    , a4
    , a5
    , a6
    , a7
    , a8
    , a9
    , a10
    , a11
    , a12
    , a13
    , a14
    , a15
    , a16
    , a17
    , a18
    , a19
    , a20
    , a21
    )
zip22 = zipWith22 (,,,,,,,,,,,,,,,,,,,,,)
{-# INLINE zip22 #-}

-- | Like 'zipWith', but for 22 vectors
zipWith22 ::
  ( a0 ->
    a1 ->
    a2 ->
    a3 ->
    a4 ->
    a5 ->
    a6 ->
    a7 ->
    a8 ->
    a9 ->
    a10 ->
    a11 ->
    a12 ->
    a13 ->
    a14 ->
    a15 ->
    a16 ->
    a17 ->
    a18 ->
    a19 ->
    a20 ->
    a21 ->
    a22
  ) ->
  Vec n a0 ->
  Vec n a1 ->
  Vec n a2 ->
  Vec n a3 ->
  Vec n a4 ->
  Vec n a5 ->
  Vec n a6 ->
  Vec n a7 ->
  Vec n a8 ->
  Vec n a9 ->
  Vec n a10 ->
  Vec n a11 ->
  Vec n a12 ->
  Vec n a13 ->
  Vec n a14 ->
  Vec n a15 ->
  Vec n a16 ->
  Vec n a17 ->
  Vec n a18 ->
  Vec n a19 ->
  Vec n a20 ->
  Vec n a21 ->
  Vec n a22
zipWith22 f a0s a1s a2s a3s a4s a5s a6s a7s a8s a9s a10s a11s a12s a13s a14s a15s a16s a17s a18s a19s a20s a21s =
  zipWith
    ( \a0
       ( a1
        , a2
        , a3
        , a4
        , a5
        , a6
        , a7
        , a8
        , a9
        , a10
        , a11
        , a12
        , a13
        , a14
        , a15
        , a16
        , a17
        , a18
        , a19
        , a20
        , a21
        ) -> f a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20 a21
    )
    a0s
    ( zip21
        a1s
        a2s
        a3s
        a4s
        a5s
        a6s
        a7s
        a8s
        a9s
        a10s
        a11s
        a12s
        a13s
        a14s
        a15s
        a16s
        a17s
        a18s
        a19s
        a20s
        a21s
    )
{-# INLINE zipWith22 #-}

-- | Like 'zip', but for 23 vectors
zip23 ::
  Vec n a0 ->
  Vec n a1 ->
  Vec n a2 ->
  Vec n a3 ->
  Vec n a4 ->
  Vec n a5 ->
  Vec n a6 ->
  Vec n a7 ->
  Vec n a8 ->
  Vec n a9 ->
  Vec n a10 ->
  Vec n a11 ->
  Vec n a12 ->
  Vec n a13 ->
  Vec n a14 ->
  Vec n a15 ->
  Vec n a16 ->
  Vec n a17 ->
  Vec n a18 ->
  Vec n a19 ->
  Vec n a20 ->
  Vec n a21 ->
  Vec n a22 ->
  Vec
    n
    ( a0
    , a1
    , a2
    , a3
    , a4
    , a5
    , a6
    , a7
    , a8
    , a9
    , a10
    , a11
    , a12
    , a13
    , a14
    , a15
    , a16
    , a17
    , a18
    , a19
    , a20
    , a21
    , a22
    )
zip23 = zipWith23 (,,,,,,,,,,,,,,,,,,,,,,)
{-# INLINE zip23 #-}

-- | Like 'zipWith', but for 23 vectors
zipWith23 ::
  ( a0 ->
    a1 ->
    a2 ->
    a3 ->
    a4 ->
    a5 ->
    a6 ->
    a7 ->
    a8 ->
    a9 ->
    a10 ->
    a11 ->
    a12 ->
    a13 ->
    a14 ->
    a15 ->
    a16 ->
    a17 ->
    a18 ->
    a19 ->
    a20 ->
    a21 ->
    a22 ->
    a23
  ) ->
  Vec n a0 ->
  Vec n a1 ->
  Vec n a2 ->
  Vec n a3 ->
  Vec n a4 ->
  Vec n a5 ->
  Vec n a6 ->
  Vec n a7 ->
  Vec n a8 ->
  Vec n a9 ->
  Vec n a10 ->
  Vec n a11 ->
  Vec n a12 ->
  Vec n a13 ->
  Vec n a14 ->
  Vec n a15 ->
  Vec n a16 ->
  Vec n a17 ->
  Vec n a18 ->
  Vec n a19 ->
  Vec n a20 ->
  Vec n a21 ->
  Vec n a22 ->
  Vec n a23
zipWith23 f a0s a1s a2s a3s a4s a5s a6s a7s a8s a9s a10s a11s a12s a13s a14s a15s a16s a17s a18s a19s a20s a21s a22s =
  zipWith
    ( \a0
       ( a1
        , a2
        , a3
        , a4
        , a5
        , a6
        , a7
        , a8
        , a9
        , a10
        , a11
        , a12
        , a13
        , a14
        , a15
        , a16
        , a17
        , a18
        , a19
        , a20
        , a21
        , a22
        ) -> f a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20 a21 a22
    )
    a0s
    ( zip22
        a1s
        a2s
        a3s
        a4s
        a5s
        a6s
        a7s
        a8s
        a9s
        a10s
        a11s
        a12s
        a13s
        a14s
        a15s
        a16s
        a17s
        a18s
        a19s
        a20s
        a21s
        a22s
    )
{-# INLINE zipWith23 #-}

-- | Like 'zip', but for 24 vectors
zip24 ::
  Vec n a0 ->
  Vec n a1 ->
  Vec n a2 ->
  Vec n a3 ->
  Vec n a4 ->
  Vec n a5 ->
  Vec n a6 ->
  Vec n a7 ->
  Vec n a8 ->
  Vec n a9 ->
  Vec n a10 ->
  Vec n a11 ->
  Vec n a12 ->
  Vec n a13 ->
  Vec n a14 ->
  Vec n a15 ->
  Vec n a16 ->
  Vec n a17 ->
  Vec n a18 ->
  Vec n a19 ->
  Vec n a20 ->
  Vec n a21 ->
  Vec n a22 ->
  Vec n a23 ->
  Vec
    n
    ( a0
    , a1
    , a2
    , a3
    , a4
    , a5
    , a6
    , a7
    , a8
    , a9
    , a10
    , a11
    , a12
    , a13
    , a14
    , a15
    , a16
    , a17
    , a18
    , a19
    , a20
    , a21
    , a22
    , a23
    )
zip24 = zipWith24 (,,,,,,,,,,,,,,,,,,,,,,,)
{-# INLINE zip24 #-}

-- | Like 'zipWith', but for 24 vectors
zipWith24 ::
  ( a0 ->
    a1 ->
    a2 ->
    a3 ->
    a4 ->
    a5 ->
    a6 ->
    a7 ->
    a8 ->
    a9 ->
    a10 ->
    a11 ->
    a12 ->
    a13 ->
    a14 ->
    a15 ->
    a16 ->
    a17 ->
    a18 ->
    a19 ->
    a20 ->
    a21 ->
    a22 ->
    a23 ->
    a24
  ) ->
  Vec n a0 ->
  Vec n a1 ->
  Vec n a2 ->
  Vec n a3 ->
  Vec n a4 ->
  Vec n a5 ->
  Vec n a6 ->
  Vec n a7 ->
  Vec n a8 ->
  Vec n a9 ->
  Vec n a10 ->
  Vec n a11 ->
  Vec n a12 ->
  Vec n a13 ->
  Vec n a14 ->
  Vec n a15 ->
  Vec n a16 ->
  Vec n a17 ->
  Vec n a18 ->
  Vec n a19 ->
  Vec n a20 ->
  Vec n a21 ->
  Vec n a22 ->
  Vec n a23 ->
  Vec n a24
zipWith24 f a0s a1s a2s a3s a4s a5s a6s a7s a8s a9s a10s a11s a12s a13s a14s a15s a16s a17s a18s a19s a20s a21s a22s a23s =
  zipWith
    ( \a0
       ( a1
        , a2
        , a3
        , a4
        , a5
        , a6
        , a7
        , a8
        , a9
        , a10
        , a11
        , a12
        , a13
        , a14
        , a15
        , a16
        , a17
        , a18
        , a19
        , a20
        , a21
        , a22
        , a23
        ) ->
          f a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20 a21 a22 a23
    )
    a0s
    ( zip23
        a1s
        a2s
        a3s
        a4s
        a5s
        a6s
        a7s
        a8s
        a9s
        a10s
        a11s
        a12s
        a13s
        a14s
        a15s
        a16s
        a17s
        a18s
        a19s
        a20s
        a21s
        a22s
        a23s
    )
{-# INLINE zipWith24 #-}

-- | Like 'zip', but for 25 vectors
zip25 ::
  Vec n a0 ->
  Vec n a1 ->
  Vec n a2 ->
  Vec n a3 ->
  Vec n a4 ->
  Vec n a5 ->
  Vec n a6 ->
  Vec n a7 ->
  Vec n a8 ->
  Vec n a9 ->
  Vec n a10 ->
  Vec n a11 ->
  Vec n a12 ->
  Vec n a13 ->
  Vec n a14 ->
  Vec n a15 ->
  Vec n a16 ->
  Vec n a17 ->
  Vec n a18 ->
  Vec n a19 ->
  Vec n a20 ->
  Vec n a21 ->
  Vec n a22 ->
  Vec n a23 ->
  Vec n a24 ->
  Vec
    n
    ( a0
    , a1
    , a2
    , a3
    , a4
    , a5
    , a6
    , a7
    , a8
    , a9
    , a10
    , a11
    , a12
    , a13
    , a14
    , a15
    , a16
    , a17
    , a18
    , a19
    , a20
    , a21
    , a22
    , a23
    , a24
    )
zip25 = zipWith25 (,,,,,,,,,,,,,,,,,,,,,,,,)
{-# INLINE zip25 #-}

-- | Like 'zipWith', but for 25 vectors
zipWith25 ::
  ( a0 ->
    a1 ->
    a2 ->
    a3 ->
    a4 ->
    a5 ->
    a6 ->
    a7 ->
    a8 ->
    a9 ->
    a10 ->
    a11 ->
    a12 ->
    a13 ->
    a14 ->
    a15 ->
    a16 ->
    a17 ->
    a18 ->
    a19 ->
    a20 ->
    a21 ->
    a22 ->
    a23 ->
    a24 ->
    a25
  ) ->
  Vec n a0 ->
  Vec n a1 ->
  Vec n a2 ->
  Vec n a3 ->
  Vec n a4 ->
  Vec n a5 ->
  Vec n a6 ->
  Vec n a7 ->
  Vec n a8 ->
  Vec n a9 ->
  Vec n a10 ->
  Vec n a11 ->
  Vec n a12 ->
  Vec n a13 ->
  Vec n a14 ->
  Vec n a15 ->
  Vec n a16 ->
  Vec n a17 ->
  Vec n a18 ->
  Vec n a19 ->
  Vec n a20 ->
  Vec n a21 ->
  Vec n a22 ->
  Vec n a23 ->
  Vec n a24 ->
  Vec n a25
zipWith25 f a0s a1s a2s a3s a4s a5s a6s a7s a8s a9s a10s a11s a12s a13s a14s a15s a16s a17s a18s a19s a20s a21s a22s a23s a24s =
  zipWith
    ( \a0
       ( a1
        , a2
        , a3
        , a4
        , a5
        , a6
        , a7
        , a8
        , a9
        , a10
        , a11
        , a12
        , a13
        , a14
        , a15
        , a16
        , a17
        , a18
        , a19
        , a20
        , a21
        , a22
        , a23
        , a24
        ) ->
          f
            a0
            a1
            a2
            a3
            a4
            a5
            a6
            a7
            a8
            a9
            a10
            a11
            a12
            a13
            a14
            a15
            a16
            a17
            a18
            a19
            a20
            a21
            a22
            a23
            a24
    )
    a0s
    ( zip24
        a1s
        a2s
        a3s
        a4s
        a5s
        a6s
        a7s
        a8s
        a9s
        a10s
        a11s
        a12s
        a13s
        a14s
        a15s
        a16s
        a17s
        a18s
        a19s
        a20s
        a21s
        a22s
        a23s
        a24s
    )
{-# INLINE zipWith25 #-}

-- | Like 'zip', but for 26 vectors
zip26 ::
  Vec n a0 ->
  Vec n a1 ->
  Vec n a2 ->
  Vec n a3 ->
  Vec n a4 ->
  Vec n a5 ->
  Vec n a6 ->
  Vec n a7 ->
  Vec n a8 ->
  Vec n a9 ->
  Vec n a10 ->
  Vec n a11 ->
  Vec n a12 ->
  Vec n a13 ->
  Vec n a14 ->
  Vec n a15 ->
  Vec n a16 ->
  Vec n a17 ->
  Vec n a18 ->
  Vec n a19 ->
  Vec n a20 ->
  Vec n a21 ->
  Vec n a22 ->
  Vec n a23 ->
  Vec n a24 ->
  Vec n a25 ->
  Vec
    n
    ( a0
    , a1
    , a2
    , a3
    , a4
    , a5
    , a6
    , a7
    , a8
    , a9
    , a10
    , a11
    , a12
    , a13
    , a14
    , a15
    , a16
    , a17
    , a18
    , a19
    , a20
    , a21
    , a22
    , a23
    , a24
    , a25
    )
zip26 = zipWith26 (,,,,,,,,,,,,,,,,,,,,,,,,,)
{-# INLINE zip26 #-}

-- | Like 'zipWith', but for 26 vectors
zipWith26 ::
  ( a0 ->
    a1 ->
    a2 ->
    a3 ->
    a4 ->
    a5 ->
    a6 ->
    a7 ->
    a8 ->
    a9 ->
    a10 ->
    a11 ->
    a12 ->
    a13 ->
    a14 ->
    a15 ->
    a16 ->
    a17 ->
    a18 ->
    a19 ->
    a20 ->
    a21 ->
    a22 ->
    a23 ->
    a24 ->
    a25 ->
    a26
  ) ->
  Vec n a0 ->
  Vec n a1 ->
  Vec n a2 ->
  Vec n a3 ->
  Vec n a4 ->
  Vec n a5 ->
  Vec n a6 ->
  Vec n a7 ->
  Vec n a8 ->
  Vec n a9 ->
  Vec n a10 ->
  Vec n a11 ->
  Vec n a12 ->
  Vec n a13 ->
  Vec n a14 ->
  Vec n a15 ->
  Vec n a16 ->
  Vec n a17 ->
  Vec n a18 ->
  Vec n a19 ->
  Vec n a20 ->
  Vec n a21 ->
  Vec n a22 ->
  Vec n a23 ->
  Vec n a24 ->
  Vec n a25 ->
  Vec n a26
zipWith26 f a0s a1s a2s a3s a4s a5s a6s a7s a8s a9s a10s a11s a12s a13s a14s a15s a16s a17s a18s a19s a20s a21s a22s a23s a24s a25s =
  zipWith
    ( \a0
       ( a1
        , a2
        , a3
        , a4
        , a5
        , a6
        , a7
        , a8
        , a9
        , a10
        , a11
        , a12
        , a13
        , a14
        , a15
        , a16
        , a17
        , a18
        , a19
        , a20
        , a21
        , a22
        , a23
        , a24
        , a25
        ) ->
          f
            a0
            a1
            a2
            a3
            a4
            a5
            a6
            a7
            a8
            a9
            a10
            a11
            a12
            a13
            a14
            a15
            a16
            a17
            a18
            a19
            a20
            a21
            a22
            a23
            a24
            a25
    )
    a0s
    ( zip25
        a1s
        a2s
        a3s
        a4s
        a5s
        a6s
        a7s
        a8s
        a9s
        a10s
        a11s
        a12s
        a13s
        a14s
        a15s
        a16s
        a17s
        a18s
        a19s
        a20s
        a21s
        a22s
        a23s
        a24s
        a25s
    )
{-# INLINE zipWith26 #-}

-- | Like 'zip', but for 27 vectors
zip27 ::
  Vec n a0 ->
  Vec n a1 ->
  Vec n a2 ->
  Vec n a3 ->
  Vec n a4 ->
  Vec n a5 ->
  Vec n a6 ->
  Vec n a7 ->
  Vec n a8 ->
  Vec n a9 ->
  Vec n a10 ->
  Vec n a11 ->
  Vec n a12 ->
  Vec n a13 ->
  Vec n a14 ->
  Vec n a15 ->
  Vec n a16 ->
  Vec n a17 ->
  Vec n a18 ->
  Vec n a19 ->
  Vec n a20 ->
  Vec n a21 ->
  Vec n a22 ->
  Vec n a23 ->
  Vec n a24 ->
  Vec n a25 ->
  Vec n a26 ->
  Vec
    n
    ( a0
    , a1
    , a2
    , a3
    , a4
    , a5
    , a6
    , a7
    , a8
    , a9
    , a10
    , a11
    , a12
    , a13
    , a14
    , a15
    , a16
    , a17
    , a18
    , a19
    , a20
    , a21
    , a22
    , a23
    , a24
    , a25
    , a26
    )
zip27 = zipWith27 (,,,,,,,,,,,,,,,,,,,,,,,,,,)
{-# INLINE zip27 #-}

-- | Like 'zipWith', but for 27 vectors
zipWith27 ::
  ( a0 ->
    a1 ->
    a2 ->
    a3 ->
    a4 ->
    a5 ->
    a6 ->
    a7 ->
    a8 ->
    a9 ->
    a10 ->
    a11 ->
    a12 ->
    a13 ->
    a14 ->
    a15 ->
    a16 ->
    a17 ->
    a18 ->
    a19 ->
    a20 ->
    a21 ->
    a22 ->
    a23 ->
    a24 ->
    a25 ->
    a26 ->
    a27
  ) ->
  Vec n a0 ->
  Vec n a1 ->
  Vec n a2 ->
  Vec n a3 ->
  Vec n a4 ->
  Vec n a5 ->
  Vec n a6 ->
  Vec n a7 ->
  Vec n a8 ->
  Vec n a9 ->
  Vec n a10 ->
  Vec n a11 ->
  Vec n a12 ->
  Vec n a13 ->
  Vec n a14 ->
  Vec n a15 ->
  Vec n a16 ->
  Vec n a17 ->
  Vec n a18 ->
  Vec n a19 ->
  Vec n a20 ->
  Vec n a21 ->
  Vec n a22 ->
  Vec n a23 ->
  Vec n a24 ->
  Vec n a25 ->
  Vec n a26 ->
  Vec n a27
zipWith27 f a0s a1s a2s a3s a4s a5s a6s a7s a8s a9s a10s a11s a12s a13s a14s a15s a16s a17s a18s a19s a20s a21s a22s a23s a24s a25s a26s =
  zipWith
    ( \a0
       ( a1
        , a2
        , a3
        , a4
        , a5
        , a6
        , a7
        , a8
        , a9
        , a10
        , a11
        , a12
        , a13
        , a14
        , a15
        , a16
        , a17
        , a18
        , a19
        , a20
        , a21
        , a22
        , a23
        , a24
        , a25
        , a26
        ) ->
          f
            a0
            a1
            a2
            a3
            a4
            a5
            a6
            a7
            a8
            a9
            a10
            a11
            a12
            a13
            a14
            a15
            a16
            a17
            a18
            a19
            a20
            a21
            a22
            a23
            a24
            a25
            a26
    )
    a0s
    ( zip26
        a1s
        a2s
        a3s
        a4s
        a5s
        a6s
        a7s
        a8s
        a9s
        a10s
        a11s
        a12s
        a13s
        a14s
        a15s
        a16s
        a17s
        a18s
        a19s
        a20s
        a21s
        a22s
        a23s
        a24s
        a25s
        a26s
    )
{-# INLINE zipWith27 #-}

-- | Like 'zip', but for 28 vectors
zip28 ::
  Vec n a0 ->
  Vec n a1 ->
  Vec n a2 ->
  Vec n a3 ->
  Vec n a4 ->
  Vec n a5 ->
  Vec n a6 ->
  Vec n a7 ->
  Vec n a8 ->
  Vec n a9 ->
  Vec n a10 ->
  Vec n a11 ->
  Vec n a12 ->
  Vec n a13 ->
  Vec n a14 ->
  Vec n a15 ->
  Vec n a16 ->
  Vec n a17 ->
  Vec n a18 ->
  Vec n a19 ->
  Vec n a20 ->
  Vec n a21 ->
  Vec n a22 ->
  Vec n a23 ->
  Vec n a24 ->
  Vec n a25 ->
  Vec n a26 ->
  Vec n a27 ->
  Vec
    n
    ( a0
    , a1
    , a2
    , a3
    , a4
    , a5
    , a6
    , a7
    , a8
    , a9
    , a10
    , a11
    , a12
    , a13
    , a14
    , a15
    , a16
    , a17
    , a18
    , a19
    , a20
    , a21
    , a22
    , a23
    , a24
    , a25
    , a26
    , a27
    )
zip28 = zipWith28 (,,,,,,,,,,,,,,,,,,,,,,,,,,,)
{-# INLINE zip28 #-}

-- | Like 'zipWith', but for 28 vectors
zipWith28 ::
  ( a0 ->
    a1 ->
    a2 ->
    a3 ->
    a4 ->
    a5 ->
    a6 ->
    a7 ->
    a8 ->
    a9 ->
    a10 ->
    a11 ->
    a12 ->
    a13 ->
    a14 ->
    a15 ->
    a16 ->
    a17 ->
    a18 ->
    a19 ->
    a20 ->
    a21 ->
    a22 ->
    a23 ->
    a24 ->
    a25 ->
    a26 ->
    a27 ->
    a28
  ) ->
  Vec n a0 ->
  Vec n a1 ->
  Vec n a2 ->
  Vec n a3 ->
  Vec n a4 ->
  Vec n a5 ->
  Vec n a6 ->
  Vec n a7 ->
  Vec n a8 ->
  Vec n a9 ->
  Vec n a10 ->
  Vec n a11 ->
  Vec n a12 ->
  Vec n a13 ->
  Vec n a14 ->
  Vec n a15 ->
  Vec n a16 ->
  Vec n a17 ->
  Vec n a18 ->
  Vec n a19 ->
  Vec n a20 ->
  Vec n a21 ->
  Vec n a22 ->
  Vec n a23 ->
  Vec n a24 ->
  Vec n a25 ->
  Vec n a26 ->
  Vec n a27 ->
  Vec n a28
zipWith28 f a0s a1s a2s a3s a4s a5s a6s a7s a8s a9s a10s a11s a12s a13s a14s a15s a16s a17s a18s a19s a20s a21s a22s a23s a24s a25s a26s a27s =
  zipWith
    ( \a0
       ( a1
        , a2
        , a3
        , a4
        , a5
        , a6
        , a7
        , a8
        , a9
        , a10
        , a11
        , a12
        , a13
        , a14
        , a15
        , a16
        , a17
        , a18
        , a19
        , a20
        , a21
        , a22
        , a23
        , a24
        , a25
        , a26
        , a27
        ) ->
          f
            a0
            a1
            a2
            a3
            a4
            a5
            a6
            a7
            a8
            a9
            a10
            a11
            a12
            a13
            a14
            a15
            a16
            a17
            a18
            a19
            a20
            a21
            a22
            a23
            a24
            a25
            a26
            a27
    )
    a0s
    ( zip27
        a1s
        a2s
        a3s
        a4s
        a5s
        a6s
        a7s
        a8s
        a9s
        a10s
        a11s
        a12s
        a13s
        a14s
        a15s
        a16s
        a17s
        a18s
        a19s
        a20s
        a21s
        a22s
        a23s
        a24s
        a25s
        a26s
        a27s
    )
{-# INLINE zipWith28 #-}

-- | Like 'zip', but for 29 vectors
zip29 ::
  Vec n a0 ->
  Vec n a1 ->
  Vec n a2 ->
  Vec n a3 ->
  Vec n a4 ->
  Vec n a5 ->
  Vec n a6 ->
  Vec n a7 ->
  Vec n a8 ->
  Vec n a9 ->
  Vec n a10 ->
  Vec n a11 ->
  Vec n a12 ->
  Vec n a13 ->
  Vec n a14 ->
  Vec n a15 ->
  Vec n a16 ->
  Vec n a17 ->
  Vec n a18 ->
  Vec n a19 ->
  Vec n a20 ->
  Vec n a21 ->
  Vec n a22 ->
  Vec n a23 ->
  Vec n a24 ->
  Vec n a25 ->
  Vec n a26 ->
  Vec n a27 ->
  Vec n a28 ->
  Vec
    n
    ( a0
    , a1
    , a2
    , a3
    , a4
    , a5
    , a6
    , a7
    , a8
    , a9
    , a10
    , a11
    , a12
    , a13
    , a14
    , a15
    , a16
    , a17
    , a18
    , a19
    , a20
    , a21
    , a22
    , a23
    , a24
    , a25
    , a26
    , a27
    , a28
    )
zip29 = zipWith29 (,,,,,,,,,,,,,,,,,,,,,,,,,,,,)
{-# INLINE zip29 #-}

-- | Like 'zipWith', but for 29 vectors
zipWith29 ::
  ( a0 ->
    a1 ->
    a2 ->
    a3 ->
    a4 ->
    a5 ->
    a6 ->
    a7 ->
    a8 ->
    a9 ->
    a10 ->
    a11 ->
    a12 ->
    a13 ->
    a14 ->
    a15 ->
    a16 ->
    a17 ->
    a18 ->
    a19 ->
    a20 ->
    a21 ->
    a22 ->
    a23 ->
    a24 ->
    a25 ->
    a26 ->
    a27 ->
    a28 ->
    a29
  ) ->
  Vec n a0 ->
  Vec n a1 ->
  Vec n a2 ->
  Vec n a3 ->
  Vec n a4 ->
  Vec n a5 ->
  Vec n a6 ->
  Vec n a7 ->
  Vec n a8 ->
  Vec n a9 ->
  Vec n a10 ->
  Vec n a11 ->
  Vec n a12 ->
  Vec n a13 ->
  Vec n a14 ->
  Vec n a15 ->
  Vec n a16 ->
  Vec n a17 ->
  Vec n a18 ->
  Vec n a19 ->
  Vec n a20 ->
  Vec n a21 ->
  Vec n a22 ->
  Vec n a23 ->
  Vec n a24 ->
  Vec n a25 ->
  Vec n a26 ->
  Vec n a27 ->
  Vec n a28 ->
  Vec n a29
zipWith29 f a0s a1s a2s a3s a4s a5s a6s a7s a8s a9s a10s a11s a12s a13s a14s a15s a16s a17s a18s a19s a20s a21s a22s a23s a24s a25s a26s a27s a28s =
  zipWith
    ( \a0
       ( a1
        , a2
        , a3
        , a4
        , a5
        , a6
        , a7
        , a8
        , a9
        , a10
        , a11
        , a12
        , a13
        , a14
        , a15
        , a16
        , a17
        , a18
        , a19
        , a20
        , a21
        , a22
        , a23
        , a24
        , a25
        , a26
        , a27
        , a28
        ) ->
          f
            a0
            a1
            a2
            a3
            a4
            a5
            a6
            a7
            a8
            a9
            a10
            a11
            a12
            a13
            a14
            a15
            a16
            a17
            a18
            a19
            a20
            a21
            a22
            a23
            a24
            a25
            a26
            a27
            a28
    )
    a0s
    ( zip28
        a1s
        a2s
        a3s
        a4s
        a5s
        a6s
        a7s
        a8s
        a9s
        a10s
        a11s
        a12s
        a13s
        a14s
        a15s
        a16s
        a17s
        a18s
        a19s
        a20s
        a21s
        a22s
        a23s
        a24s
        a25s
        a26s
        a27s
        a28s
    )
{-# INLINE zipWith29 #-}

-- | Like 'zip', but for 30 vectors
zip30 ::
  Vec n a0 ->
  Vec n a1 ->
  Vec n a2 ->
  Vec n a3 ->
  Vec n a4 ->
  Vec n a5 ->
  Vec n a6 ->
  Vec n a7 ->
  Vec n a8 ->
  Vec n a9 ->
  Vec n a10 ->
  Vec n a11 ->
  Vec n a12 ->
  Vec n a13 ->
  Vec n a14 ->
  Vec n a15 ->
  Vec n a16 ->
  Vec n a17 ->
  Vec n a18 ->
  Vec n a19 ->
  Vec n a20 ->
  Vec n a21 ->
  Vec n a22 ->
  Vec n a23 ->
  Vec n a24 ->
  Vec n a25 ->
  Vec n a26 ->
  Vec n a27 ->
  Vec n a28 ->
  Vec n a29 ->
  Vec
    n
    ( a0
    , a1
    , a2
    , a3
    , a4
    , a5
    , a6
    , a7
    , a8
    , a9
    , a10
    , a11
    , a12
    , a13
    , a14
    , a15
    , a16
    , a17
    , a18
    , a19
    , a20
    , a21
    , a22
    , a23
    , a24
    , a25
    , a26
    , a27
    , a28
    , a29
    )
zip30 = zipWith30 (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,)
{-# INLINE zip30 #-}

-- | Like 'zipWith', but for 30 vectors
zipWith30 ::
  ( a0 ->
    a1 ->
    a2 ->
    a3 ->
    a4 ->
    a5 ->
    a6 ->
    a7 ->
    a8 ->
    a9 ->
    a10 ->
    a11 ->
    a12 ->
    a13 ->
    a14 ->
    a15 ->
    a16 ->
    a17 ->
    a18 ->
    a19 ->
    a20 ->
    a21 ->
    a22 ->
    a23 ->
    a24 ->
    a25 ->
    a26 ->
    a27 ->
    a28 ->
    a29 ->
    a30
  ) ->
  Vec n a0 ->
  Vec n a1 ->
  Vec n a2 ->
  Vec n a3 ->
  Vec n a4 ->
  Vec n a5 ->
  Vec n a6 ->
  Vec n a7 ->
  Vec n a8 ->
  Vec n a9 ->
  Vec n a10 ->
  Vec n a11 ->
  Vec n a12 ->
  Vec n a13 ->
  Vec n a14 ->
  Vec n a15 ->
  Vec n a16 ->
  Vec n a17 ->
  Vec n a18 ->
  Vec n a19 ->
  Vec n a20 ->
  Vec n a21 ->
  Vec n a22 ->
  Vec n a23 ->
  Vec n a24 ->
  Vec n a25 ->
  Vec n a26 ->
  Vec n a27 ->
  Vec n a28 ->
  Vec n a29 ->
  Vec n a30
zipWith30 f a0s a1s a2s a3s a4s a5s a6s a7s a8s a9s a10s a11s a12s a13s a14s a15s a16s a17s a18s a19s a20s a21s a22s a23s a24s a25s a26s a27s a28s a29s =
  zipWith
    ( \a0
       ( a1
        , a2
        , a3
        , a4
        , a5
        , a6
        , a7
        , a8
        , a9
        , a10
        , a11
        , a12
        , a13
        , a14
        , a15
        , a16
        , a17
        , a18
        , a19
        , a20
        , a21
        , a22
        , a23
        , a24
        , a25
        , a26
        , a27
        , a28
        , a29
        ) ->
          f
            a0
            a1
            a2
            a3
            a4
            a5
            a6
            a7
            a8
            a9
            a10
            a11
            a12
            a13
            a14
            a15
            a16
            a17
            a18
            a19
            a20
            a21
            a22
            a23
            a24
            a25
            a26
            a27
            a28
            a29
    )
    a0s
    ( zip29
        a1s
        a2s
        a3s
        a4s
        a5s
        a6s
        a7s
        a8s
        a9s
        a10s
        a11s
        a12s
        a13s
        a14s
        a15s
        a16s
        a17s
        a18s
        a19s
        a20s
        a21s
        a22s
        a23s
        a24s
        a25s
        a26s
        a27s
        a28s
        a29s
    )
{-# INLINE zipWith30 #-}

-- | Like 'zip', but for 31 vectors
zip31 ::
  Vec n a0 ->
  Vec n a1 ->
  Vec n a2 ->
  Vec n a3 ->
  Vec n a4 ->
  Vec n a5 ->
  Vec n a6 ->
  Vec n a7 ->
  Vec n a8 ->
  Vec n a9 ->
  Vec n a10 ->
  Vec n a11 ->
  Vec n a12 ->
  Vec n a13 ->
  Vec n a14 ->
  Vec n a15 ->
  Vec n a16 ->
  Vec n a17 ->
  Vec n a18 ->
  Vec n a19 ->
  Vec n a20 ->
  Vec n a21 ->
  Vec n a22 ->
  Vec n a23 ->
  Vec n a24 ->
  Vec n a25 ->
  Vec n a26 ->
  Vec n a27 ->
  Vec n a28 ->
  Vec n a29 ->
  Vec n a30 ->
  Vec
    n
    ( a0
    , a1
    , a2
    , a3
    , a4
    , a5
    , a6
    , a7
    , a8
    , a9
    , a10
    , a11
    , a12
    , a13
    , a14
    , a15
    , a16
    , a17
    , a18
    , a19
    , a20
    , a21
    , a22
    , a23
    , a24
    , a25
    , a26
    , a27
    , a28
    , a29
    , a30
    )
zip31 = zipWith31 (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,)
{-# INLINE zip31 #-}

-- | Like 'zipWith', but for 31 vectors
zipWith31 ::
  ( a0 ->
    a1 ->
    a2 ->
    a3 ->
    a4 ->
    a5 ->
    a6 ->
    a7 ->
    a8 ->
    a9 ->
    a10 ->
    a11 ->
    a12 ->
    a13 ->
    a14 ->
    a15 ->
    a16 ->
    a17 ->
    a18 ->
    a19 ->
    a20 ->
    a21 ->
    a22 ->
    a23 ->
    a24 ->
    a25 ->
    a26 ->
    a27 ->
    a28 ->
    a29 ->
    a30 ->
    a31
  ) ->
  Vec n a0 ->
  Vec n a1 ->
  Vec n a2 ->
  Vec n a3 ->
  Vec n a4 ->
  Vec n a5 ->
  Vec n a6 ->
  Vec n a7 ->
  Vec n a8 ->
  Vec n a9 ->
  Vec n a10 ->
  Vec n a11 ->
  Vec n a12 ->
  Vec n a13 ->
  Vec n a14 ->
  Vec n a15 ->
  Vec n a16 ->
  Vec n a17 ->
  Vec n a18 ->
  Vec n a19 ->
  Vec n a20 ->
  Vec n a21 ->
  Vec n a22 ->
  Vec n a23 ->
  Vec n a24 ->
  Vec n a25 ->
  Vec n a26 ->
  Vec n a27 ->
  Vec n a28 ->
  Vec n a29 ->
  Vec n a30 ->
  Vec n a31
zipWith31 f a0s a1s a2s a3s a4s a5s a6s a7s a8s a9s a10s a11s a12s a13s a14s a15s a16s a17s a18s a19s a20s a21s a22s a23s a24s a25s a26s a27s a28s a29s a30s =
  zipWith
    ( \a0
       ( a1
        , a2
        , a3
        , a4
        , a5
        , a6
        , a7
        , a8
        , a9
        , a10
        , a11
        , a12
        , a13
        , a14
        , a15
        , a16
        , a17
        , a18
        , a19
        , a20
        , a21
        , a22
        , a23
        , a24
        , a25
        , a26
        , a27
        , a28
        , a29
        , a30
        ) ->
          f
            a0
            a1
            a2
            a3
            a4
            a5
            a6
            a7
            a8
            a9
            a10
            a11
            a12
            a13
            a14
            a15
            a16
            a17
            a18
            a19
            a20
            a21
            a22
            a23
            a24
            a25
            a26
            a27
            a28
            a29
            a30
    )
    a0s
    ( zip30
        a1s
        a2s
        a3s
        a4s
        a5s
        a6s
        a7s
        a8s
        a9s
        a10s
        a11s
        a12s
        a13s
        a14s
        a15s
        a16s
        a17s
        a18s
        a19s
        a20s
        a21s
        a22s
        a23s
        a24s
        a25s
        a26s
        a27s
        a28s
        a29s
        a30s
    )
{-# INLINE zipWith31 #-}

-- | Like 'zip', but for 32 vectors
zip32 ::
  Vec n a0 ->
  Vec n a1 ->
  Vec n a2 ->
  Vec n a3 ->
  Vec n a4 ->
  Vec n a5 ->
  Vec n a6 ->
  Vec n a7 ->
  Vec n a8 ->
  Vec n a9 ->
  Vec n a10 ->
  Vec n a11 ->
  Vec n a12 ->
  Vec n a13 ->
  Vec n a14 ->
  Vec n a15 ->
  Vec n a16 ->
  Vec n a17 ->
  Vec n a18 ->
  Vec n a19 ->
  Vec n a20 ->
  Vec n a21 ->
  Vec n a22 ->
  Vec n a23 ->
  Vec n a24 ->
  Vec n a25 ->
  Vec n a26 ->
  Vec n a27 ->
  Vec n a28 ->
  Vec n a29 ->
  Vec n a30 ->
  Vec n a31 ->
  Vec
    n
    ( a0
    , a1
    , a2
    , a3
    , a4
    , a5
    , a6
    , a7
    , a8
    , a9
    , a10
    , a11
    , a12
    , a13
    , a14
    , a15
    , a16
    , a17
    , a18
    , a19
    , a20
    , a21
    , a22
    , a23
    , a24
    , a25
    , a26
    , a27
    , a28
    , a29
    , a30
    , a31
    )
zip32 = zipWith32 (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,)
{-# INLINE zip32 #-}

-- | Like 'zipWith', but for 32 vectors
zipWith32 ::
  ( a0 ->
    a1 ->
    a2 ->
    a3 ->
    a4 ->
    a5 ->
    a6 ->
    a7 ->
    a8 ->
    a9 ->
    a10 ->
    a11 ->
    a12 ->
    a13 ->
    a14 ->
    a15 ->
    a16 ->
    a17 ->
    a18 ->
    a19 ->
    a20 ->
    a21 ->
    a22 ->
    a23 ->
    a24 ->
    a25 ->
    a26 ->
    a27 ->
    a28 ->
    a29 ->
    a30 ->
    a31 ->
    a32
  ) ->
  Vec n a0 ->
  Vec n a1 ->
  Vec n a2 ->
  Vec n a3 ->
  Vec n a4 ->
  Vec n a5 ->
  Vec n a6 ->
  Vec n a7 ->
  Vec n a8 ->
  Vec n a9 ->
  Vec n a10 ->
  Vec n a11 ->
  Vec n a12 ->
  Vec n a13 ->
  Vec n a14 ->
  Vec n a15 ->
  Vec n a16 ->
  Vec n a17 ->
  Vec n a18 ->
  Vec n a19 ->
  Vec n a20 ->
  Vec n a21 ->
  Vec n a22 ->
  Vec n a23 ->
  Vec n a24 ->
  Vec n a25 ->
  Vec n a26 ->
  Vec n a27 ->
  Vec n a28 ->
  Vec n a29 ->
  Vec n a30 ->
  Vec n a31 ->
  Vec n a32
zipWith32 f a0s a1s a2s a3s a4s a5s a6s a7s a8s a9s a10s a11s a12s a13s a14s a15s a16s a17s a18s a19s a20s a21s a22s a23s a24s a25s a26s a27s a28s a29s a30s a31s =
  zipWith
    ( \a0
       ( a1
        , a2
        , a3
        , a4
        , a5
        , a6
        , a7
        , a8
        , a9
        , a10
        , a11
        , a12
        , a13
        , a14
        , a15
        , a16
        , a17
        , a18
        , a19
        , a20
        , a21
        , a22
        , a23
        , a24
        , a25
        , a26
        , a27
        , a28
        , a29
        , a30
        , a31
        ) ->
          f
            a0
            a1
            a2
            a3
            a4
            a5
            a6
            a7
            a8
            a9
            a10
            a11
            a12
            a13
            a14
            a15
            a16
            a17
            a18
            a19
            a20
            a21
            a22
            a23
            a24
            a25
            a26
            a27
            a28
            a29
            a30
            a31
    )
    a0s
    ( zip31
        a1s
        a2s
        a3s
        a4s
        a5s
        a6s
        a7s
        a8s
        a9s
        a10s
        a11s
        a12s
        a13s
        a14s
        a15s
        a16s
        a17s
        a18s
        a19s
        a20s
        a21s
        a22s
        a23s
        a24s
        a25s
        a26s
        a27s
        a28s
        a29s
        a30s
        a31s
    )
{-# INLINE zipWith32 #-}
