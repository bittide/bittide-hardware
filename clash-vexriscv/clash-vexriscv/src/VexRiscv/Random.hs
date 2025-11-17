-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE CPP #-}

module VexRiscv.Random where

import Clash.Prelude

-- See https://github.com/clash-lang/clash-compiler/issues/2707
#if __GLASGOW_HASKELL__ < 902
import Numeric.Natural
#endif

import Clash.Sized.Internal.BitVector
import Data.Bifunctor (bimap)
import GHC.IO (unsafePerformIO)
import System.Random

{- | Unsafe version of 'makeDefinedRandom' with a NOINLINE pragma to avoid
compiler optimizations.
-}
unsafeMakeDefinedRandom :: (DefinedRandom a) => a -> a
unsafeMakeDefinedRandom = unsafePerformIO . makeDefinedRandom
{-# NOINLINE unsafeMakeDefinedRandom #-}

{- | A class for types that can be (partially) undefined whose undefined values
can be replaced with defined random values.
-}
class DefinedRandom a where
  makeDefinedRandom :: a -> IO a

instance DefinedRandom Bool where
  makeDefinedRandom b
    | hasUndefined b = randomIO
    | otherwise = pure b

instance DefinedRandom Bit where
  makeDefinedRandom b
    | hasUndefined b = Bit 0 <$> randomRIO (0, 1)
    | otherwise = pure b

instance (KnownNat n) => DefinedRandom (BitVector n) where
  makeDefinedRandom :: (KnownNat n) => BitVector n -> IO (BitVector n)
  makeDefinedRandom bv@(ensureSpine -> BV mask dat)
    | mask == 0 = pure bv
    | otherwise = do
        let maxVal = natToNum @(2 ^ n - 1)
        randomNat <- genNatural (0, maxVal)
        pure $ BV 0 (((maxVal `xor` mask) .&. dat) .|. (mask .&. randomNat))

-- | Generate a random natural number in the given inclusive range.
genNatural :: (Natural, Natural) -> IO Natural
genNatural = fmap fromInteger . randomRIO . bimap toInteger toInteger
