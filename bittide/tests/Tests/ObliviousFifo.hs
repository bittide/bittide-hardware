-- SPDX-FileCopyrightText: 2022-2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Tests.ObliviousFifo where

import Clash.Prelude
import Clash.Sized.Internal.BitVector
import Control.Monad (foldM)
import Data.Maybe (isJust)

import Test.Tasty
import Test.Tasty.HUnit

--import Bittide.ClockControl (DataCount, targetDataCount)
import Bittide.ObliviousFifo
import Bittide.ObliviousFifo.Frame

import qualified Data.List as L


-- the FIFO should preserve the following invariant:
-- readCount < (maxbound :: Unsigned 16) `implies` readCount == diffCount

createDomain vXilinxSystem{vPeriod=hzToPeriod 200e6, vName="Fast"}
createDomain vXilinxSystem{vPeriod=hzToPeriod 20e6, vName="Slow"}

tests :: TestTree
tests = testGroup "Tests.ObliviousFifo"
  [ testCase "case_ObliviousFifoFrameBitPack"
              case_ObliviousFifoFrameBitPack
  , testCase "case_xilinxObliviousFifoWriteFastReadSlow"
              case_xilinxObliviousFifoWriteFastReadSlow
  , testCase "case_xilinxObliviousFifoWriteSlowReadFast"
              case_xilinxObliviousFifoWriteSlowReadFast
--  , testCase "case_xilinxObliviousFifoEq"
--              case_xilinxObliviousFifoEq
  ]

-- | When the xilinxObliviousFifo is written to more quickly than it
-- is being read from, its data will lost after some time.
case_xilinxObliviousFifoWriteFastReadSlow :: Assertion
case_xilinxObliviousFifoWriteFastReadSlow = do
  let
    (counts, elements, frames, readys) =
        L.unzip4
      $ sampleN 100
      $ bundle
      $ xilinxObliviousFifo @16 @4 @Fast @Slow @(Unsigned 4)
          clockGen          enableGen
          clockGen resetGen enableGen
          (pure 0)

  assertBool "Oblivious FIFO did not get ready in time"
    $ or $ readys

  assertBool "Oblivious FIFO does not loose data"
    $ or $ isLostFrame <$> frames

  assertBool "Oblivious FIFO does not strictly grow in the virtual elements once it started"
    $ isJust
    $ foldM (\a e -> if e == 0 || e > a then Just e else Nothing)
        minBound counts

  assertBool "Oblivious FIFO does not grow in the number of real elements"
    $ isJust
    $ foldM (\a e -> if e >= a then Just e else Nothing)
        minBound elements

 where
  isLostFrame = \case
    Lost{} -> True
    _      -> False

-- | When the xilinxObliviousFifo is read from more quickly than it is being written to,
-- its data count should underflow.
case_xilinxObliviousFifoWriteSlowReadFast :: Assertion
case_xilinxObliviousFifoWriteSlowReadFast = do
  let
    (counts, elements, frames, readys) =
        L.unzip4
      $ sampleN 100
      $ bundle
      $ xilinxObliviousFifo @16 @4 @Slow @Fast @(Unsigned 4)
          clockGen          enableGen
          clockGen resetGen enableGen
          (pure 0)

  assertBool "Oblivious FIFO did not get ready in time"
    $ or $ readys

  assertBool "Oblivious FIFO does loose data"
    $ and $ L.zipWith (||) (isDataFrame <$> frames) (not <$> readys)

  assertBool "Oblivious FIFO does not strictly grow in the virtual elements once it started"
    $ isJust
    $ foldM (\a e -> if e == 0 || e > a then Just e else Nothing)
        minBound counts

  assertBool "Oblivious FIFO does not grow in the number of real elements"
    $ isJust
    $ foldM (\a e -> if e >= a then Just e else Nothing)
        minBound elements

 where
  isDataFrame = \case
    Data{} -> True
    _      -> False


-- | When the xilinxObliviousFifo is written to as quickly to as it is read from, it should
-- neither overflow nor underflow.
case_xilinxObliviousFifoEq :: Assertion
case_xilinxObliviousFifoEq = assertBool "" True {-do
  let
    ebMode = fromList $ L.replicate 32 Fill <> L.repeat Pass
    wData = pure (0 :: Unsigned 8)
    underflows =
      sampleN 256
        ((\(_, under, _, _)-> under) (xilinxObliviousFifo @5 (clockGen @Slow) (clockGen @Slow) resetGen ebMode wData))
    overflows =
      sampleN 256
        ((\(_, _, over, _)-> over) (xilinxObliviousFifo @5 (clockGen @Slow) (clockGen @Slow) resetGen ebMode wData))

  assertBool "elastic buffer should not underflow" (not $ or underflows)
  assertBool "elastic buffer should not overflow" (not $ or overflows)
-}

case_ObliviousFifoFrameBitPack :: Assertion
case_ObliviousFifoFrameBitPack = do
  assertBool "(pack (Data ())) /= (0b0 ++# pack ())"
    $ isLike# (pack (Data ()))    (0b0 ++# pack ())

  assertBool "(pack (Data True)) /= (0b0 ++# pack True)"
    $ isLike# (pack (Data True))    (0b0 ++# pack True)

  assertBool "(pack (Data False)) /= (0b0 ++# pack False)"
    $ isLike# (pack (Data False))    (0b0 ++# pack False)

  assertBool "(pack (Data (3 :: Unsigned 3))) /= (0b0 ++# pack (3 :: Unsigned 3))"
    $ isLike# (pack (Data (3 :: Unsigned 3)))    (0b0 ++# pack (3 :: Unsigned 3))

  assertBool "(pack (Data (0 :: Unsigned 3))) /= (0b0 ++# pack (0 :: Unsigned 3))"
    $ isLike# (pack (Data (0 :: Unsigned 3)))    (0b0 ++# pack (0 :: Unsigned 3))

  assertBool "(pack (Data (4 :: Unsigned 3))) /= (0b0 ++# pack (4 :: Unsigned 3))"
    $ isLike# (pack (Data (4 :: Unsigned 3)))    (0b0 ++# pack (4 :: Unsigned 3))

  assertBool "(pack (Data (Nothing :: Maybe Bool))) /= (0b0 ++# pack (Nothing :: Maybe Bool))"
    $ isLike# (pack (Data (Nothing :: Maybe Bool)))    (0b0 ++# pack (Nothing :: Maybe Bool))

  assertBool "(pack (Data (Just False:: Maybe Bool))) /= (0b0 ++# pack (Just False :: Maybe Bool))"
    $ isLike# (pack (Data (Just False:: Maybe Bool)))    (0b0 ++# pack (Just False :: Maybe Bool))
  assertBool "(pack (Data False)) /= (0b0 ++# pack False)"
    $ isLike# (pack (Data False))    (0b0 ++# pack False)

  assertBool "(pack (Empty :: Frame ())) /= 0b1"
    $ isLike# (pack (Empty :: Frame ()))    0b1

  assertBool "(pack (Empty :: Frame Bool)) /= 0b10"
    $ isLike# (pack (Empty :: Frame Bool))    0b10

  assertBool "(pack (Empty :: Frame (Unsigned 3))) /= 0b1000"
    $ isLike# (pack (Empty :: Frame (Unsigned 3)))    0b1000

  assertBool "(pack (Empty :: Frame (Maybe Bool))) /= 0b100"
    $ isLike# (pack (Empty :: Frame (Maybe Bool)))    0b100

  assertBool "(pack (Lost 1 :: Frame Bool)) /= 0b11)"
    $ isLike# (pack (Lost 1 :: Frame Bool))    0b11

  assertBool "(pack (Lost 3 :: Frame (Unsigned 3))) /= 0b1011)"
    $ isLike# (pack (Lost 3 :: Frame (Unsigned 3)))    0b1011

  assertBool "(pack (Lost 2 :: Frame (Maybe Bool))) /= 0b110)"
    $ isLike# (pack (Lost 2 :: Frame (Maybe Bool)))    0b110
