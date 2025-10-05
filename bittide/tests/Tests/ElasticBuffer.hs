-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# OPTIONS_GHC -Wno-orphans #-}

module Tests.ElasticBuffer where

import Clash.Prelude

import Test.Tasty
import Test.Tasty.HUnit

import Bittide.ClockControl (RelDataCount, targetDataCount)
import Bittide.ElasticBuffer

import qualified Data.List as L

createDomain vXilinxSystem{vPeriod = hzToPeriod 200e6, vName = "Fast"}
createDomain vXilinxSystem{vPeriod = hzToPeriod 199e6, vName = "Slow"}

tests :: TestTree
tests =
  testGroup
    "Tests.ElasticBuffer"
    [ testGroup
        "xilinxElasticBuffer"
        [ testCase "case_xilinxElasticBufferMaxBound" case_xilinxElasticBufferMaxBound
        , testCase "case_xilinxElasticBufferMinBound" case_xilinxElasticBufferMinBound
        , testCase "case_xilinxElasticBufferEq" case_xilinxElasticBufferEq
        ]
    , testGroup
        "resettableXilinxElasticBuffer"
        [ testCase "case_resettableXilinxElasticBufferEq" case_resettableXilinxElasticBufferEq
        , testCase
            "case_resettableXilinxElasticBufferMaxBound"
            case_resettableXilinxElasticBufferMaxBound
        , testCase
            "case_resettableXilinxElasticBufferMinBound"
            case_resettableXilinxElasticBufferMinBound
        ]
    ]

{- | When the xilinxElasticBuffer is written to more quickly than it is being read from,
its data count should overflow.
-}
case_xilinxElasticBufferMaxBound :: Assertion
case_xilinxElasticBufferMaxBound = do
  let
    command = fromList $ L.replicate 60 (Just Fill) <> L.repeat Nothing
    wData = pure (0 :: Unsigned 8)
    underflows =
      sampleN
        1024
        ( (\(_, under, _, _, _) -> under)
            (xilinxElasticBuffer @6 (clockGen @Slow) (clockGen @Fast) resetGen command wData)
        )
    overflows =
      sampleN
        1024
        ( (\(_, _, over, _, _) -> over)
            (xilinxElasticBuffer @6 (clockGen @Slow) (clockGen @Fast) resetGen command wData)
        )

  assertBool "elastic buffer should overflow" (or overflows)
  assertBool "elastic buffer should not underflow" (not $ or underflows)

{- | When the xilinxElasticBuffer is read from more quickly than it is being written to,
its data count should underflow.
-}
case_xilinxElasticBufferMinBound :: Assertion
case_xilinxElasticBufferMinBound = do
  let
    command = fromList $ L.replicate 8 (Just Fill) <> L.repeat Nothing
    wData = pure (0 :: Unsigned 8)
    underflows =
      sampleN
        2048
        ( (\(_, under, _, _, _) -> under)
            (xilinxElasticBuffer @6 (clockGen @Fast) (clockGen @Slow) resetGen command wData)
        )
    overflows =
      sampleN
        2048
        ( (\(_, _, over, _, _) -> over)
            (xilinxElasticBuffer @6 (clockGen @Fast) (clockGen @Slow) resetGen command wData)
        )

  assertBool "elastic buffer should underflow" (or underflows)
  assertBool "elastic buffer should not overflow" (not $ or overflows)

{- | When the xilinxElasticBuffer is written to as quickly to as it is read from, it should
neither overflow nor underflow.
-}
case_xilinxElasticBufferEq :: Assertion
case_xilinxElasticBufferEq = do
  let
    command = fromList $ L.replicate 16 (Just Fill) <> L.repeat Nothing
    wData = pure (0 :: Unsigned 8)
    underflows =
      sampleN
        256
        ( (\(_, under, _, _, _) -> under)
            (xilinxElasticBuffer @5 (clockGen @Slow) (clockGen @Slow) resetGen command wData)
        )
    overflows =
      sampleN
        256
        ( (\(_, _, over, _, _) -> over)
            (xilinxElasticBuffer @5 (clockGen @Slow) (clockGen @Slow) resetGen command wData)
        )

  assertBool "elastic buffer should not underflow" (not $ or underflows)
  assertBool "elastic buffer should not overflow" (not $ or overflows)

{- | When the resettableXilinxElasticBuffer is written to as quickly as it is read from,
it eventually stabilizes.
-}
case_resettableXilinxElasticBufferEq :: Assertion
case_resettableXilinxElasticBufferEq = do
  let
    wData = pure (0 :: Unsigned 8)
    (dataCounts, underflows, overflows, ebStables, _) =
      L.unzip5
        . L.dropWhile (\(_, _, _, stable, _) -> not stable)
        $ sampleN
          1024
          ( bundle (resettableXilinxElasticBuffer @5 (clockGen @Slow) (clockGen @Slow) resetGen wData)
          )

  assertBool "elastic buffer should get out of its Fill state" ((> 0) $ L.length ebStables)
  assertBool "elastic buffer should not overflow after stabilizing" (not $ or overflows)
  assertBool "elastic buffer should not underflow after stabilizing" (not $ or underflows)
  assertBool
    "elastic buffer should remain stable after stabilizing once"
    (L.and ebStables)
  assertBool
    "elastic buffer datacount should be `targetDataCount` after stabilizing"
    (L.all ((== (targetDataCount :: RelDataCount 5))) dataCounts)

{- | When the xilinxElasticBuffer is written to more quickly than it is being read from,
its data count should overflow. Upon an overflow, the fifo is Drained and then filled
to half full, after which the cycle repeats.
-}
case_resettableXilinxElasticBufferMaxBound :: Assertion
case_resettableXilinxElasticBufferMaxBound = do
  let
    wData = pure (0 :: Unsigned 8)
    (_, underflows, overflows, _, _) =
      L.unzip5
        $ sampleN
          10000
          ( bundle (resettableXilinxElasticBuffer @5 (clockGen @Slow) (clockGen @Fast) resetGen wData)
          )

  -- After the fifo overflows, it should Drain the buffer, then fill it to half full and
  -- reset.
  assertBool
    "elastic buffer should reset after an overflow"
    ([True, False] `L.isInfixOf` overflows)
  assertBool
    "elastic buffer should not underflow when written to faster than read from"
    (not $ or underflows)

{- | When the xilinxElasticBuffer is read from more quickly than it is being written to,
its data count should overflow. Upon an overflow, the fifo is Drained and then filled
to half full, after which the cycle repeats.
-}
case_resettableXilinxElasticBufferMinBound :: Assertion
case_resettableXilinxElasticBufferMinBound = do
  let
    wData = pure (0 :: Unsigned 8)
    (_, underflows, overflows, _, _) =
      L.unzip5
        . L.filter (\(_, _, _, stable, _) -> stable)
        $ sampleN
          10000
          ( bundle (resettableXilinxElasticBuffer @5 (clockGen @Fast) (clockGen @Slow) resetGen wData)
          )

  -- After the fifo underflows, it should Drain for 1 cycle and then fill it to half
  -- full and reset.
  assertBool
    "elastic buffer should reset after an underflow"
    ([True, False] `L.isInfixOf` underflows)
  assertBool
    "elastic buffer should not overflow when read from faster than written to"
    (not $ or overflows)
