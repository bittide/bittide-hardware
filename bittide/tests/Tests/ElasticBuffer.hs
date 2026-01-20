-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# OPTIONS_GHC -Wno-orphans #-}

module Tests.ElasticBuffer where

import Clash.Prelude

import Test.Tasty
import Test.Tasty.HUnit

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
        ( (\(_, under, _, _, _, _, _) -> under)
            (xilinxElasticBuffer @6 (clockGen @Slow) (clockGen @Fast) command wData)
        )
    overflows =
      sampleN
        1024
        ( (\(_, _, over, _, _, _, _) -> over)
            (xilinxElasticBuffer @6 (clockGen @Slow) (clockGen @Fast) command wData)
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
        ( (\(_, under, _, _, _, _, _) -> under)
            (xilinxElasticBuffer @6 (clockGen @Fast) (clockGen @Slow) command wData)
        )
    overflows =
      sampleN
        2048
        ( (\(_, _, over, _, _, _, _) -> over)
            (xilinxElasticBuffer @6 (clockGen @Fast) (clockGen @Slow) command wData)
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
        ( (\(_, under, _, _, _, _, _) -> under)
            (xilinxElasticBuffer @5 (clockGen @Slow) (clockGen @Slow) command wData)
        )
    overflows =
      sampleN
        256
        ( (\(_, _, over, _, _, _, _) -> over)
            (xilinxElasticBuffer @5 (clockGen @Slow) (clockGen @Slow) command wData)
        )

  assertBool "elastic buffer should not underflow" (not $ or underflows)
  assertBool "elastic buffer should not overflow" (not $ or overflows)
