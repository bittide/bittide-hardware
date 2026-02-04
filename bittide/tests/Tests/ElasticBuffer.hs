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
    command = fromList $ L.replicate 60 (Just 1) <> L.repeat Nothing
    wData = pure (0 :: Unsigned 8)
    underflows =
      sampleN
        2048
        ( (\(_, under, _, _, _) -> under)
            (xilinxElasticBuffer @6 (clockGen @Slow) (clockGen @Fast) command wData)
        )
    overflows =
      sampleN
        16192
        ( (\(_, _, over, _, _) -> over)
            (xilinxElasticBuffer @6 (clockGen @Slow) (clockGen @Fast) command wData)
        )

    -- Ignore the first 32 samples to allow the buffer to fill up
    underflowsTail = L.drop 32 underflows
    overflowsTail = L.drop 32 overflows

  assertBool
    ("elastic buffer should not underflow: " <> show underflowsTail)
    (not $ or underflowsTail)
  assertBool ("elastic buffer should overflow: " <> show overflowsTail) (or overflowsTail)

{- | When the xilinxElasticBuffer is read from more quickly than it is being written to,
its data count should underflow.
-}
case_xilinxElasticBufferMinBound :: Assertion
case_xilinxElasticBufferMinBound = do
  let
    command = fromList $ L.replicate 8 (Just 1) <> L.repeat Nothing
    wData = pure (0 :: Unsigned 8)
    underflows =
      sampleN
        2048
        ( (\(_, under, _, _, _) -> under)
            (xilinxElasticBuffer @6 (clockGen @Fast) (clockGen @Slow) command wData)
        )
    overflows =
      sampleN
        2048
        ( (\(_, _, over, _, _) -> over)
            (xilinxElasticBuffer @6 (clockGen @Fast) (clockGen @Slow) command wData)
        )

    -- Ignore the first 32 samples to allow the buffer to fill up
    underflowsTail = L.drop 32 underflows
    overflowsTail = L.drop 32 overflows

  assertBool ("elastic buffer should underflow: " <> show underflowsTail) (or underflowsTail)
  assertBool ("elastic buffer should not overflow: " <> show overflowsTail) (not $ or overflowsTail)

{- | When the xilinxElasticBuffer is written to as quickly to as it is read from, it should
neither overflow nor underflow.
-}
case_xilinxElasticBufferEq :: Assertion
case_xilinxElasticBufferEq = do
  let
    command = fromList $ L.replicate 16 (Just 1) <> L.repeat Nothing
    wData = pure (0 :: Unsigned 8)
    underflows =
      sampleN
        256
        ( (\(_, under, _, _, _) -> under)
            (xilinxElasticBuffer @5 (clockGen @Slow) (clockGen @Slow) command wData)
        )
    overflows =
      sampleN
        256
        ( (\(_, _, over, _, _) -> over)
            (xilinxElasticBuffer @5 (clockGen @Slow) (clockGen @Slow) command wData)
        )

    -- Ignore the first 32 samples to allow the buffer to fill up
    underflowsTail = L.drop 32 underflows
    overflowsTail = L.drop 32 overflows

  assertBool
    ("elastic buffer should not underflow: " <> show underflowsTail)
    (not $ or underflowsTail)
  assertBool ("elastic buffer should not overflow: " <> show overflowsTail) (not $ or overflowsTail)
