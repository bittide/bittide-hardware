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
createDomain vXilinxSystem{vPeriod = hzToPeriod 20e6, vName = "Slow"}

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
    ebMode = fromList $ L.replicate 3 Fill <> L.repeat Pass
    wData = pure (0 :: Unsigned 8)
    underflows =
      sampleN
        256
        ( (\(_, under, _, _) -> under)
            (xilinxElasticBuffer @6 (clockGen @Slow) (clockGen @Fast) resetGen ebMode wData)
        )
    overflows =
      sampleN
        256
        ( (\(_, _, over, _) -> over)
            (xilinxElasticBuffer @6 (clockGen @Slow) (clockGen @Fast) resetGen ebMode wData)
        )

  assertBool "elastic buffer should overflow" (or overflows)
  assertBool "elastic buffer should not underflow" (not $ or underflows)

{- | When the xilinxElasticBuffer is read from more quickly than it is being written to,
its data count should underflow.
-}
case_xilinxElasticBufferMinBound :: Assertion
case_xilinxElasticBufferMinBound = do
  let
    ebMode = fromList $ L.replicate 32 Fill <> L.repeat Pass
    wData = pure (0 :: Unsigned 8)
    underflows =
      sampleN
        256
        ( (\(_, under, _, _) -> under)
            (xilinxElasticBuffer @6 (clockGen @Fast) (clockGen @Slow) resetGen ebMode wData)
        )
    overflows =
      sampleN
        256
        ( (\(_, _, over, _) -> over)
            (xilinxElasticBuffer @6 (clockGen @Fast) (clockGen @Slow) resetGen ebMode wData)
        )

  assertBool "elastic buffer should underflow" (or underflows)
  assertBool "elastic buffer should not overflow" (not $ or overflows)

{- | When the xilinxElasticBuffer is written to as quickly to as it is read from, it should
neither overflow nor underflow.
-}
case_xilinxElasticBufferEq :: Assertion
case_xilinxElasticBufferEq = do
  let
    ebMode = fromList $ L.replicate 32 Fill <> L.repeat Pass
    wData = pure (0 :: Unsigned 8)
    underflows =
      sampleN
        256
        ( (\(_, under, _, _) -> under)
            (xilinxElasticBuffer @5 (clockGen @Slow) (clockGen @Slow) resetGen ebMode wData)
        )
    overflows =
      sampleN
        256
        ( (\(_, _, over, _) -> over)
            (xilinxElasticBuffer @5 (clockGen @Slow) (clockGen @Slow) resetGen ebMode wData)
        )

  assertBool "elastic buffer should not underflow" (not $ or underflows)
  assertBool "elastic buffer should not overflow" (not $ or overflows)

{- | When the resettableXilinxElasticBuffer is written to as quickly as it is read from, it eventually
 stabalises.
-}
case_resettableXilinxElasticBufferEq :: Assertion
case_resettableXilinxElasticBufferEq = do
  let
    wData = pure (0 :: Unsigned 8)
    (dataCounts, underflows, overflows, ebModes, _) =
      L.unzip5
        . L.dropWhile (\(_, _, _, eb, _) -> eb == Drain || eb == Fill)
        $ sampleN
          256
          ( bundle (resettableXilinxElasticBuffer @5 (clockGen @Slow) (clockGen @Slow) resetGen wData)
          )
    dataCountBounds =
      L.all
        ((< 3) . abs . subtract (toInteger (targetDataCount :: RelDataCount 5)) . toInteger)
        dataCounts

  assertBool "elastic buffer should get out of its Fill state" ((> 0) $ L.length ebModes)
  assertBool "elastic buffer should not overflow after stabalising" (not $ or overflows)
  assertBool "elastic buffer should not underflow after stabalising" (not $ or underflows)
  assertBool
    "elastic buffer should be in Pass mode after stabalising"
    (L.all (== Pass) ebModes)
  assertBool
    "elastic buffer datacount should be `targetDataCount` (margin 3 elements) after stabalising"
    dataCountBounds

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
        . L.filter (\(_, _, _, eb, _) -> eb == Pass)
        $ sampleN
          256
          ( bundle (resettableXilinxElasticBuffer @5 (clockGen @Slow) (clockGen @Fast) resetGen wData)
          )

  -- After the fifo overflows, it should Drain the buffer, then fill it to half full and
  -- reset.
  assertBool
    "elastic buffer should reset after an overflow"
    ([True, False] `L.isInfixOf` overflows)
  -- Since the overflows list is filtered on eb==Pass, the Drain and Fill operations are
  -- left out. Therefore, the fifo should not return the overflow signal twice in a row.
  assertBool
    "elastic buffer should not overflow twice in a row"
    (not $ L.isInfixOf [True, True] overflows)
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
        . L.filter (\(_, _, _, eb, _) -> eb == Pass)
        $ sampleN
          512
          ( bundle (resettableXilinxElasticBuffer @5 (clockGen @Fast) (clockGen @Slow) resetGen wData)
          )

  -- After the fifo underflows, it should Drain for 1 cycle and then fill it to half
  -- full and reset.
  assertBool
    "elastic buffer should reset after an underflow"
    ([True, False] `L.isInfixOf` underflows)
  -- Since the underflows list is filtered on eb==Pass, the Drain and Fill operations are
  -- left out. Therefore, the fifo should not return the underflow signal twice in a row.
  assertBool
    "elastic buffer should not underflow twice in a row"
    (not $ L.isInfixOf [True, True] underflows)
  assertBool
    "elastic buffer should not overflow when read from faster than written to"
    (not $ or overflows)
