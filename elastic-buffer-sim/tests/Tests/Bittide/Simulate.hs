-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Tests.Bittide.Simulate where

import Clash.Explicit.Prelude

import Test.Tasty
import Test.Tasty.HUnit

import Bittide.Simulate
import Bittide.Simulate.Ppm

createDomain vXilinxSystem{vPeriod=hzToPeriod 200e3, vName="Fast"}
createDomain vXilinxSystem{vPeriod=hzToPeriod 20e3, vName="Slow"}

tests :: TestTree
tests = testGroup "Simulate"
  [ testGroup "elasticBuffer"
    [ testCase "case_elasticBufferMaxBound" case_elasticBufferMaxBound
    , testCase "case_elasticBufferMinBound" case_elasticBufferMinBound
    , testCase "case_elasticBufferEq" case_elasticBufferEq
    , testCase "case_caseClockControlMaxBound" case_clockControlMaxBound
    , testCase "case_caseClockControlMinBound" case_clockControlMinBound
    ]
  ]

fastPeriod :: PeriodPs
fastPeriod = hzToPeriod 200e3

clockConfig :: Ppm -> ClockControlConfig
clockConfig clockUncertainty = ClockControlConfig
  { cccPessimisticPeriod = speedUpPeriod clockUncertainty fastPeriod
  , cccSettlePeriod      = fastPeriod * 200
  , cccDynamicRange      = clockUncertainty * 2
  , cccStepSize          = 10
  , cccBufferSize        = 128
  }

case_clockControlMaxBound :: Assertion
case_clockControlMaxBound = do
  let
    config = clockConfig (Ppm 100)
    dataCounts = pure (cccBufferSize config) :> Nil
    (change:_) = sample (clockControl @_ @Fast config dataCounts)

  change @?= SpeedUp

case_clockControlMinBound :: Assertion
case_clockControlMinBound = do
  let
    config = clockConfig (Ppm 100)
    dataCounts = pure 0 :> Nil
    (change:_) = sample (clockControl @_ @Fast config dataCounts)

  change @?= SlowDown

-- | When the elasticBuffer is written to more quickly than it is being read from,
-- its data count should reach 'maxBound'.
case_elasticBufferMaxBound :: Assertion
case_elasticBufferMaxBound = do
  let dataCounts = sampleN 1024 (elasticBuffer Saturate 32 (clockGen @Slow) (clockGen @Fast))
  -- it never hits exactly the maximum because the occupancy is in the read
  -- domain, i.e. we have to look for one less than the max
  assertBool "elastic buffer should reach its near maximum (read domain)" (31 `elem` dataCounts)
  assertBool "elastic buffer should not reach its minimum" (0 `notElem` dataCounts)

-- | When the elasticBuffer is written to more quickly than it is being read from,
-- its data count should reach 'maxBound'.
case_elasticBufferMinBound :: Assertion
case_elasticBufferMinBound = do
  let dataCounts = sampleN 1024 (elasticBuffer Saturate 32 (clockGen @Fast) (clockGen @Slow))
  assertBool "elastic buffer should reach its minimum" (0 `elem` dataCounts)
  assertBool "elastic buffer should not reach its maximum" (32 `notElem` dataCounts)

-- | When the elasticBuffer written to as quikly to as it is read from, it should
-- reach neiher its maxBound nor minBound.
case_elasticBufferEq :: Assertion
case_elasticBufferEq = do
  let dataCounts = sampleN 1024 (elasticBuffer Saturate 32 (clockGen @Slow) (clockGen @Slow))
  assertBool "elastic buffer should not reach its minimum" (0 `notElem` dataCounts)
  assertBool "elastic buffer should not reach its maximum" (32 `notElem` dataCounts)
