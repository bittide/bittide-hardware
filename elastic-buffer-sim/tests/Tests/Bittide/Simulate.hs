-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Tests.Bittide.Simulate where

import Clash.Explicit.Prelude

import Test.Tasty
import Test.Tasty.HUnit
import Data.Maybe

import Bittide.ClockControl
import Bittide.ClockControl.Callisto

createDomain vXilinxSystem{vPeriod=hzToPeriod 200e6, vName="Fast"}
createDomain vXilinxSystem{vPeriod=hzToPeriod 20e6, vName="Slow"}

tests :: TestTree
tests = testGroup "Simulate"
  [ testGroup "elasticBuffer"
    [ testCase "case_caseClockControlMaxBound" case_clockControlMaxBound
    , testCase "case_caseClockControlMinBound" case_clockControlMinBound
    ]
  ]

case_clockControlMaxBound :: Assertion
case_clockControlMaxBound = do
  let
    config = defClockConfig
    dataCounts = pure maxBound :> Nil
    mask = pure $ pack (repeat high)
    changes =
      fmap (fromMaybe NoChange . maybeSpeedChange)  $ sampleN
        -- +10_000 assumes callisto's pipeline less than 10_000 deep
        (fromIntegral (cccPessimisticSettleCycles config + 10_000))
        (callistoClockControl @_ @_ @Fast clockGen resetGen enableGen config mask dataCounts)

  assertBool
    "only requests speed up"
    (SpeedUp `elem` changes && SlowDown `notElem` changes)

case_clockControlMinBound :: Assertion
case_clockControlMinBound = do
  let
    config = defClockConfig
    dataCounts = pure minBound :> Nil
    mask = pure $ pack (repeat high)
    changes =
      fmap (fromMaybe NoChange . maybeSpeedChange) $ sampleN
        -- +100 assumes callisto's pipeline less than 100 deep
        (fromIntegral (cccPessimisticSettleCycles config + 100))
        (callistoClockControl @_ @_ @Fast clockGen resetGen enableGen config mask dataCounts)

  assertBool
    "only requests slow down"
    (SlowDown `elem` changes && SpeedUp `notElem` changes)
