-- SPDX-FileCopyrightText: 2026 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
module Tests.Protocols.Df.Extra where

import Clash.Prelude

import Hedgehog (Gen, Property, Range)
import Protocols
import Protocols.Hedgehog (defExpectOptions, idWithModelSingleDomain)
import Test.Tasty (TestTree)
import Test.Tasty.Hedgehog (testProperty)
import Test.Tasty.TH (testGroupGenerator)

import Protocols.Df.Extra (skid)

import qualified Clash.Prelude as C
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

smallInt :: Range Int
smallInt = Range.linear 0 10

genSmallInt :: Gen Int
genSmallInt =
  Gen.frequency
    [ (90, Gen.integral smallInt)
    , (10, Gen.constant (Range.lowerBound 99 smallInt))
    ]

genData :: Gen a -> Gen [a]
genData genA = do
  n <- genSmallInt
  Gen.list (Range.singleton n) genA

-- | Wrapper around 'skid' that discards the Ready signal
skidDropReady ::
  forall dom a.
  (NFDataX a, HiddenClockResetEnable dom) =>
  Circuit (Df dom a) (Df dom a)
skidDropReady = circuit $ \dfIn -> do
  (dfOut, _ready) <- skid -< dfIn
  idC -< dfOut

prop_skid :: Property
prop_skid =
  idWithModelSingleDomain
    @C.System
    defExpectOptions
    (genData genSmallInt)
    (C.exposeClockResetEnable id)
    (C.exposeClockResetEnable skidDropReady)

tests :: TestTree
tests = $(testGroupGenerator)
