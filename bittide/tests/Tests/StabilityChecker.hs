-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE OverloadedStrings #-}

module Tests.StabilityChecker where

import Clash.Prelude hiding (someNatVal, (^))
import Prelude ((^))

import Clash.Hedgehog.Sized.Signed (genSigned)
import Hedgehog
import Test.Tasty
import Test.Tasty.Hedgehog

import Bittide.ClockControl
import Bittide.ClockControl.StabilityChecker
import Tests.Shared

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

tests :: TestTree
tests =
  testGroup
    "Tests.StabilityChecker"
    [ testPropertyNamed
        "stabilityCheckerTest behaves the same as its golden reference"
        "stabilityCheckerTest"
        stabilityCheckerTest
    ]

stabilityCheckerTest :: Property
stabilityCheckerTest = property $ do
  dataCountBits <- forAll $ Gen.integral $ Range.linear 2 128
  cyclesStable <- forAll $ Gen.integral $ Range.linear 1 128
  margin <- forAll $ Gen.integral $ Range.linear 0 (2 ^ dataCountBits - 1)

  -- Convert generated variables to type level ones
  case ( someSNat dataCountBits
       , someSNat (cyclesStable - 1)
       , someSNat margin
       ) of
    (SomeSNat sDataCountBits, SomeSNat sCyclesStable, SomeSNat sMargin) ->
      prop sDataCountBits (succSNat sCyclesStable) sMargin
 where
  -- Property locked to arbitrary type level values
  prop ::
    forall cyclesStable dataCountBits margin.
    (1 <= cyclesStable) =>
    SNat dataCountBits ->
    SNat cyclesStable ->
    SNat margin ->
    PropertyT IO ()
  prop SNat sCyclesStable@SNat sMargin@SNat = do
    simLength <- forAll $ Gen.integral (Range.linear 4 1024)
    dataCounts <-
      forAll
        $ Gen.list
          (Range.singleton simLength)
          (genSigned @_ @dataCountBits Range.constantBounded)
    let
      topEntity = wcre $ fmap stable . stabilityChecker @System sMargin sCyclesStable
      simOut = simulateN simLength topEntity dataCounts

    simOut === golden (snatToNum sMargin) (snatToNum sCyclesStable) dataCounts

  -- 'stabilityChecker' reference design
  golden :: forall n. (KnownNat n) => Integer -> Integer -> [RelDataCount n] -> [Bool]
  golden margin cyclesStable dataCounts =
    f
      (0, fromIntegral (targetDataCount :: RelDataCount n))
      (fmap fromIntegral dataCounts)
   where
    f _ [] = []
    f (!cnt, target) (x : xs)
      | inMargin && cnt >= cyclesStable = True : f (cnt + 1, target) xs
      | inMargin = False : f (cnt + 1, target) xs
      | otherwise = False : f (0, x) xs
     where
      inMargin = abs (x - target) <= margin
