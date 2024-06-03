{-# LANGUAGE NumericUnderscores #-}
module Tests.Haxioms where

import Clash.Prelude
import Data.Constraint.Nat.Extra
import Hedgehog
import Test.Tasty
import Test.Tasty.Hedgehog

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

prop_leMult = property $ do
  a <- forAll $ Gen.int (Range.constant 1 1_000_000)
  b <- forAll $ Gen.int (Range.constant 1 1_000_000)
  assert (1 <= a * b)

tests = testGroup "Haxioms"
  [ testProperty "prop_leMult" prop_leMult
  ]
