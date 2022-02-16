{-# LANGUAGE NegativeLiterals #-}

module Tests.Contranomy.Core.ALU where

import Prelude
import Test.Tasty

import Clash.Class.BitPack (BitPack (BitSize, pack, unpack))
import Clash.Sized.Unsigned (Unsigned)
import Clash.Sized.Signed (Signed)

import Contranomy.Core.ALU (multdivSim)
import Contranomy.Instruction (MOp (DIVU, DIV, REM, REMU))

import qualified Hedgehog as H
import qualified Hedgehog.Range as Range
import qualified Hedgehog.Gen as Gen

import Hedgehog ((===))

import Test.Tasty.HUnit ((@?=))
import qualified Test.Tasty as T
import qualified Test.Tasty.Hedgehog as T
import qualified Test.Tasty.HUnit as T
import Data.Maybe (fromMaybe)

main :: IO ()
main = defaultMain tests

multdiv :: (BitPack a, BitSize a ~ 32) => a -> a -> MOp -> a
multdiv (pack -> a) (pack -> b) op = unpack (multdivSim a b op)

genLinearBounded :: (Integral a, Bounded a) => H.Gen a
genLinearBounded = Gen.integral Range.linearBounded

-- | Tests a CPU operation for the properties defined in the following table row:
--
-- Document: Volume I: RISC-V User-Level ISA V2.2
-- Section: 6.2 Division Operations
-- Table 6.1: Semantics for division by zero and division overflow.
-- Row: condition => Division by zero
--
prop_div_zero ::
  (Show a, Integral a, Bounded a, BitPack a, BitSize a ~ 32) =>
  -- | Instruction to test
  MOp ->
  -- | Expected outcome: if given 'Nothing', the test expects the generated
  -- value (`x`) to be returned.
  Maybe a ->
  H.Property
prop_div_zero op expected = H.property $ do
  x <- H.forAll genLinearBounded
  multdiv x 0 op === fromMaybe x expected

prop_div_zero_divu :: H.Property
prop_div_zero_divu = prop_div_zero @(Unsigned 32) DIVU (Just maxBound)

prop_div_zero_remu :: H.Property
prop_div_zero_remu = prop_div_zero @(Unsigned 32) REMU Nothing

prop_div_zero_div :: H.Property
prop_div_zero_div = prop_div_zero @(Signed 32) DIV (Just (-1))

prop_div_zero_rem :: H.Property
prop_div_zero_rem = prop_div_zero @(Signed 32) REM Nothing

test_overflow_div :: T.Assertion
test_overflow_div = multdiv @(Signed 32) minBound (-1) DIV @?= minBound

test_overflow_rem :: T.Assertion
test_overflow_rem = multdiv @(Signed 32) minBound (-1) REM @?= 0

tests :: TestTree
tests = T.testGroup "ALU"
  [ T.testGroup "Division by zero"
    [ T.testProperty "prop_div_zero_divu" prop_div_zero_divu
    , T.testProperty "prop_div_zero_remu" prop_div_zero_remu
    , T.testProperty "prop_div_zero_div" prop_div_zero_div
    , T.testProperty "prop_div_zero_rem" prop_div_zero_rem
    ]
  , T.testGroup "Overflow (signed only)"
    [ T.testCase "test_overflow_div" test_overflow_div
    , T.testCase "test_overflow_rem" test_overflow_rem
    ]
  ]
