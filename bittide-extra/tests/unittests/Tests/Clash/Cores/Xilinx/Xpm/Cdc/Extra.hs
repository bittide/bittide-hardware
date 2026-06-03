-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# OPTIONS_GHC -Wno-orphans #-}

module Tests.Clash.Cores.Xilinx.Xpm.Cdc.Extra where

import Clash.Explicit.Prelude

import Hedgehog (Gen, Property)
import Hedgehog.Gen.Extra (genSmallInt)
import Protocols.Experimental.Hedgehog (ExpectOptions (..), defExpectOptions, idWithModel)
import Test.Tasty (TestTree, defaultMain)
import Test.Tasty.Hedgehog (testProperty)
import Test.Tasty.TH (testGroupGenerator)

import Clash.Cores.Xilinx.Xpm.Cdc.Extra (xpmCdcHandshakeDf)

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

createDomain vSystem{vName = "Slow", vPeriod = 10000}
createDomain vSystem{vName = "Fast", vPeriod = 1000}

genData :: Gen a -> Gen [a]
genData genA = do
  n <- genSmallInt
  Gen.list (Range.singleton n) genA

-- | Number of reset cycles to use in these tests
nResetCycles :: SNat 30
nResetCycles = d30

{- | Custom expect options for these tests. The values are picked such that the
stalls generated have more than enough time to propagate through the CDC and
back. The 'eoStopAfterEmpty' is picked experimentally -- if the test fails it
/probably/ means that it should be increased.
-}
xpmExpectOptions :: ExpectOptions
xpmExpectOptions =
  defExpectOptions
    { eoConsecutiveStalls = 100
    , eoStopAfterEmpty = Just 2000
    , eoResetCycles = snatToNum nResetCycles
    }

prop_xpmCdcHandshakeDf_same_domain :: Property
prop_xpmCdcHandshakeDf_same_domain =
  idWithModel
    xpmExpectOptions
    (genData genSmallInt)
    id
    (xpmCdcHandshakeDf clk rst clk rst)
 where
  clk = clockGen @Fast
  rst = resetGenN @Fast nResetCycles

prop_xpmCdcHandshakeDf_slow_fast :: Property
prop_xpmCdcHandshakeDf_slow_fast =
  idWithModel
    xpmExpectOptions
    (genData genSmallInt)
    id
    (xpmCdcHandshakeDf clkSrc rstSrc clkDst rstDst)
 where
  clkSrc = clockGen @Slow
  rstSrc = resetGenN @Slow nResetCycles
  clkDst = clockGen @Fast
  rstDst = resetGenN @Fast nResetCycles

prop_xpmCdcHandshakeDf_fast_slow :: Property
prop_xpmCdcHandshakeDf_fast_slow =
  idWithModel
    xpmExpectOptions
    (genData genSmallInt)
    id
    (xpmCdcHandshakeDf clkSrc rstSrc clkDst rstDst)
 where
  clkSrc = clockGen @Fast
  rstSrc = resetGenN @Fast nResetCycles
  clkDst = clockGen @Slow
  rstDst = resetGenN @Slow nResetCycles

tests :: TestTree
tests = $(testGroupGenerator)

main :: IO ()
main = defaultMain tests
