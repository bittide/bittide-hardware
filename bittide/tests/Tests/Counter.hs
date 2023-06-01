-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Tests.Counter where

import Clash.Explicit.Prelude
import qualified Prelude as P

import Control.Monad (forM_)

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.TH

import Bittide.Counter (domainDiffCounter)

createDomain vXilinxSystem{vName="D10", vPeriod=hzToPeriod 100e6}
createDomain vXilinxSystem{vName="D17", vPeriod=hzToPeriod 170e6}
createDomain vXilinxSystem{vName="D20", vPeriod=hzToPeriod 200e6}

noRst :: KnownDomain dom => Reset dom
noRst = unsafeFromHighPolarity (pure False)

rst :: KnownDomain dom => Reset dom
rst = unsafeFromHighPolarity (pure True)

rstN :: KnownDomain dom => Int -> Reset dom
rstN n = unsafeFromHighPolarity (fromList (P.replicate n True <> P.repeat False))

top ::
  forall src dst .
  ( KnownDomain src
  , KnownDomain dst
  ) =>
  Reset src ->
  Reset dst ->
  Signal dst (Signed 32)
top rstSrc rstDst = fst <$> domainDiffCounter clockGen rstSrc enableGen clockGen rstDst enableGen

-- | 'domainDiffCounter' should continuously emit zeros when applied to the same domain
case_zeroSameDomain :: Assertion
case_zeroSameDomain = sampleN 1000 (top @D10 @D10 noRst noRst) @?= P.replicate 1000 0

-- | 'domainDiffCounter' should continuously emit zeros when src reset is kept asserted
case_zeroSrcRst :: Assertion
case_zeroSrcRst = sampleN 1000 (top @D10 @D17 rst noRst) @?= P.replicate 1000 0

-- | 'domainDiffCounter' should continuously emit zeros when dst reset is kept asserted
case_zeroDstRst :: Assertion
case_zeroDstRst = sampleN 1000 (top @D10 @D17 noRst rst) @?= P.replicate 1000 0

-- | No matter when we release the destination reset, we should zeros followed by counting
case_glitchless :: Assertion
case_glitchless = --
  forM_ [0..512] $ \n -> do
    let
      sampled = sampleN 1000 (dut (rstN n))
      sampledNonZero = P.dropWhile (==0) sampled
      len = P.length sampledNonZero
    assertBool (">1 @ " <> show n) (len > 1)
    assertEqual ("exp @ " <> show n) sampledNonZero (P.take len expected)
 where
  dut = top @D10 @D20 noRst
  expected = P.concat [[n, n] | n <- [1..]]

tests :: TestTree
tests = $(testGroupGenerator)

-- Run with:
--
--    ghcid -c cabal repl bittide:unittests -T Tests.Counter.main
--
-- Add -W if you want to run tests in spite of warnings
--
main :: IO ()
main = defaultMain tests
