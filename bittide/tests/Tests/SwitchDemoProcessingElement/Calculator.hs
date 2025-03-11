-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}

module Tests.SwitchDemoProcessingElement.Calculator where

import Clash.Prelude hiding (indices)

import Bittide.SwitchDemoProcessingElement.Calculator (
  PeConfig (..),
  chainConfiguration,
  toCounterMap,
  toFpgaIndexed,
 )

import Test.Tasty (TestTree, defaultMain)
import Test.Tasty.HUnit (Assertion, testCase, (@?=))
import Test.Tasty.TH (testGroupGenerator)

{- FOURMOLU_DISABLE -} -- data / tabular format
parts :: Vec 3 (Vec 2 (Int, Int))
parts =
    ((1, 0) :> (3, 4) :> Nil)
 :> ((3, 1) :> (4, 6) :> Nil)
 :> ((5, 2) :> (5, 8) :> Nil)
 :> Nil

indices :: Vec 3 (Vec 2 Int)
indices =
    (0 :> 1 :> Nil)
 :> (2 :> 3 :> Nil)
 :> (4 :> 5 :> Nil)
 :> Nil

fpgaSetup :: Vec 3 (String, Vec 2 (Index 3))
fpgaSetup =
     ("A", 1 :> 2 :> Nil)
  :> ("B", 2 :> 0 :> Nil)
  :> ("C", 0 :> 1 :> Nil)
  :> Nil
{- FOURMOLU_ENABLE -}

case_toFpgaIndexed :: Assertion
case_toFpgaIndexed = toFpgaIndexed fpgaSetup indices @?= expected
 where
  expected = [(1, 0), (2, 1)] :> [(2, 2), (0, 3)] :> [(0, 4), (1, 5)] :> Nil

case_toCounterMap :: Assertion
case_toCounterMap = toCounterMap (toFpgaIndexed fpgaSetup parts) @?= expected
 where
  expected = [(1, 2), (2, 7)] :> [(0, 5), (2, 1)] :> [(0, 3), (1, 6)] :> Nil

case_chainConfiguration :: Assertion
case_chainConfiguration = chainConfiguration d3 d3 fpgaSetup parts 100 @?= expected
 where
  expected = peConfig0 :> peConfig1 :> peConfig2 :> Nil

  peConfig0 = PeConfig{startWriteAt = 116, writeForN = 1, startReadAt = 154, readForN = 3}
  peConfig1 = PeConfig{startWriteAt = 135, writeForN = 2, startReadAt = 118, readForN = 1}
  peConfig2 = PeConfig{startWriteAt = 151, writeForN = 3, startReadAt = 136, readForN = 2}

tests :: TestTree
tests = $(testGroupGenerator)

main :: IO ()
main = defaultMain tests
