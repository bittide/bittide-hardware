-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE OverloadedLists #-}

module Tests.SwitchDemoProcessingElement.Calculator where

import Clash.Prelude hiding (indices)

import Bittide.Calculator

import Test.Tasty (TestTree, defaultMain)
import Test.Tasty.HUnit (Assertion, testCase, (@?=))
import Test.Tasty.TH (testGroupGenerator)

type NumNodes = 3
type SdpeConfig = DefaultSdpeConfig NumNodes

{- FOURMOLU_DISABLE -} -- data / tabular format
parts :: Vec NumNodes (Vec (NumNodes - 1) (Int, Int))
parts =
    ((1, 0) :> (3, 4) :> Nil)
 :> ((3, 1) :> (4, 6) :> Nil)
 :> ((5, 2) :> (5, 8) :> Nil)
 :> Nil

indices :: Vec NumNodes (Vec (NumNodes - 1) Int)
indices =
    (0 :> 1 :> Nil)
 :> (2 :> 3 :> Nil)
 :> (4 :> 5 :> Nil)
 :> Nil

fpgaSetup :: Vec NumNodes (String, Vec (NumNodes - 1) (Index NumNodes))
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
case_chainConfiguration = config @?= expected
 where
  configMeta :: Vec NumNodes (DefaultSdpeMetaPeConfig Int NumNodes 3)
  configMeta = fullChainConfiguration defaultSdpeCalcConfig fpgaSetup parts 100
  config :: Vec NumNodes (CyclePeConfig Int (Index (NumNodes + 1)))
  config = metaPeConfigToCyclePeConfig (natToNum @(CalMetacycleLength SdpeConfig)) <$> configMeta

  expected = peConfig0 :> peConfig1 :> peConfig2 :> Nil

  peConfig0 = CyclePeConfig{startWriteAt = 116, writeForN = 1, startReadAt = 154, readForN = 3}
  peConfig1 = CyclePeConfig{startWriteAt = 135, writeForN = 2, startReadAt = 118, readForN = 1}
  peConfig2 = CyclePeConfig{startWriteAt = 151, writeForN = 3, startReadAt = 136, readForN = 2}

tests :: TestTree
tests = $(testGroupGenerator)

main :: IO ()
main = defaultMain tests
