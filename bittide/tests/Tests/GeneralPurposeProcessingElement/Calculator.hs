-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE OverloadedLists #-}

module Tests.GeneralPurposeProcessingElement.Calculator where

import Clash.Prelude

import Bittide.Calculator (
  DefaultGppeConfig,
  DefaultGppeMetaPeConfig,
  MetaPeConfig (..),
  defaultGppeCalcConfig,
  fullChainConfiguration,
 )

import Test.Tasty (TestTree, defaultMain)
import Test.Tasty.HUnit (Assertion, testCase, (@?=))
import Test.Tasty.TH (testGroupGenerator)

import qualified Bittide.Calculator as Calc

{- FOURMOLU_DISABLE -} -- data / tabular format
parts :: Vec NumNodes (Vec (NumNodes - 1) (Int, Int))
parts =
    ((1, 0) :> (3, 4) :> Nil)
 :> ((3, 1) :> (4, 6) :> Nil)
 :> ((5, 2) :> (5, 8) :> Nil)
 :> Nil

fpgaSetup :: Vec NumNodes (String, Vec (NumNodes - 1) (Index NumNodes))
fpgaSetup =
     ("A", 1 :> 2 :> Nil)
  :> ("B", 2 :> 0 :> Nil)
  :> ("C", 0 :> 1 :> Nil)
  :> Nil
{- FOURMOLU_ENABLE -}

type NumNodes = 3
type WindowCycles = Calc.WindowCycles NumNodes 3
type Padding = WindowCycles * 5
type GppeConfig = DefaultGppeConfig NumNodes Padding

gppeConfig :: GppeConfig
gppeConfig = defaultGppeCalcConfig

case_chainConfiguration :: Assertion
case_chainConfiguration = result @?= expected
 where
  result ::
    Vec NumNodes (DefaultGppeMetaPeConfig Int NumNodes 3 Padding)
  result = fullChainConfiguration gppeConfig fpgaSetup parts 100
  expected = peConfig0 :> peConfig1 :> peConfig2 :> Nil

  peConfig0 =
    MetaPeConfig
      { writeMetacycle = 1
      , writeOffset = 8
      , writeForN = 1
      , readMetacycle = 3
      , readOffset = 10
      , readForN = 3
      }
  peConfig1 =
    MetaPeConfig
      { writeMetacycle = 2
      , writeOffset = 9
      , writeForN = 2
      , readMetacycle = 1
      , readOffset = 10
      , readForN = 1
      }
  peConfig2 =
    MetaPeConfig
      { writeMetacycle = 3
      , writeOffset = 7
      , writeForN = 3
      , readMetacycle = 2
      , readOffset = 10
      , readForN = 2
      }

tests :: TestTree
tests = $(testGroupGenerator)

main :: IO ()
main = defaultMain tests
