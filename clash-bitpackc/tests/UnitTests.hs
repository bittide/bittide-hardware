-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Main where

import Prelude

import Test.Tasty
import Test.Tasty.Hedgehog

import qualified Tests.BitPackC
import qualified Tests.Padding

tests :: TestTree
tests =
  testGroup
    "Unittests"
    [ Tests.BitPackC.tests
    , Tests.Padding.tests
    ]

{- | Default number of tests is 100, which is too low for our (complicated)
state machinery.
-}
setDefaultHedgehogTestLimit :: HedgehogTestLimit -> HedgehogTestLimit
setDefaultHedgehogTestLimit (HedgehogTestLimit Nothing) = HedgehogTestLimit (Just 1000)
setDefaultHedgehogTestLimit opt = opt

{- | Hedgehog seemingly gets stuck in an infinite loop when shrinking - probably
due to the way our generators depend on each other. We limit the number of
shrinks to 100.
-}
setDefaultHedgehogShrinkLimit :: HedgehogShrinkLimit -> HedgehogShrinkLimit
setDefaultHedgehogShrinkLimit (HedgehogShrinkLimit Nothing) = HedgehogShrinkLimit (Just 100)
setDefaultHedgehogShrinkLimit opt = opt

main :: IO ()
main =
  defaultMain $
    adjustOption setDefaultHedgehogTestLimit $
      adjustOption setDefaultHedgehogShrinkLimit $
        tests
