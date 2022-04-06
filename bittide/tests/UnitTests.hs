-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE ImplicitPrelude #-}

module Main where

import Test.Tasty
import Test.Tasty.Hedgehog

import Tests.Calendar
import Tests.DoubleBufferedRam
import Tests.ScatterGather
import Tests.Switch
import Tests.ElasticBuffer

tests :: TestTree
tests = testGroup "Unittests"
  [calGroup, sgGroup, ebGroup, switchGroup, ramGroup]

setDefaultHedgehogTestLimit :: HedgehogTestLimit -> HedgehogTestLimit
setDefaultHedgehogTestLimit (HedgehogTestLimit Nothing) = HedgehogTestLimit (Just 10000)
setDefaultHedgehogTestLimit opt = opt

main :: IO ()
main = defaultMain $
  adjustOption setDefaultHedgehogTestLimit
  tests
