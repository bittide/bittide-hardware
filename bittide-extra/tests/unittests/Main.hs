-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Main where

import Prelude

import Test.Tasty

import Test.Tasty.Hedgehog (HedgehogTestLimit (..))

import qualified Tests.Clash.Cores.Xilinx.Xpm.Cdc.Extra
import qualified Tests.Numeric.Extra
import qualified Tests.Protocols.Df.Extra

setDefaultHedgehogTestLimit :: HedgehogTestLimit -> HedgehogTestLimit
setDefaultHedgehogTestLimit (HedgehogTestLimit Nothing) = HedgehogTestLimit (Just 1000)
setDefaultHedgehogTestLimit opt = opt

tests :: TestTree
tests =
  testGroup
    "tests"
    [ Tests.Numeric.Extra.tests
    , Tests.Clash.Cores.Xilinx.Xpm.Cdc.Extra.tests
    , Tests.Protocols.Df.Extra.tests
    ]

main :: IO ()
main =
  defaultMain $
    adjustOption
      setDefaultHedgehogTestLimit
      tests
