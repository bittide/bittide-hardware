-- SPDX-FileCopyrightText: 2026 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Wishbone.DecodeLoop where

import Clash.Explicit.Prelude

import Control.Monad.IO.Class (liftIO)
import Data.List (isInfixOf)
import Data.String.Interpolate (i)
import Test.Tasty
import Test.Tasty.Hedgehog (testProperty)
import Test.Tasty.TH

import Bittide.Instances.Tests.DecodeLoop (simResultDecodeLoop)

import qualified Hedgehog as H

prop_decode_loop_test :: H.Property
prop_decode_loop_test =
  -- This simulation is _very_ slow, so we run it once at the physical
  -- link latency (~34 cycles one way).
  H.withTests 1 $ H.property $ do
    result <- liftIO $ simResultDecodeLoop (SNat @34)
    H.annotate [i|Result of decode_loop_test: \n#{result}|]
    H.assert ("TEST PASSED" `isInfixOf` result)

tests :: TestTree
tests = $(testGroupGenerator)
