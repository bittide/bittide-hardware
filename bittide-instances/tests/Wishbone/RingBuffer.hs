-- SPDX-FileCopyrightText: 2026 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
-- Don't warn about orphan instances, caused by `createDomain`.
{-# OPTIONS_GHC -Wno-orphans #-}
-- Don't warn about partial functions: this is a test, so we'll see it fail.
{-# OPTIONS_GHC -Wno-x-partial #-}

module Wishbone.RingBuffer where

import Clash.Explicit.Prelude

import Control.Monad.IO.Class (liftIO)
import Data.List (isInfixOf)
import Data.Proxy (Proxy (..))
import Data.String.Interpolate (i)
import Test.Tasty
import Test.Tasty.Hedgehog (testProperty)
import Test.Tasty.TH

import Bittide.Instances.Tests.RingBuffer (
  simResultRingBuffer,
 )

import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

prop_ring_buffer_test :: H.Property
prop_ring_buffer_test =
  -- This test is _very_ slow, so we only run it once.
  H.withTests 1 $ H.property $ do
    latency <- H.forAll $ Gen.integral (Range.constant 0 100)
    liftIO $ putStrLn $ "Testing ring_buffer_test with latency " <> show latency <> " cycles"
    result <- liftIO
      $ case someNatVal (fromInteger latency) of
        Just (SomeNat (_ :: Proxy n)) -> simResultRingBuffer (SNat @n)
        Nothing -> error [i|Invalid latency value: #{latency}|]
    H.annotate [i|Result of ring_buffer_test with latency #{latency} cycles: \n#{result}|]
    H.assert ("TEST PASSED" `isInfixOf` result)

tests :: TestTree
tests = $(testGroupGenerator)
