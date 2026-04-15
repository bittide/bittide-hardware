-- SPDX-FileCopyrightText: 2026 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
-- Don't warn about orphan instances, caused by `createDomain`.
{-# OPTIONS_GHC -Wno-orphans #-}
-- Don't warn about partial functions: this is a test, so we'll see it fail.
{-# OPTIONS_GHC -Wno-x-partial #-}

module Wishbone.Ringbuffer where

import Clash.Explicit.Prelude

import Control.Monad.IO.Class (liftIO)
import Data.List (isInfixOf)
import Data.Proxy (Proxy (..))
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog (testProperty)
import Test.Tasty.TH

import Bittide.Instances.Tests.Ringbuffer (simResultRingbuffer)

prop_ringbuffer_test :: H.Property
prop_ringbuffer_test = H.withTests 1 $ H.property $ do
  latency <- H.forAll $ Gen.integral (Range.constant 0 100)
  liftIO $ putStrLn $ "Testing ringbuffer_test with latency " <> show latency <> " cycles"
  let result = case someNatVal (fromInteger latency) of
        Just (SomeNat (_ :: Proxy n)) -> simResultRingbuffer (SNat @n)
        Nothing -> error $ "Invalid latency value: " <> show latency
  H.annotate
    $ "Running ringbuffer_test with latency "
    <> show latency
    <> " cycles\nReceived the following from the CPU over UART:\n"
    <> result
  H.assert ("TEST PASSED" `isInfixOf` result)

tests :: TestTree
tests = $(testGroupGenerator)
