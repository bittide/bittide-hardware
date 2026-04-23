-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
module Df.WbToDf where

import Bittide.Instances.Tests.WbToDf
import Clash.Explicit.Prelude
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Char
import Hedgehog
import Protocols
import Protocols.Hedgehog
import Protocols.Idle
import Protocols.MemoryMap
import Test.Tasty
import Test.Tasty.Hedgehog
import Test.Tasty.TH

-- | Test whether the wbToDf component correctly converts wishbone writes to Df stream transactions
prop_wb_to_df_test :: Property
prop_wb_to_df_test = property $ do
  dumpVcd <- liftIO getDumpVcd
  peConfig <- liftIO peConfigSim
  let
    impl :: Circuit (Df System ()) (Df System SomeAdt, Df System (BitVector 8))
    impl = idleSink |> (unMemmap $ dut dumpVcd peConfig)
  propWithModelT eOpts (pure []) model impl prop
 where
  eOpts =
    defExpectOptions
      { eoSampleMax = 1_000_000
      , eoStopAfterEmpty = Just 1_000 -- Increase when using UART
      , eoStallsMax = 1000
      , eoConsecutiveStalls = 10
      }

  model _ = (toList testValue, [])
  prop (expected, _) (actual, uart) = do
    footnote $ "Log: " <> fmap (chr . fromIntegral) uart
    actual === expected

tests :: TestTree
tests = $(testGroupGenerator)
