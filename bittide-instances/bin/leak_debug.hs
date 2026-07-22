-- SPDX-FileCopyrightText: 2026 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

-- | Runs the @Wishbone.RegisterWb@ simulation wrapped in a ghc-debug stub, so
-- a ghc-debug client can pause the process and take heap snapshots while the
-- simulation is running. Used for hunting space leaks; see
-- https://github.com/bittide/bittide-hardware/issues/784.
module Main where

import Prelude

import Bittide.Instances.Tests.RegisterWb (sim)
import GHC.Debug.Stub (withGhcDebug)

main :: IO ()
main = withGhcDebug sim
