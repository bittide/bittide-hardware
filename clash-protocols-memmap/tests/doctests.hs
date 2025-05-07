-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Main where

import System.Environment (getArgs)
import Test.DocTest (mainFromCabal)

main :: IO ()
main = do
  -- We use Nix to setup tooling, not to provide GHC packages so we need to set --no-nix
  args <- getArgs
  mainFromCabal "clash-protocols-memmap" ("--no-nix" : args)
