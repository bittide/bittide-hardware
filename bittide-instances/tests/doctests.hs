-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Main where

import Test.DocTest (mainFromCabal)
import System.Environment (getArgs)

main :: IO ()
main = do
  -- We use Nix to setup tooling, not to provide GHC packages so we need to set --no-nix
  args <- getArgs
  mainFromCabal "bittide-instances" ("--no-nix":args)
