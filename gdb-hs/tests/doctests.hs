-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Main where

import Prelude

import System.Environment (getArgs)
import Test.DocTest (mainFromCabal)

main :: IO ()
main = do
  args <- getArgs
  mainFromCabal "gdb-hs" ("--no-nix" : args)
