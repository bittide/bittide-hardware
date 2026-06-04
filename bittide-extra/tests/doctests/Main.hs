-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Main where

import System.Environment (getArgs)
import Test.DocTest (mainFromLibrary)
import Test.DocTest.Helpers (Library (..), extractCabalLibrary, findCabalPackage)

import qualified Distribution.ModuleName as ModuleName

main :: IO ()
main = do
  lib <- extractCabalLibrary =<< findCabalPackage "bittide-extra"
  -- 'System.Hardware.Serial' is a .hsc file; doctest-parallel resolves modules
  -- to .hs sources and can't find it (the hsc2hs output lives in dist-newstyle).
  -- It has no doctests anyway, so drop it.
  let lib' = lib{libModules = filter (/= serialModule) (libModules lib)}
      serialModule = ModuleName.fromString "System.Hardware.Serial"
  -- We use Nix to setup tooling, not to provide GHC packages so we need to set --no-nix
  args <- getArgs
  mainFromLibrary lib' ("--no-nix" : args)
