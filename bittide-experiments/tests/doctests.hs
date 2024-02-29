-- SPDX-FileCopyrightText: 2022-2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Main (main) where

import Test.DocTest (mainFromCabal)
import System.Environment (getArgs, lookupEnv, setEnv)

main :: IO ()
main = do
  -- Workaround for kernel bugs
  ghcRts0 <- lookupEnv "GHCRTS"
  let
    ghcRts1 = "-xp -xm20000000"
    ghcRts2 = maybe ghcRts1 (\s -> s <> " " <> ghcRts1) ghcRts0
  setEnv "GHCRTS" ghcRts2

  -- We use Nix to setup tooling, not to provide GHC packages so we need to set --no-nix
  args <- getArgs
  mainFromCabal "bittide-experiments" ("--no-nix":args)
