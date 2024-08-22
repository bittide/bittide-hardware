-- SPDX-FileCopyrightText: 2023-2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

-- | Flags used by Shake
module Clash.Shake.Flags where

import Prelude

import System.Console.GetOpt (ArgDescr (NoArg), OptDescr (Option))

data Options = Options
  { forceTestRerun :: Bool
  }

defaultOptions :: Options
defaultOptions =
  Options
    { forceTestRerun = False
    }

{- | List of custom flags supported by us. Note that we currently support only
one flag, 'HardwareTargets'.
-}
customFlags :: [OptDescr (Either String (Options -> Options))]
customFlags =
  [ Option
      "" -- no short flags
      ["force-test-rerun"]
      (NoArg $ Right (\opts -> opts{forceTestRerun = True}))
      "Force the rerun of hardware in the loop tests"
  ]
