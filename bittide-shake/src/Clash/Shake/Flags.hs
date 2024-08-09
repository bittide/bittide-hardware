{-# LANGUAGE DeriveAnyClass #-}
-- SPDX-FileCopyrightText: 2023 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE DeriveGeneric #-}

-- | Flags used by Shake
module Clash.Shake.Flags where

import Prelude

import Development.Shake.Classes
import GHC.Generics (Generic)
import System.Console.GetOpt (ArgDescr (NoArg, ReqArg), OptDescr (Option))
import Text.Read (readMaybe)

data Options = Options
  { hardwareTargets :: HardwareTargets
  , forceTestRerun :: Bool
  }

defaultOptions :: Options
defaultOptions =
  Options
    { hardwareTargets = OneAny
    , forceTestRerun = False
    }

-- | Number of hardware targets to program and optionally test
data HardwareTargets
  = -- | Program the first FPGA found by Vivado. This is not necessarily the first
    -- FPGA in the demo rack.
    OneAny
  | -- | Program the FPGAs in the demo rack at the specific indices. The actual
    -- IDs of the FPGAs in the demo rack are specified in @HardwareTest.tcl@.
    Specific [Int]
  | -- | Program all connected FPGAs. Note that we currently hardcode a list of all
    -- FPGAs in our possesion. If we can't find them all, the program will exit with
    -- and error code.
    All
  deriving (Read, Show, Eq, Typeable, Generic, Hashable, Binary, NFData)

{- | Parse string to 'HardwareTargets'. Return 'Left' if given string could not
be parsed.
-}
parseHardwareTargetsFlag :: String -> Either String (Options -> Options)
parseHardwareTargetsFlag s =
  case readMaybe s of
    Just f ->
      case f of
        Specific [] -> Left ("Specify at least one index from the demo rack, or use OneAny")
        _ -> Right (\opts -> opts{hardwareTargets = f})
    Nothing -> Left ("Not a valid hardware target: " ++ s)

{- | List of custom flags supported by us. Note that we currently support only
one flag, 'HardwareTargets'.
-}
customFlags :: [OptDescr (Either String (Options -> Options))]
customFlags =
  [ Option
      "" -- no short flags
      ["hardware-targets"] -- long name of flag
      (ReqArg parseHardwareTargetsFlag "TARGET")
      "Options: OneAny, Specific, All. See 'HardwareTargets' in 'Flags.hs'."
  , Option
      "" -- no short flags
      ["force-test-rerun"]
      (NoArg $ Right (\opts -> opts{forceTestRerun = True}))
      "Force the rerun of hardware in the loop tests"
  ]
