-- SPDX-FileCopyrightText: 2023 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

-- | Flags used by Shake
module Clash.Shake.Flags where

import Prelude

import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)
import System.Console.GetOpt (OptDescr(Option), ArgDescr(ReqArg))

-- | Number of hardware targets to program and optionally test
data HardwareTargets
  -- | Program the first FPGA found by Vivado. This is not necessarily the first
  -- FPGA in the demo rack.
  = OneAny
  -- | Program the FPGAs in the demo rack at the specific indices. The actual
  -- IDs of the FPGAs in the demo rack are specified in @HardwareTest.tcl@.
  | Specific [Int]
  -- | Program all connected FPGAs. Note that we currently hardcode a list of all
  -- FPGAs in our possesion. If we can't find them all, the program will exit with
  -- and error code.
  | All
  deriving (Read)

-- | Like 'last', but returns 'Nothing' if given list is empty
lastMaybe :: [a] -> Maybe a
lastMaybe [] = Nothing
lastMaybe (x:xs) = Just $ last (x:xs)

-- | Parse string to 'HardwareTargets'. Return 'Left' if given string could not
-- be parsed.
parseHardwareTargetsFlag :: String -> Either String HardwareTargets
parseHardwareTargetsFlag s =
  case readMaybe s of
    Just f ->
      case f of
        Specific [] -> Left ("Specify at least one index from the demo rack, or use OneAny")
        _ -> Right f
    Nothing -> Left ("Not a valid hardware target: " ++ s)

-- | Get hardware flag based on a list of parsed flags. Defaults to 'OneAny'. If
-- multiple options are given, the last one is picked.
getHardwareTargetsFlag :: [HardwareTargets] -> HardwareTargets
getHardwareTargetsFlag = fromMaybe OneAny . lastMaybe

-- | List of custom flags supported by us. Note that we currently support only
-- one flag, 'HardwareTargets'.
customFlags :: [OptDescr (Either String HardwareTargets)]
customFlags =
  [ Option
      "" -- no short flags
      ["hardware-targets"] -- long name of flag
      (ReqArg parseHardwareTargetsFlag "TARGET")
      "Options: OneAny, Specific, All. See 'HardwareTargets' in 'Flags.hs'."
  ]
