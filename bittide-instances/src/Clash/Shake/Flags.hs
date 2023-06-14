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
  -- | Program first FPGA found by Vivado
  = FirstOfAny
  -- | Program the first FPGA from a list of hardcoded IDs (FGPA IDs), see
  -- @HardwareTest.tcl@
  | FirstOfKnown
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
    Just f -> Right f
    Nothing -> Left ("Not a valid hardware target: " ++ s)

-- | Get hardware flag based on a list of parsed flags. Defaults to 'FirstOfAny'. If
-- multiple options are given, the last one is picked.
getHardwareTargetsFlag :: [HardwareTargets] -> HardwareTargets
getHardwareTargetsFlag = fromMaybe FirstOfAny . lastMaybe

-- | List of custom flags supported by us. Note that we currently support only
-- one flag, 'HardwareTargets'.
customFlags :: [OptDescr (Either String HardwareTargets)]
customFlags =
  [ Option
      "" -- no short flags
      ["hardware-targets"] -- long name of flag
      (ReqArg parseHardwareTargetsFlag "TARGET")
      "Options: FirstOfAny, FirstOfKnown, All. See 'HardwareTargets' in 'Flags.hs'."
  ]
