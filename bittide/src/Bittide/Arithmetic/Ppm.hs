-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NumericUnderscores #-}

module Bittide.Arithmetic.Ppm where

import Clash.Explicit.Prelude
import Clash.Signal.Internal (Femtoseconds (Femtoseconds), hzToFs, fsToHz)

import Data.Int (Int64)
import Data.Ratio
import GHC.Stack (HasCallStack)
import Numeric.Natural
import System.Random (Random)

newtype Ppm = Ppm Int64
  deriving newtype (Num, Random)
  deriving (Lift, Show)

type Hz = Ratio Natural

-- PPM arithmetic on Hz
diffHz :: HasCallStack => Ppm -> Hz -> Hz
diffHz (Ppm ppm) hz
  | ppm < 0   = error $ "diffHz: ppm must be absolute, not" <> show ppm
  | otherwise = hz / (1e6 / (fromIntegral ppm % 1))

speedUpHz :: HasCallStack => Ppm -> Hz -> Hz
speedUpHz ppm hz = hz + diffHz ppm hz

slowDownHz :: HasCallStack => Ppm -> Hz -> Hz
slowDownHz ppm hz = hz - diffHz ppm hz

-- PPM arithmetic on periods
diffPeriod :: HasCallStack => Ppm -> Femtoseconds -> Femtoseconds
diffPeriod (Ppm ppm) (Femtoseconds fs) = Femtoseconds absFs
 where
  absFs = fs `div` (1_000_000 `div` ppm)

adjustPeriod :: HasCallStack => Ppm -> Femtoseconds -> Femtoseconds
adjustPeriod (Ppm ppm) fs =
  case compare ppm 0 of
    LT -> hzToFs (slowDownHz (Ppm (abs ppm)) (fsToHz fs))
    EQ -> fs
    GT -> hzToFs (speedUpHz (Ppm ppm) (fsToHz fs))
