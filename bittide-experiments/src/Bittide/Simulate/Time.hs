-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Bittide.Simulate.Time
  ( Offset
  , StepSize
  , Period
  , addFs
  , subFsZero
  ) where

import Clash.Prelude
import Clash.Signal.Internal

-- Number of type aliases for documentation purposes in various functions defined
-- down below.
type Offset = Femtoseconds
type StepSize = Femtoseconds
type Period = Femtoseconds

addFs :: Femtoseconds -> Femtoseconds -> Femtoseconds
addFs (Femtoseconds a) (Femtoseconds b) = Femtoseconds (a + b)

subFsZero :: Femtoseconds -> Femtoseconds -> Femtoseconds
subFsZero (Femtoseconds a) (Femtoseconds b)
  | aMinB < 0 = Femtoseconds 0
  | otherwise = Femtoseconds aMinB
 where
  aMinB = a - b
