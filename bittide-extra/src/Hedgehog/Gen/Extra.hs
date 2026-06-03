-- SPDX-FileCopyrightText: 2026 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Hedgehog.Gen.Extra where

import Hedgehog
import Prelude

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

smallInt :: Range Int
smallInt = Range.linear 0 10

genSmallInt :: Gen Int
genSmallInt =
  Gen.frequency
    [ (90, Gen.integral smallInt)
    , (10, Gen.constant (Range.lowerBound 99 smallInt))
    ]
