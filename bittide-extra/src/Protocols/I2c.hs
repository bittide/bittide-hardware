-- SPDX-FileCopyrightText: 2026 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
module Protocols.I2c (
  I2c,
  I2cIn,
  I2cOut,
) where

import Clash.Prelude
import Protocols

import Clash.Cores.I2c (I2cIn, I2cOut)

{- | An I2C bus, seen from the controller: it drives the lines ('I2cOut') and
samples them ('I2cIn').
-}
data I2c (dom :: Domain)

instance Protocol (I2c dom) where
  type Fwd (I2c dom) = Signal dom I2cOut
  type Bwd (I2c dom) = Signal dom I2cIn
