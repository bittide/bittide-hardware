-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Bittide.Instances.Pnr.AsciiDebugMux where

import Clash.Prelude
import Protocols

import Bittide.SharedTypes (Byte)

import qualified Bittide.Df as Df

asciiDebugMux ::
  Clock System ->
  Reset System ->
  Enable System ->
  Circuit (Vec 3 (Df System Byte)) (Df System Byte)
asciiDebugMux clk rst ena =
  withClockResetEnable @System clk rst ena
    $ Df.asciiDebugMux
      (SNat @1024)
      ((0xA :> Nil) :> (0xB :> Nil) :> (0xC :> Nil) :> Nil)
