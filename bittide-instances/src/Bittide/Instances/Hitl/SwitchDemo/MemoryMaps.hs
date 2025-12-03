-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Bittide.Instances.Hitl.SwitchDemo.MemoryMaps where

import Clash.Explicit.Prelude
import Protocols

import Bittide.Instances.Hitl.SwitchDemo.BringUp (bringUp)
import Bittide.SharedTypes (withBittideByteOrder)
import Protocols.MemoryMap (MemoryMap)
import VexRiscv (JtagIn (..))

import qualified Protocols.Spi as Spi

boot, mu, cc :: MemoryMap
(boot, mu, cc) = (bootMm, muMm, ccMm)
 where
  Circuit circuitFn = withBittideByteOrder $ bringUp clockGen noReset

  ((SimOnly bootMm, SimOnly muMm, SimOnly ccMm, _, _), _) =
    circuitFn
      (
        ( ()
        , ()
        , ()
        , pure (JtagIn 0 0 0)
        , (clockGen, SimOnly (repeat 0), 0, 0, repeat "", repeat "")
        )
      ,
        ( pure (Spi.S2M low)
        , pure low
        , ()
        , ()
        )
      )
