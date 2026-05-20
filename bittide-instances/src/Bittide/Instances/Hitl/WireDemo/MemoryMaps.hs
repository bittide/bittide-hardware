-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Bittide.Instances.Hitl.WireDemo.MemoryMaps where

import Clash.Explicit.Prelude
import Protocols

import Bittide.Instances.Hitl.WireDemo.BringUp (bringUp)
import Bittide.SharedTypes (withLittleEndian)
import Clash.Sized.Vector.ToTuple (vecToTuple)
import Protocols.MemoryMap (MemoryMap)
import VexRiscv (JtagIn (..))

import qualified Protocols.Spi as Spi

boot, managementUnit, cc :: MemoryMap
(boot, managementUnit, cc) = (bootMm, managementUnitMm, ccMm)
 where
  Circuit circuitFn = withLittleEndian $ bringUp clockGen noReset

  (SimOnly bootMm, SimOnly managementUnitMm, SimOnly ccMm) = vecToTuple memoryMaps

  ((memoryMaps, _, _), _) =
    circuitFn
      (
        ( repeat ()
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
