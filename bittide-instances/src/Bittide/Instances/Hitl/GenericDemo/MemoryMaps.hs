-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{- | Memory-map extractor shared between the wire and soft-UGN demos. Each
demo's own 'MemoryMaps' module passes its ring-buffer depth and user-core
circuit; this module extracts the three resulting memory maps (boot,
management unit, clock control).
-}
module Bittide.Instances.Hitl.GenericDemo.MemoryMaps (extractMemoryMaps) where

import Clash.Explicit.Prelude
import Protocols

import Bittide.Instances.Hitl.GenericDemo.BringUp (
  NmuRemBusWidth,
  UserCoreCircuit,
  bringUp,
 )
import Bittide.Instances.Hitl.GenericDemo.Core (NmuExternalBusses, NmuInternalBusses)
import Bittide.ProcessingElement (PrefixWidth)
import Clash.Sized.Vector.ToTuple (vecToTuple)
import Protocols.MemoryMap (MemoryMap)
import VexRiscv (JtagIn (..))

import qualified Protocols.Spi as Spi

extractMemoryMaps ::
  forall userCoreBusses ringBufferDepth.
  ( KnownNat userCoreBusses
  , KnownNat ringBufferDepth
  , 1 <= ringBufferDepth
  , PrefixWidth (NmuExternalBusses userCoreBusses + NmuInternalBusses) <= 30
  , 1 <= NmuRemBusWidth userCoreBusses
  , NmuRemBusWidth userCoreBusses <= 27
  ) =>
  SNat ringBufferDepth ->
  UserCoreCircuit userCoreBusses (NmuRemBusWidth userCoreBusses) ->
  -- | (boot, management unit, clock control)
  (MemoryMap, MemoryMap, MemoryMap)
extractMemoryMaps bufferDepth mkUserCore = (bootMm, managementUnitMm, clockControlMm)
 where
  Circuit circuitFn = bringUp bufferDepth mkUserCore clockGen noReset

  (SimOnly bootMm, SimOnly managementUnitMm, SimOnly clockControlMm) = vecToTuple memoryMaps

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
