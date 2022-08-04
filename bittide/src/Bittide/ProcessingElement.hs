-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
module Bittide.ProcessingElement where

import Clash.Prelude

import Contranomy.Core(CoreIn(..), CoreOut(..),core)
import Contranomy.RegisterFile

import Bittide.DoubleBufferedRam
import Bittide.Extra.Wishbone
import Bittide.SharedTypes
import Bittide.Wishbone

-- | Configuration for a Bittide Processing Element.
data PeConfig nBusses where
  PeConfig ::
    ( KnownNat initDepthI, initDepthI <= depthI, 1 <= depthI, 1 <= initDepthI
    , KnownNat initDepthD, initDepthD <= depthD, 1 <= depthD, 1 <= initDepthD) =>
    -- | The 'MemoryMap' for the contained 'singleMasterInterconnect'.
    MemoryMap nBusses 32 ->
    -- | Total depth of the instruction memory.
    SNat depthI ->
    -- | Total depth of the data memory.
    SNat depthD ->
    -- | Inititial content of the instruction memory, can be smaller than its total depth.
    InitialContent initDepthI (Bytes 4) ->
    -- | Inititial content of the data memory, can be smaller than its total depth.
    InitialContent initDepthD (Bytes 4) ->
    -- | Initial program counter coming out of reset.
    BitVector 32 ->
    PeConfig nBusses

-- | 'Contranomy' based RV32IMC core together with instruction memory, data memory and
-- 'singleMasterInterconnect'.
processingElement ::
  forall dom nBusses .
  ( HiddenClockResetEnable dom
  , KnownNat nBusses, 2 <= nBusses) =>
  PeConfig nBusses ->
  Vec (nBusses-2) (Signal dom (WishboneS2M 4)) ->
  Vec (nBusses-2) (Signal dom (WishboneM2S 4 32))
processingElement config bussesIn = case config of
  PeConfig memMapConfig depthI depthD initI initD pcEntry ->
    go memMapConfig depthI depthD initI initD pcEntry
 where
  go ::
    ( KnownNat initDepthI, initDepthI <= depthI, 1 <= depthI, 1 <= initDepthI
    , KnownNat initDepthD, initDepthD <= depthD, 1 <= depthD, 1 <= initDepthD) =>
    MemoryMap nBusses 32 ->
    SNat depthI ->
    SNat depthD ->
    InitialContent initDepthI (Bytes 4) ->
    InitialContent initDepthD (Bytes 4) ->
    BitVector 32 ->
    Vec (nBusses-2) (Signal dom (WishboneM2S 4 32))
  go memMapConfig depthI@SNat depthD@SNat initI initD pcEntry = bussesOut
   where
    tupToCoreIn (timerInterrupt, softwareInterrupt, externalInterrupt, iBusS2M, dBusS2M) =
      CoreIn {..}

    -- Interrupts are not used
    rvIn = tupToCoreIn <$> bundle (pure False, pure False, pure 0, iToCore, dToCore)
    rvOut = rv32 pcEntry rvIn

    iFromCore = iBusM2S <$> rvOut
    dFromCore = dBusM2S <$> rvOut

    (dToCore, unbundle -> toSlaves) = singleMasterInterconnect memMapConfig dFromCore fromSlaves

    fromSlaves = bundle (iToMap :> dToMap :> bussesIn)
    (iFromMap :> dFromMap :> bussesOut) = toSlaves

    (iToCore, iToMap) = wbStorageDP depthI initI iFromCore iFromMap
    dToMap = wbStorage depthD initD dFromMap

-- | Contranomy RV32IMC core
rv32 ::
  HiddenClockResetEnable dom =>
  -- | Reset program counter.
  BitVector 32 ->
  -- |
  Signal dom CoreIn ->
  Signal dom CoreOut
rv32 entry coreIn =
  let (coreResult,regWrite,_) = core entry (coreIn,regOut)
      regOut = registerFile regWrite
   in coreResult
