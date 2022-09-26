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
import Protocols.Wishbone

import Bittide.DoubleBufferedRam
import Bittide.SharedTypes
import Bittide.Wishbone

-- | Configuration for a 'processingElement'.
data PeConfig nBusses where
  PeConfig ::
    ( KnownNat depthI, 1 <= depthI
    , KnownNat depthD, 1 <= depthD) =>
    -- | The 'MemoryMap' for the contained 'singleMasterInterconnect'.
    MemoryMap nBusses 32 ->
    -- | Initial content of the instruction memory, can be smaller than its total depth.
    InitialContent depthI (Bytes 4) ->
    -- | Initial content of the data memory, can be smaller than its total depth.
    InitialContent depthD (Bytes 4) ->
    -- | Program counter reset value.
    BitVector 32 ->
    PeConfig nBusses

-- | 'Contranomy' based RV32IMC core together with instruction memory, data memory and
-- 'singleMasterInterconnect'.
processingElement ::
  forall dom nBusses .
  ( HiddenClockResetEnable dom
  , KnownNat nBusses, 2 <= nBusses) =>
  PeConfig nBusses ->
  Vec (nBusses-2) (Signal dom (WishboneS2M (Bytes 4))) ->
  Vec (nBusses-2) (Signal dom (WishboneM2S 32 4 (Bytes 4)))
processingElement config bussesIn = case config of
  PeConfig memMapConfig initI initD pcEntry ->
    go memMapConfig initI initD pcEntry
 where
  go ::
    ( KnownNat depthI, 1 <= depthI
    , KnownNat depthD, 1 <= depthD) =>
    MemoryMap nBusses 32 ->
    InitialContent depthI (Bytes 4) ->
    InitialContent depthD (Bytes 4) ->
    BitVector 32 ->
    Vec (nBusses-2) (Signal dom (WishboneM2S 32 4 (Bytes 4)))
  go memMapConfig initI initD pcEntry = bussesOut
   where
    tupToCoreIn (timerInterrupt, softwareInterrupt, externalInterrupt, iBusS2M, dBusS2M) =
      CoreIn {..}

    -- Interrupts are not used
    rvIn = tupToCoreIn <$> bundle (pure False, pure False, pure 0, iToCore, dToCore)
    rvOut = rv32 pcEntry rvIn

    iFromCore = iBusM2S <$> rvOut
    dFromCore = dBusM2S <$> rvOut

    (dToCore, unbundle -> toSlaves) = singleMasterInterconnect' memMapConfig dFromCore fromSlaves

    fromSlaves = bundle (iToMap :> dToMap :> bussesIn)
    (iFromMap :> dFromMap :> bussesOut) = toSlaves

    (iToCore, iToMap) = wbStorageDP initI iFromCore iFromMap
    dToMap = wbStorage' initD dFromMap

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
