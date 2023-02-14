-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}

module Bittide.ProcessingElement where

import Clash.Prelude

import Protocols.Wishbone

import Bittide.DoubleBufferedRam
import Bittide.SharedTypes
import Bittide.Wishbone

import qualified Data.ByteString as BS
import qualified VexRiscv
import qualified Contranomy.Core as Contranomy
import qualified Contranomy.RegisterFile as Contranomy

-- | Configuration for a Bittide Processing Element.
data PeConfig nBusses where
  PeConfig ::
    ( KnownNat depthI, 1 <= depthI
    , KnownNat depthD, 1 <= depthD) =>
    -- | The 'MemoryMap' for the contained 'singleMasterInterconnect'.
    MemoryMap nBusses ->
    -- | Initial content of the instruction memory, can be smaller than its total depth.
    InitialContent depthI (Bytes 4) ->
    -- | Initial content of the data memory, can be smaller than its total depth.
    InitialContent depthD (Bytes 4) ->
    PeConfig nBusses

-- | 'Contranomy' based RV32IMC core together with instruction memory, data memory and
-- 'singleMasterInterconnect'.
vexRiscvProcessingElement ::
  forall dom nBusses .
  ( HiddenClockResetEnable dom
  , KnownNat nBusses, 2 <= nBusses, CLog 2 nBusses <= 30) =>
  PeConfig nBusses ->
  Vec (nBusses-2) (Signal dom (WishboneS2M (Bytes 4))) ->
  Vec (nBusses-2) (Signal dom (WishboneM2S (32 - CLog 2 nBusses) 4 (Bytes 4)))
vexRiscvProcessingElement config bussesIn = case config of
  PeConfig memMapConfig initI initD ->
    go memMapConfig initI initD
 where
  go ::
    ( KnownNat depthI, 1 <= depthI
    , KnownNat depthD, 1 <= depthD) =>
    MemoryMap nBusses ->
    InitialContent depthI (Bytes 4) ->
    InitialContent depthD (Bytes 4) ->
    Vec (nBusses-2) (Signal dom (WishboneM2S (32 - CLog 2 nBusses) 4 (Bytes 4)))
  go memMapConfig initI initD = bussesOut
   where
    tupToCoreIn (timerInterrupt, softwareInterrupt, externalInterrupt, iBusWbS2M, dBusWbS2M) =
      VexRiscv.Input {..}

    -- Interrupts are not used
    rvIn = tupToCoreIn <$> bundle (pure low, pure low, pure low, iToCore, dToCore)
    rvOut = vexRiscRV32 rvIn

    iFromCore = mapAddr ((`shiftL` 2) . extend @_ @_ @2) . VexRiscv.iBusWbM2S <$> rvOut
    dFromCore = mapAddr ((`shiftL` 2) . extend) . VexRiscv.dBusWbM2S <$> rvOut

    (dToCore, unbundle -> toSlaves) = singleMasterInterconnect' memMapConfig dFromCore fromSlaves

    fromSlaves = bundle (iToMap :> dToMap :> bussesIn)
    (iFromMap :> dFromMap :> bussesOut) = toSlaves

    (iToCore, iToMap) = wbStorageDP initI iFromCore iFromMap
    dToMap = wbStorage' initD dFromMap

    mapAddr :: (BitVector aw1 -> BitVector aw2) -> WishboneM2S aw1 selWidth a -> WishboneM2S aw2 selWidth a
    mapAddr f wb = wb { addr = f (addr wb) }


contranomyProcessingElement ::
  forall dom nBusses .
  ( HiddenClockResetEnable dom
  , KnownNat nBusses, 2 <= nBusses, CLog 2 nBusses <= 30) =>
  PeConfig nBusses ->
  Vec (nBusses-2) (Signal dom (WishboneS2M (Bytes 4))) ->
  Vec (nBusses-2) (Signal dom (WishboneM2S (32 - CLog 2 nBusses) 4 (Bytes 4)))
contranomyProcessingElement config bussesIn = case config of
  PeConfig memMapConfig initI initD ->
    go memMapConfig initI initD
 where
  go ::
    ( KnownNat depthI, 1 <= depthI
    , KnownNat depthD, 1 <= depthD) =>
    MemoryMap nBusses ->
    InitialContent depthI (Bytes 4) ->
    InitialContent depthD (Bytes 4) ->
    Vec (nBusses-2) (Signal dom (WishboneM2S (32 - CLog 2 nBusses) 4 (Bytes 4)))
  go memMapConfig initI initD = bussesOut
   where
    tupToCoreIn (timerInterrupt, softwareInterrupt, externalInterrupt, iBusS2M, dBusS2M) =
      Contranomy.CoreIn {..}

    -- Interrupts are not used
    rvIn = tupToCoreIn <$> bundle (pure False, pure False, pure 0, iToCore, dToCore)
    rvOut = contranomyRV32 rvIn

    iFromCore = Contranomy.iBusM2S <$> rvOut
    dFromCore = Contranomy.dBusM2S <$> rvOut

    (dToCore, unbundle -> toSlaves) = singleMasterInterconnect' memMapConfig dFromCore fromSlaves

    fromSlaves = bundle (iToMap :> dToMap :> bussesIn)
    (iFromMap :> dFromMap :> bussesOut) = toSlaves

    (iToCore, iToMap) = wbStorageDP initI iFromCore iFromMap
    dToMap = wbStorage' initD dFromMap


-- | VexRiscv RV32IMC core
vexRiscRV32 ::
  HiddenClockResetEnable dom =>
  Signal dom VexRiscv.Input ->
  Signal dom VexRiscv.Output
vexRiscRV32 = VexRiscv.vexRiscv

-- | Contranomy RV32IC core
contranomyRV32 ::
  HiddenClockResetEnable dom =>
  Signal dom Contranomy.CoreIn ->
  Signal dom Contranomy.CoreOut
contranomyRV32 coreIn =
  let (coreResult,regWrite,_) = Contranomy.core 0x8000_0000 (coreIn,regOut)
      regOut = Contranomy.registerFile regWrite
   in coreResult

-- | Stateless wishbone device that only acknowledges writes to address 0.
-- Successful writes return the 'writeData' and 'busSelect'.
wishboneSink ::
  (KnownNat addressWidth, Paddable dat) =>
  -- | Incoming wishbone bus.
  Signal dom (WishboneM2S addressWidth bs dat) ->
  -- |
  -- 1. Outgoing wishbone bus.
  -- 2. Result of successful write attempt.
  Signal dom (WishboneS2M dat, Maybe (BitVector bs, dat))
wishboneSink = fmap go
 where
  go WishboneM2S{..} = (wbOut, output)
   where
    masterActive = busCycle && strobe
    addrLegal = addr == 0
    acknowledge = masterActive && writeEnable && addrLegal
    err = masterActive && (not writeEnable || not addrLegal)

    output
      | acknowledge = Just (busSelect, writeData)
      | otherwise   = Nothing

    wbOut = emptyWishboneS2M{acknowledge, err}

-- | Provide a vector of filepaths, and a write operations containing a byteSelect and
-- a vector of characters and, for each filepath write the corresponding byte to that file
-- if the corresponding byteSelect is @1@.
printCharacters ::
  (KnownNat paths, KnownNat chars, (paths + n) ~ chars) =>
  -- | Destination files for received bytes.
  Vec paths FilePath ->
  -- | Write attempt, bytes will only be written if the corresponding byteSelect is @1@.
  Maybe (BitVector chars, Vec chars Byte) ->
  IO ()
printCharacters Nil _ = pure ()
printCharacters paths@(Cons _ _) inps = case inps of
  Just (byteSelect, chars) ->
    sequence_ $ printToFiles <*> take SNat (unpack byteSelect) <*> take SNat chars
  Nothing   -> pure ()
 where
  printToFiles = printToFile <$> paths
  printToFile path byteSelect char
    | byteSelect = BS.appendFile path $ BS.singleton $ bitCoerce char
    | otherwise  = pure ()
