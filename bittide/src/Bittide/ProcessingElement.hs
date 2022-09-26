-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}

module Bittide.ProcessingElement where

import Clash.Prelude

import Contranomy.Core(CoreIn(..), CoreOut(..),core)
import Contranomy.RegisterFile
import Protocols.Wishbone

import Bittide.DoubleBufferedRam
import Bittide.SharedTypes
import Bittide.Wishbone

import qualified Data.ByteString as BS

-- | Configuration for a Bittide Processing Element.
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
  , KnownNat nBusses, 3 <= nBusses) =>
  PeConfig nBusses ->
  Vec (nBusses-3) (Signal dom (WishboneS2M (Bytes 4))) ->
  ( Vec (nBusses-3) (Signal dom (WishboneM2S 32 4 (Bytes 4)))
  , Signal dom (Maybe (BitVector 4, BitVector 32)))
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
    ( Vec (nBusses-3) (Signal dom (WishboneM2S 32 4 (Bytes 4)))
    , Signal dom (Maybe (BitVector 4, BitVector 32))
    )
  go memMapConfig initI initD pcEntry = (bussesOut, sinkOut)
   where
    tupToCoreIn (timerInterrupt, softwareInterrupt, externalInterrupt, iBusS2M, dBusS2M) =
      CoreIn {..}

    -- Interrupts are not used
    rvIn = tupToCoreIn <$> bundle (pure False, pure False, pure 0, iToCore, dToCore)
    rvOut = rv32 pcEntry rvIn

    iFromCore = iBusM2S <$> rvOut
    dFromCore = dBusM2S <$> rvOut

    (dToCore, unbundle -> toSlaves) = singleMasterInterconnect' memMapConfig dFromCore fromSlaves

    fromSlaves = bundle (iToMap :> dToMap :> sinkS2M :> bussesIn)
    (iFromMap :> dFromMap :> sinkM2S :> bussesOut) = toSlaves

    (iToCore, iToMap) = wbStorageDP initI iFromCore iFromMap
    dToMap = wbStorage' initD dFromMap

    (sinkS2M, sinkOut) = unbundle $ wishboneSink sinkM2S

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
