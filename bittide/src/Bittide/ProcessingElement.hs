-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS -fplugin=Protocols.Plugin #-}

module Bittide.ProcessingElement where

import Clash.Prelude

import Protocols
import Protocols.Wishbone
import VexRiscv (Input(..), Output(..), vexRiscv)

import Bittide.DoubleBufferedRam
import Bittide.Extra.Maybe
import Bittide.SharedTypes
import Bittide.Wishbone

import qualified Data.ByteString as BS

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
processingElement ::
  forall dom nBusses .
  ( HiddenClockResetEnable dom
  , KnownNat nBusses, 2 <= nBusses, CLog 2 nBusses <= 30) =>
  PeConfig nBusses ->
  Circuit () (Vec (nBusses-2) (Wishbone dom 'Standard (MappedBus 32 nBusses) (Bytes 4)))
processingElement (PeConfig memMapConfig' initI' initD') =
  go memMapConfig' initI' initD'
 where
  go ::
    ( KnownNat depthI, 1 <= depthI
    , KnownNat depthD, 1 <= depthD) =>
    MemoryMap nBusses ->
    InitialContent depthI (Bytes 4) ->
    InitialContent depthD (Bytes 4) ->
    Circuit () (Vec (nBusses-2) (Wishbone dom 'Standard (MappedBus 32 nBusses) (BitVector 32)))
  go memMapConfig initI initD = circuit $ do
    (iBus, dBus) <- rvCircuit (pure low) (pure low) (pure low)
    ([iMemBus, dMemBus], extBusses) <-
      (splitAtCircuit d2 <| singleMasterInterconnect memMapConfig) -< dBus
    wbStorage initI -< iBus
    wbStorageDPC initD -< (iMemBus, dMemBus)
    idC -< extBusses

splitAtCircuit ::
  SNat left ->
  Circuit (Vec (left + right) a) (Vec left a , Vec right a)
splitAtCircuit SNat = Circuit go
 where
  go (fwd,(bwdLeft, bwdRight)) = (bwd,(fwdLeft, fwdRight))
   where
    (fwdLeft, fwdRight) = splitAtI fwd
    bwd = bwdLeft ++ bwdRight

rvCircuit ::
  (HiddenClockResetEnable dom) =>
  Signal dom Bit ->
  Signal dom Bit ->
  Signal dom Bit ->
  Circuit () (Wishbone dom 'Standard 32 (Bytes 4), Wishbone dom 'Standard 32 (Bytes 4))
rvCircuit tInterrupt sInterrupt eInterrupt = Circuit go
  where
  go ((),(iBusIn, dBusIn)) = ((),(iBusOut, dBusOut))
   where
    tupToCoreIn (timerInterrupt, softwareInterrupt, externalInterrupt, iBusWbS2M, dBusWbS2M) =
      Input {..}
    rvIn = tupToCoreIn <$> bundle (tInterrupt, sInterrupt, eInterrupt, iBusIn, dBusIn)
    rvOut = vexRiscv rvIn

    -- The VexRiscv instruction- and data-busses assume a conceptual [Bytes 4] memory
    -- while our storages work like [Bytes 1]. This is also why the address width of
    -- the VexRiscv busses are 30 bit and still cover the whole address space.
    -- These shifts bring the addresses "back into the byte domain" so to speak.
    iBusOut = mapAddr ((`shiftL` 2) . extend @_ @_ @2) . iBusWbM2S <$> rvOut
    dBusOut = mapAddr ((`shiftL` 2) . extend) . dBusWbM2S <$> rvOut

    mapAddr :: (BitVector aw1 -> BitVector aw2) -> WishboneM2S aw1 selWidth a -> WishboneM2S aw2 selWidth a
    mapAddr f wb = wb { addr = f (addr wb) }

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

    output = orNothing acknowledge (busSelect, writeData)
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
