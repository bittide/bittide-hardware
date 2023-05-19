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
import Clash.Cores.Xilinx.VIO

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
    (iBus0, dBus) <- rvCircuit (pure low) (pure low) (pure low)
    ([iMemBus, dMemBus], extBusses) <-
      (splitAtCircuit d2 <| singleMasterInterconnect memMapConfig) -< dBus
    wbStorage initD -< dMemBus
    iBus1 <- removeMsb -< iBus0
    wbStorageDPC initI -< (iBus1, iMemBus)
    idC -< extBusses

  removeMsb :: forall aw a . KnownNat aw => Circuit (Wishbone dom 'Standard (aw + 1) a) (Wishbone dom 'Standard aw a)
  removeMsb = wbMap (mapAddr (truncateB  :: BitVector (aw + 1) -> BitVector aw)) id


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
  forall dom .
  (HiddenClockResetEnable dom) =>
  Signal dom Bit ->
  Signal dom Bit ->
  Signal dom Bit ->
  Circuit () (Wishbone dom 'Standard 32 (Bytes 4), Wishbone dom 'Standard 32 (Bytes 4))
rvCircuit tInterrupt sInterrupt eInterrupt = Circuit go
  where
  go ((),(iBusIn, dBusIn)) = hwSeqX probes ((),(iBusOut, dBusOut))
   where
    tupToCoreIn (timerInterrupt, softwareInterrupt, externalInterrupt, iBusWbS2M, dBusWbS2M) =
      Input {..}
    rvIn = tupToCoreIn <$> bundle (tInterrupt, sInterrupt, eInterrupt, iBusIn, dBusIn)
    rvOut = vexRiscv rvIn

    probes =
      (vioProbe () hasClock (addr <$> iBusOut)) :>
      (vioProbe () hasClock (busCycle <$> iBusOut)) :>
      (vioProbe () hasClock (strobe <$> iBusOut)) :>
      (vioProbe () hasClock (writeEnable <$> iBusOut)) :>
      (vioProbe () hasClock (writeData <$> iBusOut)) :>
      (vioProbe () hasClock (acknowledge <$> iBusIn)) :>
      (vioProbe () hasClock (err <$> iBusIn)) :>
      (vioProbe () hasClock (readData <$> iBusIn)) :> Nil :: Vec 8 (Signal dom ())

    -- iBusEnable = vioProbe True hasClock $ (\ (WishboneM2S{addr, busCycle, strobe}) -> (addr, busCycle, strobe)) <$> dBusOut

    -- The VexRiscv instruction- and data-buses assume a conceptual [Bytes 4] memory
    -- while our storages work like [Bytes 1]. This is also why the address width of
    -- the VexRiscv busses are 30 bit and still cover the whole address space.
    -- These shifts bring the addresses "back into the byte domain" so to speak.
    iBusOut = mapAddr ((`shiftL` 2) . extend @_ @_ @2) . iBusWbM2S <$> rvOut
    dBusOut = mapAddr ((`shiftL` 2) . extend) . dBusWbM2S <$> rvOut

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

wbMap ::
  ( WishboneM2S aw0 (BitSize dat0 `DivRU` 8) dat0 ->
    WishboneM2S aw1 (BitSize dat1 `DivRU` 8) dat1
  ) ->
  (WishboneS2M dat1 -> WishboneS2M dat0) ->
  Circuit (Wishbone dom mode aw0 dat0) (Wishbone dom mode aw1 dat1)
wbMap fwd bwd = Circuit $ \(m2s, s2m) -> (fmap bwd s2m, fmap fwd m2s)

mapAddr ::
  (BitVector aw1 -> BitVector aw2) ->
  WishboneM2S aw1 selWidth a ->
  WishboneM2S aw2 selWidth a
mapAddr f wb = wb { addr = f (addr wb) }
