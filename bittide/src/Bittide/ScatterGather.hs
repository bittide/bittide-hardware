-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Bittide.ScatterGather
  ( scatterEngine
  , scatterUnitWb
  , gatherUnitWb
  ) where

import Clash.Prelude

import Contranomy.Wishbone
import Data.Type.Equality ((:~:)(Refl))

import Bittide.Calendar
import Bittide.DoubleBufferedRam
import Bittide.SharedTypes
import Data.Constraint.Nat.Extra

-- | Calendar entry that can be used by a scatter or gather engine.
type CalendarEntry memDepth = Index memDepth

-- TODO: scatterEngine should be removed after merging #71, which removes scatterEngine

-- | scatterEngine is a memory bank that allows for random writes and sequentially
-- reads data based on an internal counter that runs up to the maximum index, then wraps around.
-- The initial contents are undefined.
scatterEngine ::
  (HiddenClockResetEnable dom, KnownNat memDepth, 1 <= memDepth, NFDataX a) =>
  -- | Indicates when a new metacycle has started.
  Signal dom Bool ->
  -- | Incoming frame from link, if it contains Just a, a will be written to the memory.
  Signal dom (Maybe a) ->
  -- | Write address, when the incoming frame contains Just a, a will be written to this address.
  Signal dom (Index memDepth) ->
  -- | Data at the read address, delayed by one clock cycle.
  Signal dom a
scatterEngine newMetaCycle frameIn writeAddr =
  doubleBufferedRamU newMetaCycle readAddr writeFrame
 where
  readAddr = register 0 $ satSucc SatWrap <$> readAddr
  writeFrame = combineFrameWithAddr frameIn writeAddr

combineFrameWithAddr ::
  Signal dom (Maybe dat) ->
  Signal dom addr ->
  Signal dom (Maybe (addr,dat))
combineFrameWithAddr frameIn writeAddr = combine <$> frameIn <*> writeAddr
 where
  combine :: Maybe dat -> addr -> Maybe (addr,dat)
  combine frame addr = fmap (addr,) frame

-- | Double buffered memory component that can be written to by a Bittide link. The write
-- address of the incoming frame is determined by the incorporated 'calendar'. The buffers
-- are swapped at the beginning of each metacycle. Reading the buffer is done by supplying
-- a read address. Furthermore this component offers ports to control the incorporated 'calendar'.
scatterUnit ::
  ( HiddenClockResetEnable dom
  , KnownNat memDepth, 1 <= memDepth
  , KnownNat frameWidth) =>
  -- | Configuration for the 'calendar'.
  CalendarConfig nBytes addrW (CalendarEntry memDepth) ->
  -- | Wishbone (master -> slave) port for the 'calendar'.
  Signal dom (WishboneM2S nBytes addrW) ->
  -- | Swap active calendar and shadow calendar.
  Signal dom Bool ->
  -- | Incoming frame from Bittide link.
  Signal dom (DataLink frameWidth) ->
  -- | Read address.
  Signal dom (Index memDepth) ->
  -- | (Data at read address delayed 1 cycle, Wishbone (slave -> master) from 'calendar')
  (Signal dom (BitVector frameWidth), Signal dom (WishboneS2M nBytes))
scatterUnit calConfig wbIn calSwitch linkIn readAddr = (readOut, wbOut)
 where
  (writeAddr, metaCycle, wbOut) = mkCalendar calConfig calSwitch wbIn
  writeOp = (\a b -> (a,) <$> b) <$> writeAddr <*> linkIn
  readOut = doubleBufferedRamU metaCycle readAddr writeOp

-- | Double buffered memory component that can be written to by a generic write operation. The
-- write address of the incoming frame is determined by the incorporated 'calendar'. The
-- buffers are swapped at the beginning of each metacycle. Reading the buffer is done by
-- supplying a read address. Furthermore this component offers ports to control the
-- incorporated 'calendar'.
gatherUnit ::
  ( HiddenClockResetEnable dom
  , KnownNat memDepth, 1 <= memDepth
  , KnownNat frameWidth, 1 <= frameWidth
  , KnownNat (DivRU frameWidth 8), 1 <= (DivRU frameWidth 8)) =>
  -- | Configuration for the 'calendar'.
  CalendarConfig nBytes addrW (CalendarEntry memDepth) ->
  -- | Wishbone (master -> slave) port for the 'calendar'.
  Signal dom (WishboneM2S nBytes addrW) ->
  -- | Swap active calendar and shadow calendar.
  Signal dom Bool ->
  -- | Write operation writing a frame.
  Signal dom (Maybe (LocatedBits memDepth frameWidth)) ->
  -- | Byte enable for write operation.
  Signal dom (ByteEnable (BitVector frameWidth)) ->
  -- | (Transmitted  frame to Bittide Link, Wishbone (slave -> master) from 'calendar')
  (Signal dom (DataLink frameWidth), Signal dom (WishboneS2M nBytes))
gatherUnit calConfig wbIn calSwitch writeOp byteEnables= (linkOut, wbOut)
 where
  (readAddr, metaCycle, wbOut) = mkCalendar calConfig calSwitch wbIn
  linkOut = mux (register True ((==0) <$> readAddr)) (pure Nothing) (Just <$> bramOut)
  bramOut = doubleBufferedRamByteAddressableU metaCycle readAddr writeOp byteEnables

-- | Wishbone interface for the 'scatterUnit' and 'gatherUnit'. It makes the scatter and gather
-- unit, which operate on 64 bit frames, addressable via a 32 bit wishbone bus.
wbInterface ::
  forall nBytes addrW addresses .
  ( KnownNat nBytes
  , KnownNat addresses, 1 <= addresses
  , KnownNat addrW, 2 <= addrW) =>
  -- | Maximum address of the respective memory element as seen from the wishbone side.
  Index addresses ->
  -- | Wishbone (master -> slave) data.
  WishboneM2S nBytes addrW ->
  -- | Read data to be send to over the (slave -> master) port.
  Bytes nBytes ->
  -- | (slave - master data, read address memory element, write data memory element)
  (WishboneS2M nBytes, Index addresses, Maybe (Bytes nBytes))
wbInterface addressRange WishboneM2S{..} readData =
  (WishboneS2M{readData, acknowledge, err}, memAddr, writeOp)
 where
  masterActive = strobe && busCycle
  (alignedAddress, alignment) = split @_ @(addrW - 2) @2 addr
  wordAligned = alignment == 0
  err = masterActive && ((alignedAddress > resize (pack addressRange)) || not wordAligned)
  acknowledge = masterActive && not err
  wbAddr = unpack . resize $ pack alignedAddress
  memAddr = wbAddr
  writeOp | strobe && writeEnable && not err = Just writeData
          | otherwise  = Nothing

-- | Wishbone addressable 'scatterUnit', the wishbone port can read the data from this
-- memory element as if it has a 32 bit port by selecting the upper 32 or lower 32 bits
-- of the read data.
scatterUnitWb ::
  forall dom memDepth addrWidthSu nBytesCal addrWidthCal .
  ( HiddenClockResetEnable dom
  , KnownNat memDepth, 1 <= memDepth
  , KnownNat addrWidthSu, 2 <= addrWidthSu) =>
  -- | Configuration for the 'calendar'.
  CalendarConfig nBytesCal addrWidthCal (CalendarEntry memDepth) ->
  -- | Wishbone (master -> slave) port 'calendar'.
  Signal dom (WishboneM2S nBytesCal addrWidthCal) ->
  -- | Swap active calendar and shadow calendar.
  Signal dom Bool ->
  -- | Incoming frame from Bittide link.
  Signal dom (DataLink 64) ->
  -- | Wishbone (master -> slave) port scatter memory.
  Signal dom (WishboneM2S 4 addrWidthSu) ->
  -- | (Wishbone (slave -> master) port scatter memory, Wishbone (slave -> master) port 'calendar')
  (Signal dom (WishboneS2M 4), Signal dom (WishboneS2M nBytesCal))
scatterUnitWb calConfig wbInCal calSwitch linkIn wbInSU =
  (delayControls wbOutSU, wbOutCal)
 where
  (wbOutSU, memAddr, _) = unbundle $ wbInterface maxBound <$> wbInSU <*> scatteredData
  (readAddr, upperSelected) = unbundle $ coerceIndices <$> memAddr
  (scatterUnitRead, wbOutCal) = scatterUnit calConfig wbInCal calSwitch linkIn readAddr
  (upper, lower) = unbundle $ split <$> scatterUnitRead
  selected = register (errorX "scatterUnitWb: Initial selection undefined") upperSelected
  scatteredData = mux selected upper lower

-- | Wishbone addressable 'gatherUnit', the wishbone port can write data to this
-- memory element as if it has a 32 bit port by controlling the byte enables of the
-- 'gatherUnit' based on the third bit.
gatherUnitWb ::
  forall dom memDepth addrWidthGu nBytesCal addrWidthCal .
  ( HiddenClockResetEnable dom
  , KnownNat memDepth, 1 <= memDepth
  , KnownNat addrWidthGu, 2 <= addrWidthGu) =>
  -- | Configuration for the 'calendar'.
  CalendarConfig nBytesCal addrWidthCal (CalendarEntry memDepth) ->
  -- | Wishbone (master -> slave) data 'calendar'.
  Signal dom (WishboneM2S nBytesCal addrWidthCal) ->
  -- | Swap active calendar and shadow calendar.
  Signal dom Bool ->
  -- | Wishbone (master -> slave) port gather memory.
  Signal dom (WishboneM2S 4 addrWidthGu) ->
  -- | (Wishbone (slave -> master) port gather memory, Wishbone (slave -> master) port 'calendar')
  (Signal dom (DataLink 64), Signal dom (WishboneS2M 4), Signal dom (WishboneS2M nBytesCal))
gatherUnitWb calConfig wbInCal calSwitch wbInSU =
  (linkOut, delayControls wbOutSU, wbOutCal)
 where
  (wbOutSU, memAddr, writeOp) = unbundle $ wbInterface maxBound <$> wbInSU <*> pure 0b0
  (writeAddr, upperSelected) = unbundle $ coerceIndices <$> memAddr
  (linkOut, wbOutCal) =
    gatherUnit calConfig wbInCal calSwitch gatherWrite gatherByteEnables
  gatherWrite = mkWrite <$> writeAddr <*> writeOp
  gatherByteEnables = mkEnables <$> upperSelected <*> (busSelect <$> wbInSU)

  -- We update the 64 bit entry of the 'gatherUnit' in chunks of 32 bits. Thus we repeat
  -- the writeData of the wishbone bus twice in the write operation to the 'gatherUnit' and
  -- use the byte enables to either update the upper or lower
  mkWrite address (Just write) = Just (address, write ++# write)
  mkWrite _ _ = Nothing
  mkEnables selected byteEnables
    | selected  = byteEnables ++# 0b0
    | otherwise = 0b0 ++# byteEnables

-- | Coerces an index of size (n*2) to index n with the LSB as separate boolean.
coerceIndices :: forall n. (KnownNat n, 1 <= n) => Index (n*2) -> (Index n, Bool)
coerceIndices = case clog2axiom @n of Refl -> bitCoerce

-- | Delays the output controls to align them with the actual read / write timing.
delayControls :: HiddenClockResetEnable dom =>
  Signal dom (WishboneS2M nBytes) -> Signal dom (WishboneS2M nBytes)
delayControls wbIn = wbOut
 where
   delayedAck = register False (acknowledge <$> wbIn)
   delayedErr = register False (err <$> wbIn)
   wbOut = (\wb newAck newErr-> wb{acknowledge = newAck, err = newErr})
    <$> wbIn <*> delayedAck <*> delayedErr
