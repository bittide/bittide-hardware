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

import Bittide.Calendar
import Bittide.DoubleBufferedRam
import Bittide.Extra.Wishbone
import Bittide.SharedTypes

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
scatterEngine (fmap bitCoerce -> newMetaCycle) frameIn writeAddr =
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
  -- | Incoming frame from Bittide link.
  Signal dom (DataLink frameWidth) ->
  -- | Read address.
  Signal dom (Index memDepth) ->
  -- | (Data at read address delayed 1 cycle, Wishbone (slave -> master) from 'calendar')
  (Signal dom (BitVector frameWidth), Signal dom (WishboneS2M nBytes))
scatterUnit calConfig wbIn linkIn readAddr = (readOut, wbOut)
 where
  (writeAddr, metaCycle, wbOut) = mkCalendar calConfig wbIn
  writeOp = (\a b -> (a,) <$> b) <$> writeAddr <*> linkIn
  readOut = doubleBufferedRamU bufSelect0 readAddr writeOp
  bufSelect0 = register A bufSelect1
  bufSelect1 = mux metaCycle (flipBuffer <$> bufSelect0) bufSelect0

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
  -- | Write operation writing a frame.
  Signal dom (Maybe (LocatedBits memDepth frameWidth)) ->
  -- | Byte enable for write operation.
  Signal dom (ByteEnable (BitVector frameWidth)) ->
  -- | (Transmitted  frame to Bittide Link, Wishbone (slave -> master) from 'calendar')
  (Signal dom (DataLink frameWidth), Signal dom (WishboneS2M nBytes))
gatherUnit calConfig wbIn writeOp byteEnables= (linkOut, wbOut)
 where
  (readAddr, metaCycle, wbOut) = mkCalendar calConfig wbIn
  linkOut = mux (register True ((==0) <$> readAddr)) (pure Nothing) (Just <$> bramOut)
  bramOut = doubleBufferedRamByteAddressableU
    (bitCoerce <$> bufSelect0) readAddr writeOp byteEnables
  bufSelect0 = register A bufSelect1
  bufSelect1 = mux metaCycle (flipBuffer <$> bufSelect0) bufSelect0

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

{-# NOINLINE scatterUnitWb #-}
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
  -- | Incoming frame from Bittide link.
  Signal dom (DataLink 64) ->
  -- | Wishbone (master -> slave) port scatter memory.
  Signal dom (WishboneM2S 4 addrWidthSu) ->
  -- | (Wishbone (slave -> master) port scatter memory, Wishbone (slave -> master) port 'calendar')
  (Signal dom (WishboneS2M 4), Signal dom (WishboneS2M nBytesCal))
scatterUnitWb calConfig wbInCal linkIn wbInSu =
  (delayControls wbOutSu, wbOutCal)
 where
  (wbOutSu, memAddr, _) = unbundle $ wbInterface maxBound <$> wbInSu <*> scatteredData
  (readAddr, upperSelected) = unbundle $ div2Index <$> memAddr
  (scatterUnitRead, wbOutCal) = scatterUnit calConfig wbInCal linkIn readAddr
  (upper, lower) = unbundle $ split <$> scatterUnitRead
  selected = register (errorX "scatterUnitWb: Initial selection undefined") upperSelected
  scatteredData = mux selected upper lower

{-# NOINLINE gatherUnitWb #-}
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
  -- | Wishbone (master -> slave) port gather memory.
  Signal dom (WishboneM2S 4 addrWidthGu) ->
  -- | (Wishbone (slave -> master) port gather memory, Wishbone (slave -> master) port 'calendar')
  (Signal dom (DataLink 64), Signal dom (WishboneS2M 4), Signal dom (WishboneS2M nBytesCal))
gatherUnitWb calConfig wbInCal wbInGu =
  (linkOut, delayControls wbOutGu, wbOutCal)
 where
  (wbOutGu, memAddr, writeOp) = unbundle $ wbInterface maxBound <$> wbInGu <*> pure 0b0
  (writeAddr, upperSelected) = unbundle $ div2Index <$> memAddr
  (linkOut, wbOutCal) =
    gatherUnit calConfig wbInCal gatherWrite gatherByteEnables
  gatherWrite = mkWrite <$> writeAddr <*> writeOp
  gatherByteEnables = mkEnables <$> upperSelected <*> (busSelect <$> wbInGu)

  -- We update the 64 bit entry of the 'gatherUnit' in chunks of 32 bits. Thus we repeat
  -- the writeData of the wishbone bus twice in the write operation to the 'gatherUnit' and
  -- use the byte enables to either update the upper or lower
  mkWrite address (Just write) = Just (address, write ++# write)
  mkWrite _ _ = Nothing
  mkEnables selected byteEnables
    | selected  = byteEnables ++# 0b0
    | otherwise = 0b0 ++# byteEnables
