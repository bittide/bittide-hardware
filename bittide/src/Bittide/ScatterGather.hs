-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module Bittide.ScatterGather
  ( scatterEngine
  , gatherEngine
  , scatterGatherEngine
  , scatterUnitWb
  , gatherUnitWb) where

import Clash.Prelude

import Contranomy.Wishbone
import Data.Proxy
import Data.Type.Equality ((:~:)(Refl))

import Bittide.Calendar
import Bittide.DoubleBufferedRam
import Bittide.SharedTypes

-- | Calendar entry that can be used by a scatter or gather engine.
type CalendarEntry memDepth = Index memDepth

-- | The Calendar for the scatter and gather engines contains entries of a single index
-- to the engine. For more information regarding calendar datatypes see
-- see NOTE [component calendar types]
type Calendar calDepth memDepth = Vec calDepth (CalendarEntry memDepth)

-- | Write operation for a scatter or gather calendar.
type ConfigurationPort calDepth memDepth = Maybe (Index calDepth, CalendarEntry memDepth)

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

-- | gatherEngine is a memory bank that allows for random reads and sequentially
-- writes data based on an internal counter that runs up to the maximum index and wraps around.
-- The initial contents are undefined.
gatherEngine ::
  (HiddenClockResetEnable dom, KnownNat memDepth, 1 <= memDepth, NFDataX a) =>
  -- | Indicates when a new metacycle has started.
  Signal dom Bool ->
  -- | Incoming frame from link, if it contains Just a, a will be written to the memory.
  Signal dom (Maybe a) ->
  -- | Read address.
  Signal dom (Index memDepth) ->
  -- | Data at the read address, delayed by one clock cycle.
  Signal dom a
gatherEngine newMetaCycle frameIn readAddr =
  doubleBufferedRamU newMetaCycle readAddr writeFrame
  where
    writeAddr = register 0 $ satSucc SatWrap <$> writeAddr
    writeFrame = combineFrameWithAddr frameIn writeAddr

combineFrameWithAddr ::
  Signal dom (Maybe dat) ->
  Signal dom addr ->
  Signal dom (Maybe (addr,dat))
combineFrameWithAddr frameIn writeAddr = combine <$> frameIn <*> writeAddr
  where
    combine :: Maybe dat -> addr -> Maybe (addr,dat)
    combine frame addr = fmap (addr,) frame

-- | scatterGatherEngine is a 4 port memory component that enables gathering and scattering for the processing element.
-- Scattering and gathering data is done using two separate memory banks with each their own calendar,
-- the writeAddrSwitch dictate the read and write address on the switch side for the gather and scatter memory respectively.
-- If the read address for the scatter engine is 0, a null frame (Nothing) will be sent to the switch.
scatterGatherEngine ::
  forall dom calDepthG calDepthS memDepthG memDepthS frameWidth .
  ( HiddenClockResetEnable dom
  , KnownNat calDepthG
  , KnownNat calDepthS
  , KnownNat memDepthG
  , KnownNat memDepthS
  , KnownNat frameWidth
  , 1 <= calDepthG
  , 1 <= calDepthS
  , 1 <= memDepthG
  , 1 <= memDepthS) =>
  -- | Bootstrap calendar gather memory.
  Calendar calDepthS memDepthS ->
  -- | Bootstrap calendar scatter memory.
  Calendar calDepthG memDepthG ->
  -- | Configuration port for the scatter engine.
  Signal dom (ConfigurationPort calDepthS memDepthS) ->
  -- | Configuration port for the gather engine.
  Signal dom (ConfigurationPort calDepthG memDepthG) ->
  -- | Incoming frame from the switch
  Signal dom (DataLink frameWidth) ->
  -- | Incoming data from the PE.
  Signal dom (DataLink frameWidth) ->
  -- | Read address of the PE frame.
  Signal dom (Index memDepthS) ->
  -- | Write address for the PE link.
  Signal dom (Index memDepthG) ->
  -- | Frame read by the switch and the data read by the PE.
  (Signal dom (BitVector frameWidth),Signal dom (DataLink frameWidth))
scatterGatherEngine bootCalS bootCalG scatterConfig gatherConfig
 frameInSwitch frameInPE readAddrPE writeAddrPE = (toPE, toSwitch)
  where
    -- Scatter engine
    frameInSwitch' = mux ((==0) <$> writeAddrSwitch) (pure Nothing) frameInSwitch
    (writeAddrSwitch, newMetaCycleS) = calendar bootCalS (pure False) scatterConfig
    writeFrameSwitch = (\f a -> fmap (a,) f) <$> frameInSwitch' <*> writeAddrSwitch
    scatterOut = doubleBufferedRamU newMetaCycleS readAddrPE writeFrameSwitch
    toPE      = scatterOut

    -- Gather engine
    frameInPE' = mux ((==0) <$> writeAddrPE) (pure Nothing) frameInPE
    (readAddrSwitch, newMetaCycleG) = calendar bootCalG (pure False) gatherConfig
    writeFramePE = (\f a -> fmap (a,) f) <$> frameInPE' <*> writeAddrPE
    gatherOut = doubleBufferedRamU newMetaCycleG readAddrSwitch writeFramePE
    toSwitch  = mux (register True $ (==0) <$> readAddrSwitch) (pure Nothing)
      $ Just <$> gatherOut


-- | Double buffered memory component that can be written to by a Bittide link. The write
-- address of the incoming frame is determined by the scatterUnit's calendar. The buffers
-- are swapped at the beginning of each metacycle. Reading the buffer is done by supplying
-- a read address. Furthermore this component offers ports to control the incorporated calendar.
scatterUnit ::
  ( HiddenClockResetEnable dom
  , KnownNat memDepth, 1 <= memDepth
  , KnownNat frameWidth) =>
  -- | Configuration for the calendar.
  CalendarConfig bytes addressWidth (CalendarEntry memDepth) ->
  -- | Wishbone master-slave port for the calendar.
  Signal dom (WishboneM2S bytes addressWidth) ->
  -- | Swap active calendar and shadow calendar.
  Signal dom Bool ->
  -- | Incoming frame from Bittide link.
  Signal dom (DataLink frameWidth) ->
  -- | Read address.
  Signal dom (Index memDepth) ->
  -- | (Data at read address delayed 1 cycle, Wishbone slave-master from calendar)
  (Signal dom (BitVector frameWidth), Signal dom (WishboneS2M bytes))
scatterUnit calConfig wbIn calSwitch linkIn readAddr = (readOut, wbOut)
 where
  (writeAddr, metaCycle, wbOut) = mkCalendar calConfig calSwitch wbIn
  writeOp = (\a b -> (a,) <$> b) <$> writeAddr <*> linkIn
  readOut = doubleBufferedRamU metaCycle readAddr writeOp

-- | Double buffered memory component that can be written to by a generic write operation. The
-- write address of the incoming frame is determined by the scatterUnit's calendar. The
-- buffers are swapped at the beginning of each metacycle. Reading the buffer is done by
-- supplying a read address. Furthermore this component offers ports to control the
-- incorporated calendar.
gatherUnit ::
  ( HiddenClockResetEnable dom
  , KnownNat memDepth, 1 <= memDepth
  , KnownNat frameWidth, 1 <= frameWidth
  , KnownNat (DivRU frameWidth 8), 1 <= (DivRU frameWidth 8)) =>
  -- | Configuration for the calendar.
  CalendarConfig bytes addressWidth (CalendarEntry memDepth) ->
  -- | Wishbone master-slave port for the calendar.
  Signal dom (WishboneM2S bytes addressWidth) ->
  -- | Swap active calendar and shadow calendar.
  Signal dom Bool ->
  -- | Generic write operation writing a frame.
  Signal dom (Maybe (LocatedBits memDepth frameWidth)) ->
  -- | Byte enable for write operation.
  Signal dom (ByteEnable (DivRU frameWidth 8)) ->
  -- | (Transmitted  frame to Bittide Link, Wishbone slave-master from calendar)
  (Signal dom (DataLink frameWidth), Signal dom (WishboneS2M bytes))
gatherUnit calConfig wbIn calSwitch writeOp byteEnables= (linkOut, wbOut)
 where
  (readAddr, metaCycle, wbOut) = mkCalendar calConfig calSwitch wbIn
  linkOut = mux (register True $ (==0) <$> readAddr) (pure Nothing) $ Just <$> bramOut
  bramOut = doubleBufferedRamByteAddressableU metaCycle readAddr writeOp byteEnables

-- | Wishbone interface for the scatterUnit and gatherUnit. It makes the scatter and gather
-- unit, which operate on 64 bit frames, addressable via a 32 bit wishbone bus.
wbInterface ::
  forall bytes addressWidth addresses .
  ( KnownNat bytes
  , KnownNat addresses, 1 <= addresses
  , KnownNat addressWidth, 2 <= addressWidth) =>
  -- | Maximum address of the respective memory element as seen from the wishbone side.
  Index addresses ->
  -- | Wishbone master - slave data.
  WishboneM2S bytes addressWidth ->
  -- | Read data to be send to over the slave-master port.
  Bytes bytes ->
  -- | (slave - master data, read address memory element, write data memory element)
  (WishboneS2M bytes, Index addresses, Maybe (Bytes bytes))
wbInterface addressRange WishboneM2S{..} readData =
  (WishboneS2M{readData, acknowledge, err}, memAddr, writeOp)
 where
  masterActive = strobe && busCycle
  (alignedAddress, alignment) = split @_ @(addressWidth - 2) @2 addr
  wordAligned = alignment == (0 :: BitVector 2)
  err = masterActive && ((alignedAddress > resize (pack addressRange)) || not wordAligned)
  acknowledge = masterActive && not err
  wbAddr = unpack . resize $ pack alignedAddress
  memAddr = wbAddr
  writeOp | strobe && writeEnable && not err = Just writeData
          | otherwise  = Nothing

-- | Wishbone addressable scatterUnit, the wishbone port can read the data from this
-- memory element as if it has a 32 bit port by selecting the upper 32 or lower 32 bits
-- of the read data.
scatterUnitWb ::
  forall dom memDepth awSU bsCal awCal .
  ( HiddenClockResetEnable dom
  , KnownNat memDepth, 1 <= memDepth
  , KnownNat awSU, 2 <= awSU) =>
  -- | Configuration for the calendar.
  CalendarConfig bsCal awCal (CalendarEntry memDepth) ->
  -- | Wishbone master - slave data calendar.
  Signal dom (WishboneM2S bsCal awCal) ->
  -- | Swap active calendar and shadow calendar.
  Signal dom Bool ->
  -- | Incoming frame from Bittide link.
  Signal dom (DataLink 64) ->
  -- | Wishbone master-slave port scatterUnit.
  Signal dom (WishboneM2S 4 awSU) ->
  -- | (slave - master data scatterUnit , slave - master data calendar)
  (Signal dom (WishboneS2M 4), Signal dom (WishboneS2M bsCal))
scatterUnitWb calConfig wbInCal calSwitch linkIn wbInSU =
  (delayControls wbOutSU, wbOutCal)
 where
  (wbOutSU, memAddr, _) = unbundle $ wbInterface maxBound <$> wbInSU <*> scatteredData
  (readAddr, upperSelected) = unbundle $ coerceIndexes <$> memAddr
  (scatterUnitRead, wbOutCal) = scatterUnit calConfig wbInCal calSwitch linkIn readAddr
  (upper, lower) = unbundle $ split <$> scatterUnitRead
  selected = register (errorX "scatterUnitWb: Initial selection undefined") upperSelected
  scatteredData = mux selected upper lower

-- | Wishbone addressable gatherUnit, the wishbone port can write data to this
-- memory element as if it has a 32 bit port by controlling the byte enables of the
-- gatherUnit based on the third bit.
gatherUnitWb ::
  forall dom memDepth awSU bsCal awCal .
  ( HiddenClockResetEnable dom
  , KnownNat memDepth, 1 <= memDepth
  , KnownNat awSU, 2 <= awSU) =>
  -- | Configuration for the calendar.
  CalendarConfig bsCal awCal (CalendarEntry memDepth) ->
  -- | Wishbone master - slave data calendar.
  Signal dom (WishboneM2S bsCal awCal) ->
  -- | Swap active calendar and shadow calendar.
  Signal dom Bool ->
  -- | Wishbone master-slave port gatherUnit.
  Signal dom (WishboneM2S 4 awSU) ->
  -- | (slave - master data gatherUnit , slave - master data calendar)
  (Signal dom (DataLink 64), Signal dom (WishboneS2M 4), Signal dom (WishboneS2M bsCal))
gatherUnitWb calConfig wbInCal calSwitch wbInSU =
  (linkOut, delayControls wbOutSU, wbOutCal)
 where
  (wbOutSU, memAddr, writeOp) = unbundle $ wbInterface maxBound <$> wbInSU <*> pure 0b0
  (writeAddr, upperSelected) = unbundle $ coerceIndexes <$> memAddr
  (linkOut, wbOutCal) =
    gatherUnit calConfig wbInCal calSwitch gatherWrite gatherByteEnables
  gatherWrite = mkWrite <$> writeAddr <*> writeOp
  gatherByteEnables = mkEnables <$> upperSelected <*> (busSelect <$> wbInSU)

  mkWrite address (Just write) = Just (address, write ++# write)
  mkWrite _ _ = Nothing
  mkEnables selected byteEnables
    | selected  = byteEnables ++# 0b0
    | otherwise = 0b0 ++# byteEnables

-- | Coerces an index of size (n*2) to index n with the lower bit as separate boolean.
coerceIndexes :: forall n . (KnownNat n, 1 <= n) => (Index (n*2) -> (Index n, Bool))
coerceIndexes = case sameNat natA natB of
  Just Refl -> bitCoerce
  _ -> error "gatherUnitWb: Index coercion failed."
  where
  natA = Proxy @(CLog 2 (n*2))
  natB = Proxy @(1 + (CLog 2 n))

-- | Delays the output controls to align them with the actual read / write timing.
delayControls :: HiddenClockResetEnable dom =>
  Signal dom (WishboneS2M bytes) -> Signal dom (WishboneS2M bytes)
delayControls wbIn = wbOut
 where
   delayedAck = register False (acknowledge <$> wbIn)
   delayedErr = register False (err <$> wbIn)
   wbOut = (\wb newAck newErr-> wb{acknowledge = newAck, err = newErr})
    <$> wbIn <*> delayedAck <*> delayedErr
