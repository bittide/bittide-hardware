-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module Bittide.ScatterGather(scatterEngine, gatherEngine, scatterGatherEngine, scatterUnitWB, gatherUnitWB) where

import Clash.Prelude

import Contranomy.Wishbone
import Data.Type.Equality ((:~:)(Refl))
import Data.Proxy

import Bittide.Calendar
import Bittide.DoubleBufferedRAM
import Bittide.SharedTypes

-- | Contains a calendar entry that can be used by a scatter or gather engine.
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
  doubleBufferedRAM (deepErrorX "scatterEngine undefined") newMetaCycle readAddr writeFrame
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
  doubleBufferedRAM (deepErrorX "gatherEngine undefined") newMetaCycle readAddr writeFrame
  where
    writeAddr = register 0 $ satSucc SatWrap <$> writeAddr
    writeFrame = combineFrameWithAddr frameIn writeAddr

combineFrameWithAddr :: Signal dom (Maybe dat) -> Signal dom addr -> Signal dom (Maybe (addr,dat))
combineFrameWithAddr frameIn writeAddr = combine <$> frameIn <*> writeAddr
  where
    combine :: Maybe dat -> addr -> Maybe (addr,dat)
    combine frame addr = fmap (addr,) frame

-- | scatterGatherEngine is a 4 port memory component that enables gathering and scattering for the processing element.
-- Scattering and gathering data is done using two seperate memory banks with each their own calendar,
-- the writeAddrSwitch dictate the read and write address on the switch side for the gather and scatter memory respectively.
-- If the read address for the scatter engine is 0, a null frame (Nothing) will be sent to the switch.
scatterGatherEngine ::
  forall dom calDepthG calDepthS memDepthG memDepthS frameWidth .
  (HiddenClockResetEnable dom, KnownNat calDepthG, KnownNat calDepthS, KnownNat memDepthG, KnownNat memDepthS, KnownNat frameWidth,
  1 <= calDepthG , 1 <= calDepthS, 1 <= memDepthG, 1 <= memDepthS) =>
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
    scatterOut = doubleBufferedRAM (deepErrorX "scatterOut undefined")
      newMetaCycleS readAddrPE writeFrameSwitch
    toPE      = scatterOut

    -- Gather engine
    frameInPE' = mux ((==0) <$> writeAddrPE) (pure Nothing) frameInPE
    (readAddrSwitch, newMetaCycleG) = calendar bootCalG (pure False) gatherConfig
    writeFramePE = (\f a -> fmap (a,) f) <$> frameInPE' <*> writeAddrPE
    gatherOut = doubleBufferedRAM (deepErrorX "gatherOut undefined")
      newMetaCycleG readAddrSwitch writeFramePE
    toSwitch  = mux (register True $ (==0) <$> readAddrSwitch) (pure Nothing) $ Just <$> gatherOut

scatterUnit ::
  (HiddenClockResetEnable dom, KnownNat memDepth, KnownNat frameWidth) =>
  Vec memDepth (BitVector frameWidth) ->
  CalendarConfig bytes addressWidth (CalendarEntry memDepth) ->
  Signal dom (WishboneM2S bytes addressWidth) ->
  Signal dom Bool ->
  Signal dom (DataLink frameWidth) ->
  Signal dom (Index memDepth) ->
  (Signal dom (BitVector frameWidth), Signal dom (WishboneS2M bytes))
scatterUnit initMem calConfig wbIn calSwitch linkIn readAddr = (readOut, wbOut)
 where
  (writeAddr, metaCycle, wbOut) = mkCalendar calConfig calSwitch wbIn
  writeOp = (\a b -> (a,) <$> b) <$> writeAddr <*> linkIn
  readOut = doubleBufferedRAM initMem metaCycle readAddr writeOp

gatherUnit ::
  ( HiddenClockResetEnable dom
  , KnownNat memDepth
  , KnownNat frameWidth
  , 1 <= frameWidth
  , KnownNat (DivRU frameWidth 8)
  , 1 <= (DivRU frameWidth 8)) =>
  Vec memDepth (BitVector frameWidth) ->
  CalendarConfig bytes addressWidth (CalendarEntry memDepth) ->
  Signal dom (WishboneM2S bytes addressWidth) ->
  Signal dom Bool ->
  Signal dom (WriteBits memDepth frameWidth) ->
  Signal dom (ByteEnable (DivRU frameWidth 8)) ->
  (Signal dom (DataLink frameWidth), Signal dom (WishboneS2M bytes))
gatherUnit initMem calConfig wbIn calSwitch writeOp byteEnables= (linkOut, wbOut)
 where
  (readAddr, metaCycle, wbOut) = mkCalendar calConfig calSwitch wbIn
  linkOut = mux ((==0) <$> readAddr) (pure Nothing) $ Just <$> bramOut
  bramOut = doubleBufferedRAMByteAddressable initMem metaCycle readAddr writeOp byteEnables

wbInterface ::
  forall bytes addressWidth addresses .
  (KnownNat addresses, 1 <= addresses, KnownNat addressWidth, 2 <= addressWidth) =>
  Index addresses ->
  WishboneM2S bytes addressWidth ->
  Bytes bytes ->
  (WishboneS2M bytes, Index addresses, Maybe (Bytes bytes))
wbInterface addressRange WishboneM2S{..} readData = (WishboneS2M{readData, acknowledge, err}, memAddr, writeOp)
 where
  (alignedAddress, alignment) = split @_ @(addressWidth - 2) @2 addr
  wordAligned = alignment == (0 :: BitVector 2)
  err = (alignedAddress > resize (pack addressRange)) || not wordAligned
  acknowledge = not err && strobe
  wbAddr = unpack . resize $ pack alignedAddress
  memAddr = wbAddr
  writeOp | strobe && writeEnable && not err = Just writeData
          | otherwise  = Nothing

scatterUnitWB ::
  forall dom memDepth awSU bsCal awCal .
  (HiddenClockResetEnable dom, KnownNat memDepth, 1 <= memDepth, KnownNat awSU, 2 <= awSU) =>
  Vec memDepth (BitVector 64) ->
  CalendarConfig bsCal awCal (CalendarEntry memDepth) ->
  Signal dom (WishboneM2S bsCal awCal) ->
  Signal dom Bool ->
  Signal dom (DataLink 64) ->
  Signal dom (WishboneM2S 4 awSU) ->
  (Signal dom (WishboneS2M 4), Signal dom (WishboneS2M bsCal))
scatterUnitWB initMem calConfig wbInCal calSwitch linkIn wbInSU = (wbOutSU, wbOutCal)
 where
  (wbOutSU, memAddr, _) = unbundle $ wbInterface (maxBound :: Index memDepth) <$> wbInSU <*> scatteredData
  readAddr = (`shiftR` 1) <$> readAddr
  (scatterUnitRead, wbOutCal) = scatterUnit initMem calConfig wbInCal calSwitch linkIn readAddr
  (upper, lower) = unbundle $ split <$> scatterUnitRead
  scatteredData = mux (bitToBool . lsb <$> memAddr) upper lower

gatherUnitWB ::
  forall dom memDepth awSU bsCal awCal .
  (HiddenClockResetEnable dom, KnownNat memDepth, 1 <= memDepth, KnownNat awSU, 2 <= awSU) =>
  Vec memDepth (BitVector 64) ->
  CalendarConfig bsCal awCal (CalendarEntry memDepth) ->
  Signal dom (WishboneM2S bsCal awCal) ->
  Signal dom Bool ->
  Signal dom (WishboneM2S 4 awSU) ->
  (Signal dom (DataLink 64), Signal dom (WishboneS2M 4), Signal dom (WishboneS2M bsCal))
gatherUnitWB initMem calConfig wbInCal calSwitch wbInSU = (linkOut, wbOutSU, wbOutCal)
 where
  (wbOutSU, memAddr, writeOp) = unbundle $ wbInterface (maxBound :: Index (memDepth * 2)) <$> wbInSU <*> pure 0b0
  (writeAddr, upperSelected) = unbundle $ coerceIndexes <$> memAddr
  (linkOut, wbOutCal) = gatherUnit initMem calConfig wbInCal calSwitch gatherWrite gatherByteEnables
  gatherWrite = mkWrite <$> writeAddr <*> writeOp
  gatherByteEnables = mkEnables <$> upperSelected <*> (busSelect <$> wbInSU)
  mkWrite address (Just write) = Just (address, write ++# write)
  mkWrite _ _ = Nothing
  mkEnables selected byteEnables = if selected then byteEnables ++# 0b0 else 0b0 ++# byteEnables

  coerceIndexes :: forall n . (KnownNat n, 1 <= n) => (Index (n*2) -> (Index n, Bool))
  coerceIndexes = case sameNat natA natB of
    Just Refl -> bitCoerce
    _ -> error "gatherUnitWB: Index coercion failed."
   where
    natA = Proxy @(CLog 2 (n*2))
    natB = Proxy @(1 + CLog 2 n)
