{-|
Copyright:           Copyright © 2022, Google LLC
License:             Apache-2.0
Maintainer:          devops@qbaylogic.com
|-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE MagicHash #-}
module Bittide.ScatterGather(gatherSequential, scatterSequential, scatterGatherEngine) where
import Clash.Prelude
import Data.Maybe

-- | gatherSequential is a memory bank that allows for random writes and sequentially
-- reads data based on an internal counter that runs up to the maximum index and wraps around.
-- The initial contents are undefined and it returns the contents as valid frame using the
-- Maybe functor.
gatherSequential :: forall memDepth a dom .
  (NFDataX a, KnownNat memDepth, 1 <= memDepth, HiddenClockResetEnable dom) =>
  -- | Incoming frame from link, if it contains Just a, a will be written to the memory.
  Signal dom (Maybe a) ->
  -- | Write address, when the incoming frame contains Just a, a will be written to this address.
  Signal dom (Index memDepth) ->
  -- | Outgoing frame
  Signal dom (Maybe a)
gatherSequential frameIn writeAddr = mux outFrameValid (Just <$> ramOut) (pure Nothing)
  where
    readAddr      = register 0 $ satSucc SatWrap <$> readAddr
    newFrame      = (\a f -> fmap (a,) f) <$> writeAddr <*> frameIn
    mem           = blockRam (deepErrorX "gatherSequential undefined" :: Vec memDepth a)
    ramOut        = mem readAddr newFrame
    outFrameValid = register False $ (!!) <$> frameFlags <*> readAddr
    frameFlags    = register (repeat False) . writeFlag $ readFlag frameFlags
    readFlag v    = (replace @memDepth) <$> readAddr <*> pure False <*> v
    writeFlag v   = mux (isJust <$> frameIn) ((replace @memDepth) <$> writeAddr <*> pure True <*> v) v

-- | scatterSequential is a memory bank that allows for random reads and sequentially
-- writes data based on an internal counter that runs up to the maximum index and wraps around.
-- The initial contents are undefined and it returns the contents as valid frame using the
-- Maybe functor.
scatterSequential :: forall dom memDepth a .
  (NFDataX a, KnownNat memDepth, HiddenClockResetEnable dom, 1 <= memDepth) =>
  -- | Incoming frame from link, if it contains Just a, a will be written to the memory.
  Signal dom (Maybe a) ->
  -- | Read address.
  Signal dom (Index memDepth) ->
  -- | Outgoing frame
  Signal dom (Maybe a)
scatterSequential frameIn readAddr = mux outFrameValid (Just <$> ramOut) (pure Nothing)
  where
    writeAddr     = register 0 $ satSucc SatWrap <$> readAddr
    newFrame      = (\a f -> fmap (a,) f) <$> writeAddr <*> frameIn
    mem           = blockRam (deepErrorX "scatterSequential undefined" :: Vec memDepth a)
    ramOut        = mem readAddr newFrame
    outFrameValid = register False $ (!!) <$> frameFlags <*> readAddr
    frameFlags    = register (repeat False) . writeFlag $ readFlag frameFlags
    readFlag v    = (replace @memDepth) <$> readAddr <*> pure False <*> v
    writeFlag v   = mux (isJust <$> frameIn) ((replace @memDepth) <$> writeAddr <*> pure True <*> v) v

type DataLink frameWidth = Maybe (BitVector frameWidth)
type CalendarEntry memDepth = Index memDepth
type Calendar calDepth memDepth = Vec calDepth (CalendarEntry memDepth)
type ConfigurationPort calDepth0 memDepth0 calDepth1 memDepth1 =
  (Maybe (Index calDepth0, CalendarEntry memDepth0)
  ,Maybe (Index calDepth1, CalendarEntry memDepth1))

-- | scatterGatherEngine is a 4 port memory component that enables gathering and scattering for the processing element.
-- Scattering and gathering data is done using two seperate memory banks with each their own calendar,
-- the calendars dictate the read and write address on the switch side for the gather and scatter memory respectively.
-- If the read address for the scatter engine is 0, a null frame (Nothing) will be sent to the switch.
scatterGatherEngine :: forall calDepthG calDepthS memDepthG memDepthS frameWidth dom .
  (KnownNat calDepthS, KnownNat calDepthG, KnownNat memDepthS, KnownNat memDepthG, KnownNat frameWidth,
  1 <= calDepthG , 1 <= calDepthS, 1 <= memDepthG, 1 <= memDepthS, HiddenClockResetEnable dom) =>
  -- | Bootstrap calendar gather memory.
  Calendar calDepthG memDepthG ->
  -- | Bootstrap calendar scatter memory.
  Calendar calDepthS memDepthS ->
  -- | Configuration port for the scatter and gather calendars.
  Signal dom (ConfigurationPort calDepthG memDepthG calDepthS memDepthS) ->
  -- | Incoming frame from the switch
  Signal dom (DataLink frameWidth) ->
  -- | Incoming data from the PE.
  Signal dom (BitVector frameWidth) ->
  -- | Write address of the switch frame.
  Signal dom (Index memDepthG) ->
  -- | Read address for the switch link.
  Signal dom (Index memDepthS) ->
  -- | Tuple containing the frame read by the switch and the data read by the PE.
  Signal dom (DataLink frameWidth, BitVector frameWidth)
scatterGatherEngine bootstrapCalG bootstrapCalS configPort
 frameInSwitch frameInPE readAddrPE writeAddrPE = bundle (toSwitch, toPE)
  where
    calendarG = calendar bootstrapCalG calendarCounterG gatherConfig
    calendarCounterG = register (0 :: (Index calDepthG)) $ satSucc SatWrap <$> calendarCounterG
    gatherMem = blockRam (deepErrorX "s/g gather Initial." :: Vec memDepthG (BitVector frameWidth))
    gatherOut = gatherMem readAddrPE $ (\a f -> fmap (a,) f) <$> calendarG <*> frameInSwitch
    toPE      = gatherOut

    scatterMem = blockRam (deepErrorX "s/g scatter Initial." :: Vec memDepthS (BitVector frameWidth))
    scatterOut = scatterMem calendarS $ curry Just <$> writeAddrPE <*> frameInPE
    calendarS = calendar bootstrapCalS calendarCounterS scatterConfig
    calendarCounterS = register (0 :: (Index calDepthS)) $ satSucc SatWrap <$> calendarCounterS
    toSwitch  = mux (fmap (==0) calendarS) (pure Nothing) $ Just <$> scatterOut

    (gatherConfig, scatterConfig) = unbundle configPort

calendar ::
  (KnownNat calDepth, KnownNat memDepth, 1 <= calDepth, 1 <= memDepth, HiddenClockResetEnable dom) =>
  -- | Bootstrap calendar.
  Calendar calDepth memDepth ->
  -- | Calendar read address.
  Signal dom (Index calDepth) ->
  -- | Configuration port.
  Signal dom (Maybe (Index calDepth, CalendarEntry memDepth)) ->
  -- | Current calendar entry.
  Signal dom (CalendarEntry memDepth)
calendar = blockRam
