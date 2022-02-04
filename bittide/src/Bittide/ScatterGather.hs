{-|
Copyright:           Copyright © 2022, Google LLC
License:             Apache-2.0
Maintainer:          devops@qbaylogic.com
|-}
{-# LANGUAGE PartialTypeSignatures #-}
module Bittide.ScatterGather(scatterEngine, gatherEngine, scatterGatherEngine) where
import Clash.Prelude
type DataLink frameWidth = Maybe (BitVector frameWidth)
type CalendarEntry memDepth = Index memDepth
type Calendar calDepth memDepth = Vec calDepth (CalendarEntry memDepth)
type ConfigurationPort calDepth0 memDepth0 calDepth1 memDepth1 =
  (Maybe (Index calDepth0, CalendarEntry memDepth0)
  ,Maybe (Index calDepth1, CalendarEntry memDepth1))

-- | The double buffered RAM component is a memory component that internally uses a single
-- blockram, but enables the user to write to one part of the ram and read from another.
-- When the metacycle indicate (the first argument) is True, the read buffer and write buffer
-- are swapped. This signal should be True for the first cycle of every metacycle.
doubleBufferedRAM :: forall dom maxIndex a .
 (NFDataX a, KnownNat maxIndex, 1 <= maxIndex, HiddenClockResetEnable dom) =>
  -- | Indicates when a new metacycle has started.
  Signal dom Bool ->
  -- | Read address.
  Signal dom (Index maxIndex) ->
  -- | Write address.
  Signal dom (Index maxIndex) ->
  -- | Incoming data frame.
  Signal dom (Maybe a) ->
  -- | Outgoing data
  Signal dom a
doubleBufferedRAM switch readAddr writeAddr frameIn = ramOut
  where
    selectReg     = register @dom False writeSelect
    writeSelect   = mux switch (not <$> selectReg) selectReg
    readSelect    = not <$> writeSelect
    extUnsigned   = zeroExtend . bitCoerce:: Index maxIndex -> Unsigned (1 + BitSize (Index maxIndex))
    bufToBit buf  = if buf then setBit else clearBit
    selBuf buf (extUnsigned -> addr') = bufToBit buf addr' (finiteBitSize addr' - 1)

    readAddr'     = selBuf <$> readSelect <*> readAddr
    writeAddr'    = selBuf <$> writeSelect <*> writeAddr

    newFrame      = (\a f -> fmap (a,) f) <$> writeAddr' <*> frameIn

    ramInit       = deepErrorX "doubleBufferedRAM undefined"
    ram           = blockRam (ramInit :: Vec (2^(1+BitSize (Index maxIndex))) a)
    ramOut        = ram readAddr' newFrame

-- | scatterEngine is a memory bank that allows for random writes and sequentially
-- reads data based on an internal counter that runs up to the maximum index and wraps around.
-- The initial contents are undefined and it returns the contents as valid frame using the
-- Maybe functor.
scatterEngine :: forall dom memDepth a .
  (NFDataX a, KnownNat memDepth, 1 <= memDepth, HiddenClockResetEnable dom) =>
  -- | Boolean signal indicating when a new metacycle has started.
  Signal dom Bool ->
  -- | Incoming frame from link, if it contains Just a, a will be written to the memory.
  Signal dom (Maybe a) ->
  -- | Write address, when the incoming frame contains Just a, a will be written to this address.
  Signal dom (Index memDepth) ->
  -- | Outgoing data
  Signal dom a
scatterEngine newMetaCycle frameIn writeAddr =
  doubleBufferedRAM newMetaCycle readAddr writeAddr frameIn
    where
      readAddr = register (0 :: Index memDepth) $ satSucc SatWrap <$> readAddr

-- | gatherEngine is a memory bank that allows for random reads and sequentially
-- writes data based on an internal counter that runs up to the maximum index and wraps around.
-- The initial contents are undefined and it returns the contents as valid frame using the
-- Maybe functor.
gatherEngine :: forall dom memDepth a .
  (NFDataX a, KnownNat memDepth, HiddenClockResetEnable dom, 1 <= memDepth) =>
  -- | Boolean signal indicating when a new metacycle has started.
  Signal dom Bool ->
  -- | Incoming frame from link, if it contains Just a, a will be written to the memory.
  Signal dom (Maybe a) ->
  -- | Read address.
  Signal dom (Index memDepth) ->
  -- | Outgoing data
  Signal dom a
gatherEngine newMetaCycle frameIn readAddr =
  doubleBufferedRAM newMetaCycle readAddr writeAddr frameIn
  where
    writeAddr = register 0 $ satSucc SatWrap <$> writeAddr

-- | scatterGatherEngine is a 4 port memory component that enables gathering and scattering for the processing element.
-- Scattering and gathering data is done using two seperate memory banks with each their own calendar,
-- the calendars dictate the read and write address on the switch side for the gather and scatter memory respectively.
-- If the read address for the scatter engine is 0, a null frame (Nothing) will be sent to the switch.
scatterGatherEngine :: forall dom calDepthG calDepthS memDepthG memDepthS frameWidth .
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
  Signal dom (DataLink frameWidth) ->
  -- | Write address of the switch frame.
  Signal dom (Index memDepthG) ->
  -- | Read address for the switch link.
  Signal dom (Index memDepthS) ->
  -- | Tuple containing the frame read by the switch and the data read by the PE.
  Signal dom (DataLink frameWidth, BitVector frameWidth)
scatterGatherEngine bootstrapCalG bootstrapCalS configPort
 frameInSwitch frameInPE readAddrPE writeAddrPE = bundle (toSwitch, toPE)
  where
    newMetaCycleG = register False $ (==0) <$> calendarCounterG
    newMetaCycleS = register False $ (==0) <$> calendarCounterS

    calendarG = calendar bootstrapCalG calendarCounterG gatherConfig
    calendarCounterG = register (0 :: (Index calDepthG)) $ satSucc SatWrap <$> calendarCounterG
    scatterOut = doubleBufferedRAM newMetaCycleG readAddrPE calendarG frameInSwitch
    toPE      = scatterOut

    calendarS = calendar bootstrapCalS calendarCounterS scatterConfig
    calendarCounterS = register (0 :: (Index calDepthS)) $ satSucc SatWrap <$> calendarCounterS
    gatherOut = doubleBufferedRAM newMetaCycleS calendarS writeAddrPE frameInPE
    toSwitch  = mux (register True $ (==0) <$> calendarS) (pure Nothing) $ Just <$> gatherOut

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
