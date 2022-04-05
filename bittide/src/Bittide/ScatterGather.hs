{-|
Copyright:           Copyright © 2022, Google LLC
License:             Apache-2.0
Maintainer:          devops@qbaylogic.com
|-}
module Bittide.ScatterGather(scatterEngine, gatherEngine, scatterGatherEngine) where

import Clash.Prelude

import Bittide.Calendar
import Bittide.DoubleBufferedRAM ( doubleBufferedRAM )

type DataLink frameWidth = Maybe (BitVector frameWidth)
type CalendarEntry memDepth = Index memDepth
type Calendar calDepth memDepth = Vec calDepth (CalendarEntry memDepth)
type ConfigurationPort calDepth memDepth = Maybe (Index calDepth, CalendarEntry memDepth)


-- | scatterEngine is a memory bank that allows for random writes and sequentially
-- reads data based on an internal counter that runs up to the maximum index and wraps around.
-- The initial contents are undefined and it returns the contents as valid frame using the
-- Maybe functor.
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
      readAddr = register (0 :: Index memDepth) $ satSucc SatWrap <$> readAddr
      writeFrame = combineFrameWithAddr frameIn writeAddr

-- | gatherEngine is a memory bank that allows for random reads and sequentially
-- writes data based on an internal counter that runs up to the maximum index and wraps around.
-- The initial contents are undefined and it returns the contents as valid frame using the
-- Maybe functor.
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
  -- | Tuple containing the frame read by the switch and the data read by the PE.
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
