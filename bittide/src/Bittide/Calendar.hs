{-|
Copyright:           Copyright © 2022, Google LLC
License:             Apache-2.0
Maintainer:          devops@qbaylogic.com
|-}
module Bittide.Calendar where
import Clash.Prelude
import Bittide.DoubleBufferedRAM

-- | The calendar component is a double buffered memory component that sequentially reads
-- entries from one buffer and offers a write interface to the other buffer. The buffers can
-- be swapped by setting the shadow switch to high. Furthermore it returns a signal that
-- indicates when the first entry of the active buffer is present at the output.
calendar ::
  forall dom calDepth a .
  (KnownNat calDepth, 1 <= calDepth, HiddenClockResetEnable dom, NFDataX a) =>
  -- | Bootstrap calendar
  Vec calDepth a ->
  -- | Switch that swaps the active and shadow calendar.
  Signal dom Bool ->
  -- | New entry for the calendar.
  Signal dom (Maybe (Index calDepth, a)) ->
  -- | Active calendar entry and signal that indicates the start of a new metacycle.
  Signal dom (a, Bool)
calendar bootStrapCal shadowSwitch writeEntry = bundle (entryOut, newMetaCycle)
  where
    firstCycle = register True $ pure False
    entryOut = mux firstCycle (pure $ bootStrapCal !! (0 :: Integer)) readEntry
    readEntry = doubleBufferedRAM bootStrapCal shadowSwitch counter' writeEntry
    counter = register (0 :: (Index calDepth)) counter'
    counter' = satSucc SatWrap <$> counter
    newMetaCycle = fmap not firstCycle .&&. (==0) <$> counter
