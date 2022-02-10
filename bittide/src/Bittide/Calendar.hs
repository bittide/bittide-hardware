{-|
Copyright:           Copyright © 2022, Google LLC
License:             Apache-2.0
Maintainer:          devops@qbaylogic.com
|-}
module Bittide.Calendar where
import Clash.Prelude
import Bittide.DoubleBufferedRAM

calendar :: forall dom calDepth a . (KnownNat calDepth, 1 <= calDepth, HiddenClockResetEnable dom, NFDataX a) =>
  Vec calDepth a ->
  Signal dom Bool ->
  Signal dom (Maybe (Index calDepth, a)) ->
  Signal dom (a, Bool)
calendar bootStrapCal shadowSwitch writeEntry = bundle (entryOut, newMetaCycle)
  where
    firstCycle = register True $ pure False
    entryOut = mux firstCycle (pure $ bootStrapCal !! 0) readEntry
    readEntry = doubleBufferedRAM bootStrapCal shadowSwitch counter' writeEntry
    counter = register (0 :: (Index calDepth)) counter'
    counter' = satSucc SatWrap <$> counter
    newMetaCycle = register True $ (==0) <$> counter'
