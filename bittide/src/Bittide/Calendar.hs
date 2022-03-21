{-|
Copyright:           Copyright © 2022, Google LLC
License:             Apache-2.0
Maintainer:          devops@qbaylogic.com
|-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=8 #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Bittide.Calendar(calendar, wbCalRX) where

import Clash.Prelude

import Bittide.DoubleBufferedRAM
import Bittide.SharedTypes
import Contranomy.Wishbone

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
  (Signal dom a, Signal dom Bool)
calendar bootStrapCal shadowSwitch writeEntry = (entryOut, newMetaCycle)
  where
    firstCycle = register True $ pure False
    entryOut = mux firstCycle (pure $ bootStrapCal !! (0 :: Integer)) readEntry
    readEntry = doubleBufferedRAM bootStrapCal shadowSwitch counter' writeEntry
    counter = register (0 :: (Index calDepth)) counter'
    counter' = satSucc SatWrap <$> counter
    newMetaCycle = fmap not firstCycle .&&. (==0) <$> counter

-- | State of the calendar RX hardware, contains registers to store a new entry and
-- the shadow read address.
data WishboneRXState regSize calEntry calDepth = WishboneRXState
  { calStRegisters :: RegisterBank regSize calEntry
    -- ^ Write entry for the shadow calendar
  , calStReadAddr  :: RegisterBank regSize (Index calDepth)
    -- ^ Read address for the shadow calendar.
  } deriving (Generic)

instance (AtLeastOne regSize, KnownNat calDepth, Paddable calEntry, 1 <= CLog 2 calDepth, 1 <= calDepth) => NFDataX (WishboneRXState regSize calEntry calDepth)

-- | Control signals produced by the wishbone RX hardware for the calendar.
data CalendarControl calDepth calEntry bytes = CalendarControl
  { newShadowDepth :: Maybe (Index calDepth)
    -- ^ The size of the next calendar
  , newShadowEntry :: WriteAny calDepth calEntry
    -- ^ The next entry and its write address
  , shadowReadAddr :: Index calDepth
    -- ^ The next address to read from in the shadow calendar
  , wishboneActive :: Bool
    -- ^ Is the wishbone interface currently performing an operation
  , wishboneError :: Bool
    -- ^ Is the wishbone interface in an illegal state
  , wishboneAddress :: Index (3 + TypeRequiredRegisters calEntry (bytes * 8))
  }

-- | Interface that decodes incoming wishbone operations into useful signals for the
-- calendar.
wbCalRX
  :: forall dom calEntry calDepth aw bytes
   . ( KnownNat aw
     , KnownNat bytes
     , KnownNat calDepth
     , Paddable calEntry
     , HiddenClockResetEnable dom
     , NatFitsInBits (TypeRequiredRegisters calEntry (bytes * 8)) aw, ShowX calEntry)
  => Signal dom (WishboneM2S bytes aw)
    -- ^ Incoming wishbone signals
  -> Signal dom (CalendarControl calDepth calEntry bytes)
    -- ^ Calendar control signals.
wbCalRX = mealy go initState
 where
  initState = WishboneRXState
    { calStRegisters = deepErrorX "wbCalRX: calStRegisters undefined."
    , calStReadAddr  = deepErrorX "wbCalRX: calStReadAddr undefined."
    }

  go :: WishboneRXState (bytes * 8) calEntry calDepth
     -> WishboneM2S bytes aw
     -> ( WishboneRXState (bytes * 8) calEntry calDepth
        , CalendarControl calDepth calEntry bytes
        )
  go wbState@WishboneRXState{..} WishboneM2S{..} = (wbState', calControl)
   where
    calEntryRegs = fromSNat $ SNat @(TypeRequiredRegisters calEntry (bytes * 8))

    wbAddrValid = addr < (3 + resize (pack calEntryRegs))
    wbAddr = (paddedToData . bvAsPadded) addr
    wbActive = busCycle && strobe
    wbError  = wbActive && not wbAddrValid
    wbWriting = wbActive && writeEnable && not wbError
    wbNewCalEntry = wbWriting && wbAddr < calEntryRegs
    wbShadowWriteAddr = wbWriting && wbAddr == calEntryRegs
    wbNewShadowRdAddr = wbWriting && wbAddr == (calEntryRegs + 1)
    wbNewShadowDepth = wbWriting && wbAddr == (calEntryRegs + 2)

    wbState' = wbState{ calStRegisters = newPartialCalEntry
                        , calStReadAddr = newShadowReadAddr}

    newPartialCalEntry =
      if wbNewCalEntry
      then updateRegisters wbAddr calStRegisters
      else calStRegisters
    newShadowReadAddr =
      if wbNewShadowRdAddr
      then updateRegisters (0 :: Integer) calStReadAddr
      else calStReadAddr

    updateRegisters :: forall i a.
      (Enum i) =>
      i ->
      RegisterBank (bytes*8) a->
      RegisterBank (bytes*8) a
    updateRegisters i = updateRegBank i busSelect writeData

    calAddr = paddedToData $ bvAsPadded writeData

    newShadowDepth =
      if wbNewShadowDepth
      then  Just calAddr
      else Nothing

    newShadowEntry =
      if wbShadowWriteAddr
      then Just (calAddr , registersToData @_ @(bytes * 8) calStRegisters)
      else Nothing

    calControl = CalendarControl
      { newShadowDepth = newShadowDepth
      , newShadowEntry = newShadowEntry
      , shadowReadAddr = registersToData @_ @(bytes * 8) calStReadAddr
      , wishboneActive = wbActive
      , wishboneError = wbError
      , wishboneAddress = wbAddr
      }

updateRegBank :: (Enum i, KnownNat bytes) => i -> ByteEnable bytes -> BitVector (bytes * 8)  -> RegisterBank (bytes * 8) a-> RegisterBank (bytes * 8) a
updateRegBank i byteSelect newBV (RegisterBank p@SNat r@SNat vec) = RegisterBank p r newVec
 where
   newVec = replace i (regUpdate byteSelect (vec !! i) newBV) vec

regUpdate :: (KnownNat bytes) => ByteEnable bytes-> BitVector (bytes * 8) -> BitVector (bytes * 8) -> BitVector (bytes * 8)
regUpdate byteEnable oldEntry newEntry = bitCoerce $ (\e (o, n :: BitVector 8) -> if e then n else o) <$>
  bitCoerce byteEnable <*> zip (bitCoerce oldEntry) (bitCoerce newEntry)
