{-|
Copyright:           Copyright © 2022, Google LLC
License:             Apache-2.0
Maintainer:          devops@qbaylogic.com
|-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=8 #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Bittide.Calendar(calendar, calendarWB) where

import Clash.Prelude

import Bittide.DoubleBufferedRAM
import Bittide.SharedTypes
import Contranomy.Wishbone
import Data.Maybe

{-
NOTE [component calendar types]

The calendar is a component that stores a vector of configurations called its entries.
It can be used by any component that has a periodic configuration that can change every cycle.
The calendar does not do any inspection whatsoever of the entries that it holds and thus
it does not care about the type of its entries, this type depends on the component that
instantiates the calendar.
-}

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
    entryOut = mux firstCycle (pure $ bootStrapCal !! (0 :: Int)) readEntry
    readEntry =
      doubleBufferedRAM bootStrapCal shadowSwitch counterNext writeEntry
    counter = register (0 :: (Index calDepth)) counterNext
    counterNext = satSucc SatWrap <$> counter
    newMetaCycle = fmap not firstCycle .&&. (==0) <$> counter

-- | Datastructure that contains the state of the calendar excluding the buffers.
-- It stores the depths of the active and shadow calendar, the read pointer, buffer selector
-- and a register for first cycle behaviour.
data CalendarState maxCalDepth = CalendarState
  { firstCycle      :: Bool
    -- ^ is True after reset, becomes false after first cycle.
  , selectedBuffer  :: SelectedBuffer
    -- ^ Indicates if buffer A or B is active.
  , entryTracker    :: Index maxCalDepth
    -- ^ Read point for the active calendar.
  , calDepthA       :: Index maxCalDepth
    -- ^ Depth of buffer A.
  , calDepthB       :: Index maxCalDepth
    -- ^ Depth of buffer B.
  } deriving (Generic, NFDataX)

-- | Datastructure contains the current active calendar entry along with the metacycle
-- indicator that are provided at the output of the the calendar component. Furthermore
-- it contains the shadow entry and depth of the shadow calendar which are provided
-- to the wishbone output hardware (wbCalTX).
data CalendarOutput calDepth calEntry = CalendarOutput
  { activeEntry   :: calEntry
    -- ^ Current active entry.
  , newMetaCycle  :: Bool
    -- ^ True when entry 0 of the active calendar is present at the output.
  , shadowEntry   :: calEntry
    -- ^ Current shadow entry
  , shadowDepth   :: Index calDepth
    -- ^ Depth of currenty shadow calendar.
  }

-- | Datastructure which contains the read and write operations for both buffers.
data BufferControl calDepth calEntry = BufferControl
  { readA   :: Index calDepth
    -- ^ Read address for buffer A.
  , writeA  :: WriteAny calDepth calEntry
    -- ^ Write operation for buffer B.
  , readB   :: Index calDepth
    -- ^ Read address for buffer B
  , writeB  :: WriteAny calDepth calEntry
    -- ^ Write operation for buffer B.
  }

-- | Indicates which buffer is currently active.
data SelectedBuffer = A | B deriving (Eq, Generic, NFDataX)
bufToggle :: Bool -> SelectedBuffer -> SelectedBuffer
bufToggle tog bufs = if tog then switchBufs bufs else bufs
switchBufs :: SelectedBuffer -> SelectedBuffer
switchBufs A = B
switchBufs B = A

-- | Hardware component that stores an active bittide calendar and a shadow bittide calendar.
-- The entries of the active calendar will be sequantially provided at the output,
-- the shadow calendar can be read from and written to through the wishbone interface.
calendarWB ::
  forall dom bytes aw maxCalDepth calEntry bootstrapSizeA bootstrapSizeB .
  ( HiddenClockResetEnable dom
  , KnownNat bytes
  , KnownNat aw
  , LessThan bootstrapSizeA maxCalDepth
  , LessThan bootstrapSizeB maxCalDepth
  , Paddable calEntry
  , NatFitsInBits (TypeRequiredRegisters calEntry (bytes * 8)) aw
  , ShowX calEntry
  , Show calEntry) =>
  SNat maxCalDepth
  -- ^ The maximum amount of entries that can be stored in the individual calendars.
  -> Vec bootstrapSizeA calEntry
  -- ^ Bootstrap calendar for the active buffer.
  -> Vec bootstrapSizeB calEntry
  -- ^ Bootstrap calendar for the shadow buffer.
  -> Signal dom Bool
  -- ^ Signal that swaps the active and shadow calendar. (1 cycle delay)
  -> Signal dom (WishboneM2S bytes aw)
  -- ^ Incoming wishbone interface
  -> Signal dom (calEntry, Bool)
  -- ^ Currently active entry, Metacycle indicator and outgoing wishbone interface.
calendarWB SNat bootstrapActive bootstrapShadow shadowSwitch wbIn = bundle
  (activeEntry <$> calOut, newMetaCycle <$> calOut)
 where
  ctrl :: Signal dom (CalendarControl maxCalDepth calEntry bytes)
  ctrl = wbCalRX wbIn

  bootstrapA = bootstrapActive ++ repeat @(maxCalDepth - bootstrapSizeA) (errorX "Uninitialised active entry")
  bootstrapB = bootstrapShadow ++ repeat @(maxCalDepth - bootstrapSizeB) (errorX "Uninitialised shadow entry")

  bufA = blockRam bootstrapA (readA <$> bufCtrl) (writeA <$> bufCtrl)
  bufB = blockRam bootstrapB  (readB <$> bufCtrl) (writeB <$> bufCtrl)

  (bufCtrl, calOut) = unbundle $
   mealy go initState $ bundle (shadowSwitch, ctrl, bufA, bufB)

  initState = CalendarState
    { firstCycle     = True
    , selectedBuffer = A
    , entryTracker   = 0
    , calDepthA      = fromSNat $ SNat @(bootstrapSizeA -1)
    , calDepthB      = fromSNat $ SNat @(bootstrapSizeB -1)
    }

  go :: CalendarState maxCalDepth ->
    (Bool, CalendarControl maxCalDepth calEntry bytes, calEntry, calEntry) ->
    (CalendarState maxCalDepth, (BufferControl maxCalDepth calEntry, CalendarOutput maxCalDepth calEntry))
  go CalendarState{ .. } (bufSwitch, CalendarControl{..}, bufAIn, bufBIn) = (wbState', (bufCtrl', calOut'))
   where
    selectedBuffer' = bufToggle bufSwitch selectedBuffer
    (activeEntry', shadowEntry) =
      if selectedBuffer == A then (bufAIn, bufBIn) else (bufBIn, bufAIn)
    (activeDepth, shadowDepth) =
      if selectedBuffer == A then (calDepthA, calDepthB) else (calDepthB, calDepthA)
    (calDepthA', calDepthB') =
      case (selectedBuffer, newShadowDepth) of
        (A, Just newDepthB)   -> (calDepthA, newDepthB)
        (B, Just newDepthA)   -> (newDepthA, calDepthB)
        _                     -> (calDepthA, calDepthB)
    (readA, writeA, readB, writeB) =
      case (selectedBuffer', isJust newShadowEntry) of
        (A, True) -> (entryTracker' , Nothing       , shadowReadAddr, newShadowEntry)
        (A, _   ) -> (entryTracker' , Nothing       , shadowReadAddr, Nothing)
        (B, True) -> (shadowReadAddr, newShadowEntry, entryTracker' , Nothing)
        (B, _   ) -> (shadowReadAddr, Nothing       , entryTracker' , Nothing)

    entryTracker' = if entryTracker < activeDepth then satSucc SatWrap entryTracker else 0
    newMetaCycle = entryTracker == 0
    activeEntry = if firstCycle then bootstrapA !! (0 :: Integer)  else activeEntry'

    bufCtrl' = BufferControl
      { readA
      , writeA
      , readB
      , writeB}
    calOut' = CalendarOutput{activeEntry, newMetaCycle, shadowEntry, shadowDepth}
    wbState' = CalendarState
      { firstCycle      = False
      , selectedBuffer  = selectedBuffer'
      , entryTracker    = entryTracker'
      , calDepthA       = calDepthA'
      , calDepthB       = calDepthB'
      }

-- | State of the calendar RX hardware, contains registers to store a new entry and
-- the shadow read address.
data WishboneRXState regSize calEntry calDepth = WishboneRXState
  { calStRegisters :: RegisterBank regSize calEntry
    -- ^ Write entry for the shadow calendar
  , calStReadAddr  :: RegisterBank regSize (Index calDepth)
    -- ^ Read address for the shadow calendar.
  } deriving (Generic)

instance ( KnownNat regSize
         , 1 <= regSize
         , KnownNat calDepth
         , Paddable calEntry
         , 1 <= CLog 2 calDepth
         , 1 <= calDepth) =>
         NFDataX (WishboneRXState regSize calEntry calDepth)

-- | Control signals produced by the wishbone RX hardware for the calendar.
-- The calendar's wishbone address space is as follows:
--  * address 0 to n  -> Registers that store a calEntry, due to the polymorphic nature of
--    calEntry, multiple addresses may be required to write an entry to the shadow calendar.
--  * address (n + 1) -> Writing to this address writes the calEntry stored in the registers
--    at address 0 to n to the shadow calendar at the location provided by writeData.
--  * address (n + 2) -> Register that stores the read address for the shadow calendar.
--  * address (n + 3) -> Writing to this address updates the depth (counter wrap around point)
--    for the shadow calendar.
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
    -- ^ Address for the wishbone interface.
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
  go wbState@WishboneRXState{..} WishboneM2S{..} = (wbState1, calControl)
   where
    calEntryRegs = natToNum @(TypeRequiredRegisters calEntry (bytes * 8))

    wbAddrValid = addr < (3 + resize (pack calEntryRegs))
    wbAddr = (paddedToData . bvAsPadded) addr
    wbActive = busCycle && strobe
    wbError  = wbActive && not wbAddrValid
    wbWriting = wbActive && writeEnable && not wbError
    wbNewCalEntry = wbWriting && wbAddr < calEntryRegs

    wbNewShadowWriteAddr = wbWriting && wbAddr == shadowWriteWbAddr
    wbNewShadowReadAddr = wbWriting && wbAddr == shadowReadWbAddr
    wbNewShadowDepth = wbWriting && wbAddr == shadowDepthWbAddr

    wbState1 = wbState
      { calStRegisters = newPartialCalEntry
      , calStReadAddr = newShadowReadAddr}

    newPartialCalEntry
      | wbNewCalEntry = updateRegisters wbAddr calStRegisters
      | otherwise     = calStRegisters
    newShadowReadAddr
      | wbNewShadowReadAddr = updateRegisters (0 :: Int) calStReadAddr
      | otherwise           = calStReadAddr

    updateRegisters :: forall i a.
      (Enum i, KnownNat (BitSize a)) =>
      i ->
      RegisterBank (bytes*8) a->
      RegisterBank (bytes*8) a
    updateRegisters i = updateRegBank i busSelect writeData

    calAddr = paddedToData $ bvAsPadded writeData

    newShadowDepth
      | wbNewShadowDepth = Just calAddr
      | otherwise        = Nothing

    newShadowEntry
      | wbNewShadowWriteAddr = Just (calAddr , registersToData @_ @(bytes * 8) calStRegisters)
      | otherwise            = Nothing

    calControl = CalendarControl
      { newShadowDepth = newShadowDepth
      , newShadowEntry = newShadowEntry
      , shadowReadAddr = registersToData @_ @(bytes * 8) calStReadAddr
      , wishboneActive = wbActive
      , wishboneError = wbError
      , wishboneAddress = wbAddr
      }

updateRegBank ::
  ( Enum i
  , KnownNat bytes
  , 1 <= bytes
  , KnownNat (BitSize a)) =>
  i ->
  ByteEnable bytes ->
  BitVector (bytes * 8) ->
  RegisterBank (bytes * 8) a ->
  RegisterBank (bytes * 8) a
updateRegBank i byteSelect newBV (RegisterBank vec) = RegisterBank newVec
 where
   newVec = replace i (regUpdate byteSelect (vec !! i) newBV) vec

regUpdate ::
  KnownNat bytes =>
  ByteEnable bytes->
  BitVector (bytes * 8) ->
  BitVector (bytes * 8) ->
  BitVector (bytes * 8)
regUpdate byteEnable oldEntry newEntry =
  bitCoerce $ (\e (o, n :: BitVector 8) -> if e then n else o) <$>
    bitCoerce byteEnable <*> zip (bitCoerce oldEntry) (bitCoerce newEntry)

shadowWriteWbAddr :: forall n . (KnownNat n, 3 <=n) => Index n
shadowWriteWbAddr = natToNum @(n - 3)

shadowReadWbAddr :: forall n . (KnownNat n,  2 <= n) => Index n
shadowReadWbAddr = natToNum @(n - 2)

shadowDepthWbAddr :: forall n . (KnownNat n, 1 <= n) => Index n
shadowDepthWbAddr = natToNum @(n - 1)
