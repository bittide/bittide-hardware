-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Contains the Bittide Calendar, which is a double buffered memory element that stores
instructions for the 'scatterUnitWb', 'gatherUnitWb' or 'switch'. Implementation is based
on the "Bittide Hardware" document.

For documentation see 'Bittide.Calendar.calendar'.
|
-}
module Bittide.Calendar (
  calendar,
  mkCalendar,
  calendarMemoryMap,
  CalendarConfig (..),
  ValidEntry (..),
  ExtraRegs,
  Calendar,
) where

import Clash.Prelude

import Data.Constraint.Nat.Extra
import Data.Constraint.Nat.Lemmas
import Data.Maybe
import Protocols.Wishbone

import Bittide.Extra.Maybe
import Bittide.SharedTypes
import Clash.Class.BitPackC
import GHC.Stack (HasCallStack)
import Protocols.MemoryMap

{-
NOTE [component calendar types]

The calendar is a component that stores a vector of configurations called its entries.
It can be used by any component that has a periodic configuration that can change every cycle.
The calendar does not do any inspection whatsoever of the entries that it holds and thus
it does not care about the type of its entries, this type depends on the component that
instantiates the calendar.
-}

{- | Tuple of calendar entry @a@ and repetition count @Unsigned repetitionBits@ that
indicates the number of times the calendar entry should be repeated:
0 = no repetition, entry is valid for one cycle.
1 = repeated once, entry is valid for two cycles.
-}
data ValidEntry a repetitionBits = ValidEntry
  { veEntry :: a
  -- ^ Calendar entry
  , veRepeat :: Unsigned repetitionBits
  -- ^ Number of times the calendar entry should be repeated:
  -- 0 = no repetition, entry is valid for one cycle.
  -- 1 = repeated once, entry is valid for two cycles.
  }
  deriving (BitPack, Eq, Generic, NFDataX, Show, ShowX)

{- | 'Vec' of 'ValidEntry's to be used by a 'calendar'. The duration of the 'Calendar' in
clockCycles is equal to the @size@ of the 'Calendar' plus the 'sum' of all 'veRepeat's
of the 'ValidEntry's.
-}
type Calendar size a repetitionBits = Vec size (ValidEntry a repetitionBits)

{- | Configuration for the calendar, This type satisfies all
relevant constraints imposed by calendar.
-}
data CalendarConfig addrW a where
  CalendarConfig ::
    ( KnownNat repetitionBits
    , 1 <= 2 ^ repetitionBits -- This can be removed after https://github.com/clash-lang/ghc-typelits-natnormalise/issues/65 has been fixed.
    , KnownNat bootstrapActive
    , 1 <= bootstrapActive
    , KnownNat bootstrapShadow
    , 1 <= bootstrapShadow
    , LessThan bootstrapActive maxCalDepth
    , LessThan bootstrapShadow maxCalDepth
    , Paddable a
    , Show a
    , ShowX a
    , 2 <= maxCalDepth
    ) =>
    -- | Maximum amount of entries that can be held per calendar.
    SNat maxCalDepth ->
    -- | Number of bits for the repetition count.
    SNat repetitionBits ->
    -- | Initial contents of the active calendar.
    Calendar bootstrapActive a repetitionBits ->
    -- | Initial contents of the inactive calendar.
    Calendar bootstrapShadow a repetitionBits ->
    CalendarConfig addrW a

-- | Standalone deriving is required because 'CalendarConfig' contains existential type variables.
deriving instance Show (CalendarConfig addrW a)

{- | Wrapper function to create a 'calendar' from the given 'CalendarConfig', this way
we prevent the constraints of the type variables used in 'calendar' from leaking into
the rest of the system.
-}
mkCalendar ::
  ( HiddenClockResetEnable dom
  , KnownNat nBytes
  , 1 <= nBytes
  , KnownNat addrW
  ) =>
  -- | Calendar configuration for 'calendar'.
  CalendarConfig addrW calEntry ->
  -- | Wishbone interface (master to slave)
  Signal dom (WishboneM2S addrW nBytes (Bytes nBytes)) ->
  -- |
  -- 1. Currently active entry
  -- 2. Metacycle indicator
  -- 3. Wishbone interface. (slave to master)
  (Signal dom calEntry, Signal dom Bool, Signal dom (WishboneS2M (Bytes nBytes)))
mkCalendar (CalendarConfig maxCalDepth SNat bsActive bsShadow) =
  calendar maxCalDepth bsActive bsShadow

calendarMemoryMap ::
  forall nBytes addrW calEntry.
  ( HasCallStack
  , KnownNat nBytes
  , 1 <= nBytes
  , KnownNat addrW
  ) =>
  String ->
  SNat nBytes ->
  -- | Calendar configuration for 'calendar'.
  CalendarConfig addrW calEntry ->
  MemoryMap
calendarMemoryMap name SNat (CalendarConfig maxCalDepth@SNat SNat _ _) =
  MemoryMap
    { tree = DeviceInstance locCaller name
    , deviceDefs = deviceSingleton (deviceDef maxCalDepth)
    }
 where
  sizeEntries :: forall maxCalDepth n. (Num n) => SNat maxCalDepth -> n
  sizeEntries SNat =
    -- natToNum @(BitSize calEntry `DivRU` 8)
    natToNum @(maxCalDepth * nBytes)

  deviceDef ::
    forall maxCalDepth. (1 <= maxCalDepth) => SNat maxCalDepth -> DeviceDefinition
  deviceDef depth@SNat =
    DeviceDefinition
      { registers =
          [ NamedLoc
              { name = Name "calendarEntries" ""
              , loc = locHere
              , value =
                  Register
                    { fieldType = regType @(Vec maxCalDepth (Bytes nBytes))
                    , address = 0
                    , access = ReadWrite
                    , reset = Nothing
                    , tags = []
                    }
              }
          , NamedLoc
              { name = Name "shadowWrite" ""
              , loc = locHere
              , value =
                  Register
                    { fieldType = regType @(Bytes nBytes)
                    , address = sizeEntries depth + (natToNum @(ByteSizeC (Bytes nBytes)) * 0)
                    , access = WriteOnly
                    , reset = Nothing
                    , tags = []
                    }
              }
          , NamedLoc
              { name = Name "shadowRead" ""
              , loc = locHere
              , value =
                  Register
                    { fieldType = regType @(Bytes nBytes)
                    , address = sizeEntries depth + (natToNum @(ByteSizeC (Bytes nBytes)) * 1)
                    , access = WriteOnly
                    , reset = Nothing
                    , tags = []
                    }
              }
          , NamedLoc
              { name = Name "shadowDepth" ""
              , loc = locHere
              , value =
                  Register
                    { fieldType = regType @(Bytes nBytes)
                    , address = sizeEntries depth + (natToNum @(ByteSizeC (Bytes nBytes)) * 2)
                    , access = ReadWrite -- TODO is this correct??
                    , reset = Nothing
                    , tags = []
                    }
              }
          , NamedLoc
              { name = Name "calendarSwap" ""
              , loc = locHere
              , value =
                  Register
                    { fieldType = regType @Bool
                    , address = sizeEntries depth + (natToNum @(ByteSizeC (Bytes nBytes)) * 3)
                    , access = WriteOnly
                    , reset = Nothing
                    , tags = []
                    }
              }
          ]
      , deviceName = Name name ""
      , definitionLoc = locHere
      , tags = []
      }

{- | State of the calendar excluding the buffers. It stores the depths of the active and
shadow calendar, the read pointer, buffer selector and a register for first cycle behavior.
-}
data CalendarState maxCalDepth repetitionBits = CalendarState
  { firstCycle :: Bool
  -- ^ is True after reset, becomes false after first cycle.
  , selectedBuffer :: AorB
  -- ^ Indicates if buffer A or B is active.
  , entryTracker :: Index maxCalDepth
  -- ^ Read point for the active calendar.
  , repetitionCounter :: Unsigned repetitionBits
  -- ^ Counts the number of cycles that the current entry has been repeated.
  , calDepthA :: Index maxCalDepth
  -- ^ Depth of buffer A.
  , calDepthB :: Index maxCalDepth
  -- ^ Depth of buffer B.
  , swapCalendars :: Bool
  -- ^ Swaps the active and shadow calendar at the end of the metacycle.
  }
  deriving (Generic, NFDataX)

{- | Contains the current active calendar entry along with the metacycle
indicator that is provided at the output of the the calendar component. Furthermore
it contains the shadow entry and depth of the shadow calendar which are provided
to the wishbone output hardware ('wbCalTX').
-}
data CalendarOutput calDepth calEntry = CalendarOutput
  { activeEntry :: calEntry
  -- ^ Current active entry.
  , lastCycle :: Bool
  -- ^ True when the last entry of the active calendar is present at the output.
  , shadowEntry :: calEntry
  -- ^ Current shadow entry
  , shadowDepth :: Index calDepth
  -- ^ Depth of current shadow calendar.
  }

-- | Contains the read and write operations for both buffers.
data BufferControl calDepth calEntry = BufferControl
  { readA :: Index calDepth
  -- ^ Read address for buffer A.
  , writeA :: Maybe (Located calDepth calEntry)
  -- ^ Write operation for buffer B.
  , readB :: Index calDepth
  -- ^ Read address for buffer B
  , writeB :: Maybe (Located calDepth calEntry)
  -- ^ Write operation for buffer B.
  }

{-# NOINLINE calendar #-}

{- | Hardware component that stores an active bittide calendar and a shadow bittide calendar.
The entries of the active calendar will be sequentially provided at the output,
the shadow calendar can be read from and written to through the wishbone interface.
The active and shadow calendar can be swapped by setting the shadowSwitch to True.
-}
calendar ::
  forall dom nBytes addrW maxCalDepth a repetitionBits bootstrapSizeA bootstrapSizeB.
  ( HiddenClockResetEnable dom
  , KnownNat addrW
  , KnownNat bootstrapSizeA
  , 1 <= bootstrapSizeA
  , KnownNat bootstrapSizeB
  , 1 <= bootstrapSizeB
  , KnownNat nBytes
  , 1 <= nBytes
  , KnownNat repetitionBits
  , 2 <= maxCalDepth
  , LessThan bootstrapSizeA maxCalDepth
  , LessThan bootstrapSizeB maxCalDepth
  , Paddable a
  , ShowX a
  , Show a
  ) =>
  -- | The maximum amount of entries that can be stored in the individual calendars.
  SNat maxCalDepth ->
  -- | Bootstrap calendar for the active buffer.
  Calendar bootstrapSizeA a repetitionBits ->
  -- | Bootstrap calendar for the shadow buffer.
  Calendar bootstrapSizeB a repetitionBits ->
  -- | Incoming wishbone interface
  Signal dom (WishboneM2S addrW nBytes (Bytes nBytes)) ->
  -- | Currently active entry, Metacycle indicator and outgoing wishbone interface.
  (Signal dom a, Signal dom Bool, Signal dom (WishboneS2M (Bytes nBytes)))
calendar SNat bootstrapActive bootstrapShadow wbIn =
  (calOut.activeEntry.veEntry, calOut.lastCycle, wbOut)
 where
  ctrl :: Signal dom (CalendarControl maxCalDepth (ValidEntry a repetitionBits) nBytes)
  ctrl = wbCalRX wbIn
  wbOut = wbCalTX <$> ctrl <*> calOut

  -- XXX: Ideally we'd pad with 'errorX', but Vivado generates a critical warning
  --      on undefined initial contents. We tried using the Clash flag
  --      `-fclash-force-undefined=0`, but this triggered a bug:
  --
  --        https://github.com/clash-lang/clash-compiler/issues/2360
  --
  bootstrapA =
    bootstrapActive
      ++ repeat @(maxCalDepth - bootstrapSizeA)
        ValidEntry{veEntry = unpack 0, veRepeat = 0}
  bootstrapB =
    bootstrapShadow
      ++ repeat @(maxCalDepth - bootstrapSizeA)
        ValidEntry{veEntry = unpack 0, veRepeat = 0}

  bufA = blockRam bootstrapA bufCtrl.readA bufCtrl.writeA
  bufB = blockRam bootstrapB bufCtrl.readB bufCtrl.writeB

  (bufCtrl, calOut) = mealyB go initState (ctrl, bufA, bufB)

  -- We can safely derive the initial calDepths from the bootStrap sizes because
  -- we have the calDepth <= bootstrapSize constraints. Furthermore using resize
  -- does not require additional constraints.
  initState =
    CalendarState
      { firstCycle = True
      , selectedBuffer = A
      , entryTracker = 0
      , repetitionCounter = 0
      , calDepthA = resize (maxBound :: Index bootstrapSizeA)
      , calDepthB = resize (maxBound :: Index bootstrapSizeB)
      , swapCalendars = False
      }

  go ::
    CalendarState maxCalDepth repetitionBits ->
    ( CalendarControl maxCalDepth (ValidEntry a repetitionBits) nBytes
    , ValidEntry a repetitionBits
    , ValidEntry a repetitionBits
    ) ->
    ( CalendarState maxCalDepth repetitionBits
    , ( BufferControl maxCalDepth (ValidEntry a repetitionBits)
      , CalendarOutput maxCalDepth (ValidEntry a repetitionBits)
      )
    )
  go CalendarState{..} (CalendarControl{..}, bufAIn, bufBIn) =
    (calState, (bufCtrl1, calOut1))
   where
    selectedBuffer1
      | swapCalendars && lastCycle = swapAorB selectedBuffer
      | otherwise = selectedBuffer

    lastCycle = not entryStillValid && entryTracker == activeDepth

    entryStillValid = repetitionCounter < veRepeat

    entryTracker1
      | entryStillValid = entryTracker
      | not lastCycle = satSucc SatWrap entryTracker
      | otherwise = 0

    repetitionCounter1
      | entryStillValid = satSucc SatWrap repetitionCounter
      | otherwise = 0

    (activeEntry1, shadowEntry)
      | A <- selectedBuffer = (bufAIn, bufBIn)
      | B <- selectedBuffer = (bufBIn, bufAIn)

    (activeDepth, shadowDepth)
      | A <- selectedBuffer = (calDepthA, calDepthB)
      | B <- selectedBuffer = (calDepthB, calDepthA)

    (calDepthA1, calDepthB1) =
      case (selectedBuffer, newShadowDepth) of
        (A, Just newDepthB) -> (calDepthA, newDepthB)
        (B, Just newDepthA) -> (newDepthA, calDepthB)
        _ -> (calDepthA, calDepthB)

    (readA, writeA, readB, writeB) =
      case (selectedBuffer1, isJust newShadowEntry) of
        (A, True) -> (entryTracker1, Nothing, shadowReadAddr, newShadowEntry)
        (A, _) -> (entryTracker1, Nothing, shadowReadAddr, Nothing)
        (B, True) -> (shadowReadAddr, newShadowEntry, entryTracker1, Nothing)
        (B, _) -> (shadowReadAddr, Nothing, entryTracker1, Nothing)

    activeEntry@(ValidEntry{..})
      | firstCycle = bootstrapA !! (0 :: Index 1)
      | otherwise = activeEntry1

    bufCtrl1 = BufferControl{readA, writeA, readB, writeB}
    calOut1 = CalendarOutput{activeEntry, lastCycle, shadowEntry, shadowDepth}
    calState =
      CalendarState
        { firstCycle = False
        , selectedBuffer = selectedBuffer1
        , entryTracker = entryTracker1
        , repetitionCounter = repetitionCounter1
        , calDepthA = calDepthA1
        , calDepthB = calDepthB1
        , swapCalendars = armCalendarSwap || (not lastCycle && swapCalendars)
        }

{- | State of the calendar RX hardware, contains registers to store a new entry and
the shadow read address.
-}
data WishboneRXState regSize calEntry calDepth = WishboneRXState
  { calStRegisters :: RegisterBank regSize calEntry 'LittleEndian
  -- ^ Write entry for the shadow calendar
  , calStReadAddr :: RegisterBank regSize (Index calDepth) 'LittleEndian
  -- ^ Read address for the shadow calendar.
  }
  deriving (Generic)

instance
  ( KnownNat regSize
  , 1 <= regSize
  , KnownNat calDepth
  , Paddable calEntry
  , 1 <= CLog 2 calDepth
  , 1 <= calDepth
  ) =>
  NFDataX (WishboneRXState regSize calEntry calDepth)

{- | Control signals produced by the wishbone RX hardware for the calendar.
The calendar's wishbone address space is as follows:
 * address 0 to n  -> Registers that store a calEntry, due to the polymorphic nature of
   calEntry, multiple addresses may be required to write an entry to the shadow calendar.
 * address (n + 1) -> Writing to this address writes the calEntry stored in the registers
   at address 0 to n to the shadow calendar at the location provided by writeData.
 * address (n + 2) -> Register that stores the read address for the shadow calendar.
 * address (n + 3) -> Writing to this address updates the depth (counter wrap around point)
   for the shadow calendar.
 * address (n + 4) -> Arm the calendar to swap the active and shadow calendars at the
   end of the metacycle.
-}
data CalendarControl calDepth calEntry nBytes = CalendarControl
  { newShadowDepth :: Maybe (Index calDepth)
  -- ^ The size of the next calendar
  , newShadowEntry :: Maybe (Located calDepth calEntry)
  -- ^ The next entry and its write address
  , shadowReadAddr :: Index calDepth
  -- ^ The next address to read from in the shadow calendar
  , wishboneActive :: Bool
  -- ^ Is the wishbone interface currently performing an operation
  , wishboneError :: Bool
  -- ^ Is the wishbone interface in an illegal state
  , wishboneAddress :: WbAddress calEntry nBytes
  -- ^ Address for the wishbone interface.
  , armCalendarSwap :: Bool
  -- ^ Swap the active and shadow calendar at the end of the metacycle.
  }

{- | Interface that decodes incoming wishbone operations into useful signals for the
calendar.
-}
wbCalRX ::
  forall dom calEntry calDepth addrW nBytes.
  ( HiddenClockResetEnable dom
  , Paddable calEntry
  , ShowX calEntry
  , KnownNat calDepth
  , 2 <= calDepth
  , KnownNat addrW
  , KnownNat nBytes
  , 1 <= nBytes
  ) =>
  -- | Incoming wishbone signals
  Signal dom (WishboneM2S addrW nBytes (Bytes nBytes)) ->
  -- | Calendar control signals.
  Signal dom (CalendarControl calDepth calEntry nBytes)
wbCalRX = case oneLeCLog2n @calDepth of
  Dict -> mealy go initState
   where
    initState =
      WishboneRXState
        { calStRegisters = deepErrorX "wbCalRX: calStRegisters undefined."
        , calStReadAddr = deepErrorX "wbCalRX: calStReadAddr undefined."
        }

    go ::
      WishboneRXState (nBytes * 8) calEntry calDepth ->
      WishboneM2S addrW nBytes (Bytes nBytes) ->
      ( WishboneRXState (nBytes * 8) calEntry calDepth
      , CalendarControl calDepth calEntry nBytes
      )
    go wbState@WishboneRXState{..} WishboneM2S{..} = (wbState1, calControl)
     where
      calEntryRegs = natToNum @(Regs calEntry (nBytes * 8))

      wbAddrValid = addr <= (resize $ pack (maxBound :: WbAddress calEntry nBytes))
      wishboneAddress = unpack $ resize addr
      wishboneActive = busCycle && strobe
      wishboneError = wishboneActive && not wbAddrValid
      wbWriting = wishboneActive && writeEnable && not wishboneError
      wbNewCalEntry = wbWriting && wishboneAddress < calEntryRegs

      wbNewShadowWriteAddr = wbWriting && wishboneAddress == shadowWriteWbAddr
      wbNewShadowReadAddr = wbWriting && wishboneAddress == shadowReadWbAddr
      wbNewShadowDepth = wbWriting && wishboneAddress == shadowDepthWbAddr
      armCalendarSwap = wbWriting && wishboneAddress == calSwapWbAddr
      shadowReadAddr = getDataLe @(nBytes * 8) calStReadAddr
      shadowEntryData = getDataLe @(nBytes * 8) calStRegisters

      wbState1 =
        wbState
          { calStRegisters = newPartialCalEntry
          , calStReadAddr = newShadowReadAddr
          }

      newPartialCalEntry
        | wbNewCalEntry = updateRegisters wishboneAddress calStRegisters
        | otherwise = calStRegisters
      newShadowReadAddr
        | wbNewShadowReadAddr = updateRegisters (0 :: Int) calStReadAddr
        | otherwise = calStReadAddr

      updateRegisters ::
        forall i a.
        (Enum i, KnownNat (BitSize a)) =>
        i ->
        RegisterBank (nBytes * 8) a 'LittleEndian ->
        RegisterBank (nBytes * 8) a 'LittleEndian
      updateRegisters i = updateRegBank i busSelect writeData

      calAddr = bitCoerce $ resize writeData

      newShadowDepth = orNothing wbNewShadowDepth calAddr
      newShadowEntry = orNothing wbNewShadowWriteAddr (calAddr, shadowEntryData)

      calControl =
        CalendarControl
          { newShadowDepth
          , newShadowEntry
          , shadowReadAddr
          , wishboneActive
          , wishboneError
          , wishboneAddress
          , armCalendarSwap
          }

{- | Wishbone interface that drives the outgoing wishbone data based on the received
wishbone address. Can be used to read one of the following registers:
  * The shadow calendar entry register
  * The shadow calendar read address register
  * The shadow calendar depth register
-}
wbCalTX ::
  forall calDepth calEntry nBytes.
  ( Paddable (Index calDepth)
  , Paddable calEntry
  , Show calEntry
  , KnownNat nBytes
  , 1 <= nBytes
  ) =>
  CalendarControl calDepth calEntry nBytes ->
  CalendarOutput calDepth calEntry ->
  WishboneS2M (Bytes nBytes)
wbCalTX
  CalendarControl{shadowReadAddr, wishboneActive, wishboneError, wishboneAddress}
  CalendarOutput{shadowEntry, shadowDepth} = wbOut
   where
    readData =
      case (getRegsLe shadowEntry, getRegsLe shadowReadAddr, getRegsLe shadowDepth) of
        (RegisterBank entryVec, RegisterBank readAddrVec, RegisterBank depthVec) ->
          ((entryVec :< 0b0) ++ readAddrVec ++ depthVec) !! wishboneAddress
    wbOut =
      (emptyWishboneS2M @(Bytes nBytes))
        { acknowledge = wishboneActive
        , err = wishboneError
        , readData
        }

updateRegBank ::
  ( Enum i
  , KnownNat nBytes
  , 1 <= nBytes
  , KnownNat (BitSize a)
  ) =>
  i ->
  BitVector nBytes ->
  Bytes nBytes ->
  RegisterBank (nBytes * 8) a 'LittleEndian ->
  RegisterBank (nBytes * 8) a 'LittleEndian
updateRegBank i byteSelect newBV (RegisterBank vec) = RegisterBank newVec
 where
  newVec = replace i (regUpdate byteSelect (vec !! i) newBV) vec

regUpdate ::
  (KnownNat nBytes) =>
  BitVector nBytes ->
  Bytes nBytes ->
  Bytes nBytes ->
  Bytes nBytes
regUpdate byteEnable oldEntry newEntry =
  bitCoerce
    $ (\e (o, n :: BitVector 8) -> if e then n else o)
    <$> bitCoerce byteEnable
    <*> zip (bitCoerce oldEntry) (bitCoerce newEntry)

type WbAddress calEntry nBytes = Index (Regs calEntry (nBytes * 8) + ExtraRegs)
type ExtraRegs = 4

shadowWriteWbAddr :: forall n. (KnownNat n, 4 <= n) => Index n
shadowWriteWbAddr = natToNum @(n - 4)

shadowReadWbAddr :: forall n. (KnownNat n, 3 <= n) => Index n
shadowReadWbAddr = natToNum @(n - 3)

shadowDepthWbAddr :: forall n. (KnownNat n, 2 <= n) => Index n
shadowDepthWbAddr = natToNum @(n - 2)

calSwapWbAddr :: forall n. (KnownNat n, 1 <= n) => Index n
calSwapWbAddr = natToNum @(n - 1)
