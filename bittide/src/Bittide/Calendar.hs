-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=100 #-}

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
  mkCalendarC,
  CalendarControl (..),
  CalendarOutput (..),
  CalendarConfig (..),
  ValidEntry (..),
  Calendar,
) where

import Clash.Prelude

import Bittide.Extra.Maybe
import Data.Constraint.Nat.Extra
import Data.Constraint.Nat.Lemmas
import Data.Maybe
import GHC.Stack

import Bittide.SharedTypes
import Clash.Class.BitPackC
import Protocols
import Protocols.MemoryMap
import Protocols.MemoryMap.Registers.WishboneStandard (
  RegisterConfig (access, description),
  busActivityWrite,
  deviceWb,
  registerConfig,
  registerWbI,
  registerWbI_,
 )
import Protocols.MemoryMap.TypeDescription
import Protocols.Wishbone

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
  deriving (BitPack, Eq, Generic, NFDataX, Show, ShowX, BitPackC, Lift)

deriveTypeDesc ''ValidEntry

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

{-# OPAQUE calendar #-}

{- | Hardware component that stores an active bittide calendar and a shadow bittide calendar.
The entries of the active calendar will be sequentially provided at the output,
the shadow calendar can be read from and written to through the wishbone interface.
The active and shadow calendar can be swapped by setting the shadowSwitch to True.
-}
calendar ::
  forall dom maxCalDepth a repetitionBits bootstrapSizeA bootstrapSizeB.
  ( HiddenClockResetEnable dom
  , KnownNat bootstrapSizeA
  , 1 <= bootstrapSizeA
  , KnownNat bootstrapSizeB
  , 1 <= bootstrapSizeB
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
  Signal dom (CalendarControl maxCalDepth (ValidEntry a repetitionBits)) ->
  -- | Currently active entry, Metacycle indicator and outgoing wishbone interface.
  (Signal dom (CalendarOutput maxCalDepth (ValidEntry a repetitionBits)))
calendar SNat bootstrapActive bootstrapShadow ctrl = calOut
 where
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
    ( CalendarControl maxCalDepth (ValidEntry a repetitionBits)
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

data CalendarControl calDepth calEntry = CalendarControl
  { newShadowDepth :: Maybe (Index calDepth)
  -- ^ The size of the next calendar
  , newShadowEntry :: Maybe (Located calDepth calEntry)
  -- ^ The next entry and its write address
  , shadowReadAddr :: Index calDepth
  -- ^ Address for the wishbone interface.
  , armCalendarSwap :: Bool
  -- ^ Swap the active and shadow calendar at the end of the metacycle.
  }
  deriving (Generic, NFDataX, Show)

andAck ::
  Signal dom Bool ->
  Circuit
    (Wishbone dom 'Standard addrW (Bytes nBytes))
    (Wishbone dom 'Standard addrW (Bytes nBytes))
andAck extraAck = Circuit go
 where
  go (m2s, s2m0) = (s2m1, m2s)
   where
    s2m1 = (\wb ack -> wb{acknowledge = wb.acknowledge && ack}) <$> s2m0 <*> extraAck

mkCalendarC ::
  forall addrW a dom nBytes.
  ( HiddenClockResetEnable dom
  , ?busByteOrder :: ByteOrder
  , ?regByteOrder :: ByteOrder
  , KnownNat addrW
  , -- , 4 <= addrW
    BitPack a
  , KnownNat nBytes
  , 1 <= nBytes
  , WithTypeDescription a
  , BitPackC a
  , HasCallStack
  ) =>
  -- | Name of the component, used as identifier in the rust code generation.
  String ->
  -- | Calendar configuration for 'calendar'.
  CalendarConfig addrW a ->
  Circuit
    (ConstBwd MM, Wishbone dom 'Standard addrW (Bytes nBytes))
    (CSignal dom a, CSignal dom Bool, CSignal dom (Unsigned 32))
mkCalendarC
  compName
  ( CalendarConfig
      (maxCalDepth :: SNat calDepth)
      (SNat :: SNat repetitionBits)
      bsActive
      bsShadow
    ) = circuit $ \(mm, wb) -> do
{- FOURMOLU_DISABLE -}
    [wb0, wb1, wb2, wb3, wb4, wb5, wb6, (wb7Offset, wb7Meta, wb7Bus)] <- deviceWb (compName <> "_calendar") -< (mm, wb)
    Fwd (writeEntry, _) <- registerWbI writeEntryCfg (unpack 0) -< (wb4, Fwd noWrite)
    Fwd (_, writeActive) <- registerWbI writeAddrCfg (0 :: Index calDepth) -< (wb1, Fwd noWrite)
    Fwd (readAddr, _) <- registerWbI readAddrCfg (0 :: Index calDepth) -< (wb2, Fwd noWrite)
    registerWbI_ readEntryCfg (unpack 0) -< (wb3, Fwd readEntry)
    Fwd (_, shadowDepthActive) <- registerWbI shadowDepthCfg 0 -< (wb0, Fwd shadowDepthWrite)
    Fwd (_, swapActive) <- registerWbI swapActiveCfg False -< (wb5, Fwd noWrite)
    Fwd (metacycleCount, _) <- registerWbI metacycleCountCfg (0 :: Unsigned 32) -< (wb6, Fwd nextMetacycleCount)
    wbEom <- andAck calOut.lastCycle -< wb7Bus
    registerWbI_ endOfMetacycleCfg False -< ((wb7Offset, wb7Meta, wbEom), Fwd noWrite)
    {- FOURMOLU_ ENABLE -}
    let
      nextMetacycleCount = orNothing <$> calOut.lastCycle <*> (metacycleCount + 1)
      calOut = calendar maxCalDepth bsActive bsShadow calCtrl
      readEntry = Just <$> calOut.shadowEntry
      shadowDepthWrite = Just <$> calOut.shadowDepth

      newShadowEntry :: Signal dom (Maybe (Located calDepth (ValidEntry a repetitionBits)))
      newShadowEntry =  (\addr entry -> (,entry) <$> busActivityWrite addr) <$> writeActive <*> writeEntry
      newShadowDepth = busActivityWrite <$> shadowDepthActive
      calCtrl =
        CalendarControl
          <$> newShadowDepth
          <*> newShadowEntry
          <*> readAddr
          <*> (swapActive ./= Nothing)
    idC -< Fwd (calOut.activeEntry.veEntry, calOut.lastCycle, metacycleCount)
   where
    noWrite = pure Nothing
    -- We use separate registers for writing entries into the shadow calendar
    -- and reading entries from the shadow calendar.
    writeEntryCfg =
      (registerConfig "shadowEntry")
        { access = WriteOnly
        , description = "Stores the shadow entry before writing it to the shadow calendar."
        }
    readEntryCfg =
      (registerConfig "shadowEntry")
        { access = ReadOnly
        , description = "Contains the shadow entry at the read address in the shadow calendar."
        }
    writeAddrCfg =
      (registerConfig "writeAddr")
        { access = WriteOnly
        , description =
            "Setting this register will write the shadow entry to the shadow calendar at the given address."
        }
    readAddrCfg =
      (registerConfig "readAddr")
        { access = WriteOnly
        , description = "Controls which entry is read from the shadow calendar."
        }
    shadowDepthCfg =
      (registerConfig "shadowDepthIndex")
        { access = ReadWrite
        , description = "Index of the last entry in the shadow calendar."
        }
    swapActiveCfg =
      (registerConfig "swapActive")
        { access = WriteOnly
        , description =
            "Write to this register to swap the active and shadow calendar at the end of the metacycle."
        }
    metacycleCountCfg =
      (registerConfig "metacycleCount")
        { access = ReadOnly
        , description =
            "The number of metacycles that have passed since the last reset. This register is incremented at the end of each metacycle."
        }
    endOfMetacycleCfg =
      (registerConfig "endOfMetacycle")
        { access = WriteOnly
        , description =
            "Writes to this register will only be acknowledged at the end of the metacycle."
        }

{- | Wrapper function to create a 'calendar' from the given 'CalendarConfig', this way
we prevent the constraints of the type variables used in 'calendar' from leaking into
the rest of the system.
-}
mkCalendar ::
  forall dom nBytes addrW calEntry.
  ( HiddenClockResetEnable dom
  , KnownNat nBytes
  , 1 <= nBytes
  , KnownNat addrW
  , BitPack calEntry
  , WithTypeDescription calEntry
  , BitPackC calEntry
  , ?busByteOrder :: ByteOrder
  , ?regByteOrder :: ByteOrder
  ) =>
  -- | Name of the component, used as identifier in the rust code generation.
  String ->
  -- | Calendar configuration for 'calendar'.
  CalendarConfig addrW calEntry ->
  -- | Wishbone interface (master to slave)
  Signal dom (WishboneM2S addrW nBytes (Bytes nBytes)) ->
  -- |
  -- 1. Currently active entry
  -- 2. Metacycle indicator
  -- 3. Wishbone interface. (slave to master)
  ( Signal dom calEntry
  , Signal dom Bool
  , Signal dom (WishboneS2M (Bytes nBytes))
  , Signal dom (Unsigned 32)
  , MM
  )
mkCalendar compName cfg m2s = case cancelMulDiv @nBytes @8 of
  Dict -> (entry, endOfMetacycle, s2m, metacycleCount, mm)
   where
    ~((mm, s2m), (entry, endOfMetacycle, metacycleCount)) =
      toSignals (mkCalendarC compName cfg) (((), m2s), (pure (), pure (), pure ()))
