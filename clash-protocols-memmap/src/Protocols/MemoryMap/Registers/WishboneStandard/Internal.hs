-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE MultiWayIf #-}

module Protocols.MemoryMap.Registers.WishboneStandard.Internal where

import Clash.Explicit.Prelude
import Protocols

import Clash.Class.BitPackC (BitPackC (..), ByteOrder, Bytes)
import Clash.Class.BitPackC.Padding (SizeInWordsC, maybeUnpackWordC, packWordC)
import Clash.Sized.Internal.BitVector (BitVector (unsafeToNatural))
import Data.Coerce (coerce)
import Data.Constraint (Dict (Dict))
import Data.Constraint.Nat.Lemmas (divWithRemainder)
import Data.Data (Proxy (Proxy))
import Data.Kind (Type)
import Data.Maybe (fromMaybe, maybeToList)
import GHC.Stack (HasCallStack, SrcLoc)
import Protocols.Idle (idleSource)
import Protocols.MemoryMap (
  Access (ReadOnly, ReadWrite, WriteOnly),
  ConstBwd,
  ConstFwd,
  DeviceDefinition (DeviceDefinition, definitionLoc, deviceName, registers, tags),
  MM,
  MemoryMap (MemoryMap, deviceDefs, tree),
  MemoryMapTree (DeviceInstance),
  Name (..),
  NamedLoc (..),
  Register (..),
  locCaller,
  locHere,
  locN,
  regType,
 )
import Protocols.MemoryMap.FieldType (ToFieldType)
import Protocols.Wishbone (
  Wishbone,
  WishboneM2S (..),
  WishboneMode (Standard),
  WishboneS2M (..),
  emptyWishboneM2S,
 )

import qualified Data.List as L
import qualified Data.Map as Map
import qualified Data.String.Interpolate as I
import qualified Protocols.Wishbone as Wishbone

data BusActivity a = BusRead a | BusWrite a
  deriving (Show, Eq, Functor, Generic, NFDataX)

-- | Filters out only 'BusWrite's, mapping any other bus activity to 'Nothing'.
busActivityWrite :: Maybe (BusActivity a) -> Maybe a
busActivityWrite (Just (BusWrite a)) = Just a
busActivityWrite _ = Nothing

-- | Like 'emptyWishboneS2M', but easier for type inference in this module.
emptyWishboneS2M :: forall n. (KnownNat n) => WishboneS2M (Bytes n)
emptyWishboneS2M = (Wishbone.emptyWishboneS2M @(Bytes n))

-- | 'WishboneS2M' with its acknowledge set to 'True'
ackWishboneS2M :: forall n. (KnownNat n) => WishboneS2M (Bytes n)
ackWishboneS2M = emptyWishboneS2M{acknowledge = True}

-- | Whether the Wishbone manager is active (i.e., 'strobe' and 'busCycle' are both 'True')
managerActive :: WishboneM2S aw wordSize (Bytes wordSize) -> Bool
managerActive m2s = m2s.strobe && m2s.busCycle

{- | Type synonym for all the information needed to create a Wishbone register with
auto-assigned offsets.
-}
type RegisterWb (dom :: Domain) (aw :: Nat) (wordSize :: Nat) =
  ( ConstFwd (Offset aw)
  , -- \^ Offset from the base address of the device, produced by 'deviceWb'
    ConstBwd (RegisterMeta aw)
  , -- \^ Meta information about the register, produced by the register
    Wishbone dom 'Standard aw (Bytes wordSize)
    -- \^ A register is a Wishbone subordinate
  )

{- | Type synonym for all the information needed to create a Wishbone register with
custom offsets.
-}
type RegisterWithOffsetWb (dom :: Domain) (aw :: Nat) (wordSize :: Nat) =
  ( ConstBwd (Offset aw)
  , -- \^ Offset from the base address of the device, provided by the user
    ConstBwd (RegisterMeta aw)
  , -- \^ Meta information about the register, produced by the register
    Wishbone dom 'Standard aw (Bytes wordSize)
    -- \^ A register is a Wishbone subordinate
  )

-- | Common \"boring\" constraints for Wishbone mapped registers
type RegisterWbConstraints (a :: Type) (dom :: Domain) (wordSize :: Nat) (aw :: Nat) =
  ( HasCallStack
  , ToFieldType a
  , BitPackC a
  , NFDataX a
  , KnownDomain dom
  , KnownNat wordSize
  , KnownNat aw
  , 1 <= wordSize
  , ?busByteOrder :: ByteOrder
  , ?regByteOrder :: ByteOrder
  )

-- | Configuration for a device -- currently only the name.
data DeviceConfig = DeviceConfig
  { name :: String
  }

{- | Offset from the base address of a device. This really should be an 'Unsigned', but
we use 'BitVector' to avoid unnecessary conversions.
-}
type Offset aw = BitVector aw

{- | Meta information about a register. Fields marked as 'SimOnly' are only used to
construct the memory map during simulation and are ignored during synthesis.
-}
data RegisterMeta aw = RegisterMeta
  { name :: SimOnly Name
  , srcLoc :: SimOnly SrcLoc
  , register :: SimOnly Register
  , lockRegister :: SimOnly (Maybe Register)
  , hasLockRegister :: Bool
  , nWords :: BitVector (aw + 1)
  -- ^ Number of words occupied by this register. Note that it is (+1) because we need
  -- to be able to represent registers that occupy the full address space. Also note
  -- that this includes any lock register that may be present.
  }

data LockRequest
  = -- | Lock the register to its current value. Writing any value to the register will
    -- have no effect until a 'Commit' is written.
    Lock
  | -- | Unlock the register and update its value to the value written while locked.
    Commit
  | -- | Unlock the register and discard any writes that happened while locked.
    Clear
  deriving (Show, Eq, Generic, BitPackC, ToFieldType, NFDataX)

-- | Type synonym to make type signatures clearer.
type InLock = Bool

-- | State of the lock register, stores the data when locked.
data LockState nWords wordSize
  = Unlocked
  | Locked (Vec nWords (Bytes wordSize))
  deriving (Show, Eq, Generic, NFDataX)

-- | Map a function over the data stored in a 'LockState' (if any).
mapLockState ::
  (Vec nWords (Bytes wordSize) -> Vec nWords (Bytes wordSize)) ->
  LockState nWords wordSize ->
  LockState nWords wordSize
mapLockState _ Unlocked = Unlocked
mapLockState f (Locked d) = Locked (f d)

-- | Convert a 'LockState' to a 'Maybe' value, discarding the 'Unlocked' state.
lockStateToMaybe :: LockState nWords wordSize -> Maybe (Vec nWords (Bytes wordSize))
lockStateToMaybe Unlocked = Nothing
lockStateToMaybe (Locked d) = Just d

{- | Whether to add a lock register for a given register. A lock register allows
atomic updates to registers that needs to be updated or read from in multiple
bus cycles.
-}
data Lock
  = -- | Never add a lock register.
    NoLock
  | -- | Add a lock based on the size of the register and the bus word size. If the
    -- register fits within a single bus word, is not zero-width, and an atomic
    -- structure (see 'BitPackC') then no lock is added.
    Auto
  deriving (Show, Eq)

{- | Meta information for a zero-width register. Zero-width registers do not take up
any flip-flops, but do need to be mapped on the bus to be able to observe bus
activity.
-}
zeroWidthRegisterMeta ::
  forall a aw.
  ( KnownNat aw
  , ToFieldType a
  , BitPackC a
  , NFDataX a
  ) =>
  Proxy a ->
  RegisterConfig ->
  RegisterMeta aw
zeroWidthRegisterMeta Proxy conf =
  RegisterMeta
    { name = SimOnly (Name{name = conf.name, description = conf.description})
    , srcLoc = SimOnly locHere
    , register =
        SimOnly
          Register
            { fieldType = regType @a
            , address = 0x0
            , access = conf.access
            , tags = "zero-width" : conf.tags
            , reset = Nothing
            }
    , lockRegister = SimOnly Nothing
    , hasLockRegister = False
    , -- BitPackC would report a size of 0, but we want to be able to observe
      -- the bus activity, so an address needs to be reserved anyway. For this,
      -- the number of words gets overwritten to 1 here.
      nWords = 1
    }

-- | What to do when a bus read occurs at the same time as a circuit write.
data BusReadBehavior
  = -- | When a bus read occurs at the same time as a circuit write, read the
    -- value still in the register.
    PreferRegister
  | -- | When a bus read occurs at the same time as a circuit write, read the
    -- value being written by the circuit. Selecting this option introduces an
    -- extra mux and, depending on the type, packing logic.
    PreferCircuit
  deriving (Show)

data RegisterConfig = RegisterConfig
  { name :: String
  -- ^ Name included for code generation
  , description :: String
  -- ^ Description included for code generation
  , tags :: [String]
  -- ^ Tags included for code generation
  , access :: Access
  -- ^ Access rights for this register, also propagated to code generation. Default:
  -- 'ReadWrite'.
  , busRead :: BusReadBehavior
  -- ^ Behavior when a bus read occurs at the same time as a circuit write. Default:
  -- 'PreferRegister'.
  , lock :: Lock
  -- ^ Whether to add a lock register for this register. Default: 'Auto'.
  }
  deriving (Show)

{- | Default register configuration. The defaults are chosen in order of safety,
efficiency, then convenience. For values, see field documentation of 'RegisterConfig'.
-}
registerConfig :: String -> RegisterConfig
registerConfig name =
  -- If you add a default value here, pick the default option in this order:
  --
  --   * Safety. All safety options should be on by default, even if it comes at a
  --     hardware cost.
  --
  --   * Efficiency: If safety is not a concern, pick the option that will result in
  --     the cheapest hardware.
  --
  --   * Convenience. If neither safety nor efficiency applies, pick an option you think
  --     most developers would want.
  --
  RegisterConfig
    { name
    , description = ""
    , tags = []
    , access = ReadWrite
    , busRead = PreferRegister
    , lock = Auto
    }

-- These have no business being in this module :)
replaceWith :: (KnownNat n) => Index n -> (a -> a) -> Vec n a -> Vec n a
replaceWith i f xs = replace i (f (xs !! i)) xs

{- | Like 'deviceWb', but allows you to set offsets of registers manually. This
can be important in cases where you'd like gaps between the registers. You can
either pass in the offsets manually or use 'registerWithOffsetWbDf'.
-}
deviceWithOffsetsWb ::
  forall n wordSize aw dom.
  (HasCallStack, KnownNat n, KnownNat aw, KnownNat wordSize) =>
  -- | Device name
  String ->
  Circuit
    ( ConstBwd MM
    , Wishbone dom 'Standard aw (Bytes wordSize)
    )
    ( Vec n (RegisterWithOffsetWb dom aw wordSize)
    )
deviceWithOffsetsWb deviceName =
  case divWithRemainder @wordSize @8 @7 of
    Dict ->
      Circuit go
 where
  -- This behemoth of a type signature because the inferred type signature is
  -- too general, confusing the type checker.
  go ::
    ( ((), Signal dom (WishboneM2S aw wordSize (Bytes wordSize)))
    , Vec
        n
        ( BitVector aw
        , RegisterMeta aw
        , Signal dom (WishboneS2M (Bytes wordSize))
        )
    ) ->
    ( ( SimOnly MemoryMap
      , Signal dom (WishboneS2M (Bytes wordSize))
      )
    , Vec
        n
        ( ()
        , ()
        , Signal dom (WishboneM2S aw wordSize (Bytes wordSize))
        )
    )
  go ((_, wbM2S), unzip3 -> (offsets, metas, wbS2Ms)) =
    ((SimOnly mm, wbS2M), ((),(),) <$> unbundle wbM2Ss)
   where
    unSimOnly :: SimOnly a -> a
    unSimOnly (SimOnly a) = a

    -- Note that we filter zero-width registers out here
    metaToRegisters :: Offset aw -> RegisterMeta aw -> [NamedLoc Register]
    metaToRegisters o m =
      baseRegister : maybeToList (makeLockRegister <$> unSimOnly m.lockRegister)
     where
      -- XXX: Quasiquoter doesn't support record dot access
      baseName = baseRegister.name.name

      makeLockRegister r =
        NamedLoc
          { name =
              Name
                { name = baseRegister.name.name <> "_lock"
                , description = [I.i|Lock register for '#{baseName}'|]
                }
          , loc = unSimOnly m.srcLoc
          , value =
              r
                { address = (fromIntegral o + fromIntegral m.nWords - 1) * natToNum @wordSize
                }
          }

      baseRegister =
        NamedLoc
          { name = unSimOnly m.name
          , loc = unSimOnly m.srcLoc
          , value =
              (unSimOnly m.register)
                { address = fromIntegral o * natToNum @wordSize
                }
          }

    mm =
      MemoryMap
        { deviceDefs =
            Map.singleton deviceName
              $ DeviceDefinition
                { deviceName = Name{name = deviceName, description = ""}
                , registers = L.concat $ L.zipWith metaToRegisters (toList offsets) (toList metas)
                , definitionLoc = locN 0
                , tags = []
                }
        , tree = DeviceInstance (locN 1) deviceName
        }

    (wbS2M, wbM2Ss) =
      unbundle
        $ selectSubordinate
        <$> activeSubordinate
        <*> wbM2S
        <*> bundle wbS2Ms

    activeSubordinate :: Signal dom (Maybe (Index n))
    activeSubordinate = elemIndex True <$> activeSubordinates

    activeSubordinates :: Signal dom (Vec n Bool)
    activeSubordinates =
      fmap
        (isActiveSubordinate <$> offsets <*> metas <*>)
        (repeat <$> fmap (.addr) wbM2S)

    isActiveSubordinate ::
      -- Subordinate offset (constant during compilation)
      Offset aw ->
      -- Subordinate meta data
      RegisterMeta aw ->
      -- Current address on the bus
      BitVector aw ->
      -- Active?
      Bool
    isActiveSubordinate offset RegisterMeta{nWords} addr
      -- Optimization case of single word registers: no need to involve `Ord`
      | nWords == 1 || nWords == 0 = offset == addr
      -- General case: check whether the address is within the range of the register
      | otherwise = addr >= offset && extend addr < extend offset + nWords

    selectSubordinate ::
      Maybe (Index n) ->
      WishboneM2S aw wordSize (Bytes wordSize) ->
      Vec n (WishboneS2M (Bytes wordSize)) ->
      ( WishboneS2M (Bytes wordSize)
      , Vec n (WishboneM2S aw wordSize (Bytes wordSize))
      )
    selectSubordinate index m2s s2ms
      | m2s.strobe && m2s.busCycle =
          case index of
            Nothing ->
              -- Unmapped address requested
              ( strictV s2ms `seqX` emptyWishboneS2M{err = True}
              , repeat m2s{busCycle = False}
              )
            Just i ->
              -- Mapped address requested
              ( strictV s2ms !! i
              , replaceWith
                  i
                  (\m -> m{busCycle = True})
                  (repeat m2s{busCycle = False})
              )
      | otherwise =
          -- Bus inactive
          (strictV s2ms `seqX` emptyWishboneS2M, repeat m2s)

    strictV :: Vec n a -> Vec n a
    strictV v
      | clashSimulation = foldl (\b a -> a `seqX` b) () v `seqX` v
      | otherwise = v

{- | Circuit internal to 'registerWbDf' that rejects illegal accesses on the bus.
If an illegal access takes place, an error is returned on the wishbone bus and
the request isn't propagated to the RHS.
-}
accessC ::
  forall dom aw wordSize.
  ( KnownNat wordSize
  , KnownNat aw
  , wordSize ~ Div ((wordSize * 8) + 7) 8
  ) =>
  Access ->
  Circuit
    (Wishbone dom 'Standard aw (Bytes wordSize))
    (Wishbone dom 'Standard aw (Bytes wordSize))
accessC access =
  Circuit $ \(wbM2SIn, wbS2MOut) ->
    unbundle $ checkAccess <$> wbM2SIn <*> wbS2MOut
 where
  checkAccess ::
    WishboneM2S aw wordSize (Bytes wordSize) ->
    WishboneS2M (Bytes wordSize) ->
    ( WishboneS2M (Bytes wordSize)
    , WishboneM2S aw wordSize (Bytes wordSize)
    )
  checkAccess m2s s2m
    | managerActive m2s && accessFault =
        ( (emptyWishboneS2M @wordSize){err = True, readData = 0}
        , emptyWishboneM2S
        )
    | otherwise = (s2m, m2s)
   where
    readOnlyFault = access == ReadOnly && m2s.writeEnable
    writeOnlyFault = access == WriteOnly && not m2s.writeEnable
    accessFault = readOnlyFault || writeOnlyFault

{- | Circuit internal to 'registerWbDf' that reorders bytes on the bus if they
do not correspond to the register's internal byte order. The LHS is expected
to be in bus byte order, the RHS in register byte order.
-}
orderC ::
  forall dom aw wordSize.
  ( KnownNat wordSize
  , ?busByteOrder :: ByteOrder
  , ?regByteOrder :: ByteOrder
  ) =>
  Circuit
    (Wishbone dom 'Standard aw (Bytes wordSize))
    (Wishbone dom 'Standard aw (Bytes wordSize))
orderC = Circuit go
 where
  needReverse = ?busByteOrder /= ?regByteOrder

  go (wbM2SIn, wbS2MOut) = (wbS2MIn, wbM2SOut)
   where
    wbM2SOut = if needReverse then reorderM2S <$> wbM2SIn else wbM2SIn
    wbS2MIn = if needReverse then reorderS2M <$> wbS2MOut else wbS2MOut

  reorderM2S m2s =
    m2s
      { writeData = reverseBytes m2s.writeData
      , busSelect = reverseBits m2s.busSelect
      }

  reorderS2M s2m = s2m{readData = reverseBytes s2m.readData}

{- | Circuit internal to 'registerWbDf' that computes the relative offset on the
bus, i.e., the offset within the register and replaces the address field with it.
-}
offsetC ::
  forall a dom wordSize aw nWords.
  ( RegisterWbConstraints a dom wordSize aw
  , wordSize ~ Div ((wordSize * 8) + 7) 8
  , nWords ~ SizeInWordsC wordSize a
  , KnownNat nWords
  , 1 <= nWords
  ) =>
  Proxy a ->
  Offset aw ->
  -- | Whether the register has a lock register
  Bool ->
  Circuit
    (Wishbone dom 'Standard aw (Bytes wordSize))
    (Wishbone dom 'Standard aw (Bytes wordSize))
offsetC Proxy offset hasLockRegister =
  Circuit $ \(wbM2SIn, wbS2MOut) ->
    ( wbS2MOut
    , if hasLockRegister
        then computeRelativeOffset (SNat @(nWords + 1)) offset <$> wbM2SIn
        else computeRelativeOffset (SNat @nWords) offset <$> wbM2SIn
    )

{- | Compute the relative offset on the bus and replace the address field with
it. A relative offset is the offset within the register. E.g., for a register
occupying 4 words, the possible relative offsets are 0, 1, 2, and 3.

Note: If a lock register is present, the number of words passed in should
include the lock register.
-}
computeRelativeOffset ::
  forall nWords wordSize aw.
  (1 <= nWords, KnownNat aw) =>
  SNat nWords ->
  Offset aw ->
  WishboneM2S aw wordSize (Bytes wordSize) ->
  WishboneM2S aw wordSize (Bytes wordSize)
computeRelativeOffset SNat offset m2s = m2s{addr = resize (pack relativeOffset)}
 where
  relativeOffset :: Index nWords
  relativeOffset = unpack $ addrLsbs - offsetLsbs

  offsetLsbs = resize offset :: BitVector (BitSize (Index nWords))
  addrLsbs = resize m2s.addr :: BitVector (BitSize (Index nWords))

{- | Circuit internal to 'registerWbDf' that produces register meta information.
The address field is left as zero and will be set by 'deviceWithOffsetsWb'.
-}
metaC ::
  forall a aw wordSize.
  ( ToFieldType a
  , BitPackC a
  , NFDataX a
  , KnownNat aw
  , KnownNat wordSize
  , 1 <= wordSize
  , HasCallStack
  , ?regByteOrder :: ByteOrder
  ) =>
  SNat wordSize ->
  Proxy a ->
  RegisterConfig ->
  a ->
  Circuit (ConstBwd (RegisterMeta aw)) (ConstFwd Bool)
metaC SNat Proxy conf resetValue = Circuit $ \((), ()) -> (regMeta, hasLockRegister)
 where
  regMeta =
    RegisterMeta
      { name = SimOnly $ Name{name = conf.name, description = conf.description}
      , srcLoc = SimOnly locCaller
      , nWords = nWords + if hasLockRegister then 1 else 0
      , hasLockRegister
      , lockRegister =
          SimOnly
            $ if hasLockRegister
              then
                Just
                  $ Register
                    { fieldType = regType @LockRequest
                    , address = 0x0 -- Note: will be set by 'deviceWithOffsetsWb'
                    , access = WriteOnly
                    , tags = ["is-lock"]
                    , reset = Nothing
                    }
              else Nothing
      , register =
          SimOnly
            $ Register
              { fieldType = regType @a
              , address = 0x0 -- Note: will be set by 'deviceWithOffsetsWb'
              , access = conf.access
              , tags = conf.tags <> ["has-lock" | hasLockRegister]
              , reset = Just simOnlyResetValue
              }
      }

  nWords = natToNum @(SizeInWordsC wordSize a)

  hasLockRegister =
    case conf.lock of
      NoLock -> False
      Auto -> nWords > 1 || not (isAtomicC (Proxy @a))

  -- The fact that this goes into a 'SimOnly' construct should be enough for Clash to
  -- deduce that it can throw the expression below away, but it doesn't. As a result,
  -- it sees the constructor of BitVector and freaks out. Spelling it out like this
  -- (i.e., putting it behind a 'clashSimulation' flag) makes Clash okay with it.
  simOnlyResetValue
    | clashSimulation = (pack (packWordC @wordSize ?regByteOrder resetValue)).unsafeToNatural
    | otherwise = 0

{- | Circuit internal to 'registerWbDf' that implements the lock register
functionality. When no lock register is present, this circuit is a passthrough.
When there is, it acts as a passthrough until a lock is requested, after which
writes are stored until a 'Commit' or 'Clear' is received. During a lock, it
also indicates that the register is locked on the 'InLock' output. The normal
register (in 'registerC') must use this signal to refuse any writes from the
circuit side.

TODO: We currently duplicate 'maskWriteData' for the shadow register and the
      shadow register, even though in practice only one of them will be doing
      useful work in one specific clock cycle. I've played around with sharing
      the logic, but it just makes the code more complex for very little gain (?).
-}
lockC ::
  forall a dom wordSize aw nWords.
  ( RegisterWbConstraints a dom wordSize aw
  , wordSize ~ Div ((wordSize * 8) + 7) 8
  , nWords ~ SizeInWordsC wordSize a
  , KnownNat nWords
  , 1 <= nWords
  ) =>
  Proxy a ->
  Clock dom ->
  Reset dom ->
  -- | Whether the register has a lock register. If not, this circuit is a
  -- passthrough.
  Bool ->
  Circuit
    ( Wishbone dom Standard aw (Bytes wordSize)
    , CSignal dom (Vec (SizeInWordsC wordSize a) (Bytes wordSize)) -- Packed current register value
    )
    ( Wishbone dom Standard aw (Bytes wordSize)
    , Df dom (Vec (SizeInWordsC wordSize a) (Bytes wordSize))
    , CSignal dom InLock
    )
lockC Proxy _clk _rst False = circuit $ \(wb, _currentValue) -> do
  df <- idleSource
  idC -< (wb, df, Fwd (pure False))
lockC Proxy clk rst True = Circuit go0
 where
  commitAddress = natToNum @nWords @(BitVector aw)

  go0 ::
    ( ( Signal dom (WishboneM2S aw wordSize (Bytes wordSize))
      , Signal dom (Vec (SizeInWordsC wordSize a) (Bytes wordSize)) -- Packed current register value
      )
    , ( Signal dom (WishboneS2M (Bytes wordSize))
      , Signal dom Ack
      , Signal dom ()
      )
    ) ->
    ( ( Signal dom (WishboneS2M (Bytes wordSize))
      , Signal dom ()
      )
    , ( Signal dom (WishboneM2S aw wordSize (Bytes wordSize))
      , Signal dom (Maybe (Vec (SizeInWordsC wordSize a) (Bytes wordSize)))
      , Signal dom InLock
      )
    )
  go0 ((m2sIn, currentValueIn), (s2mIn, lockWriteAck, _inLockAck)) = ((s2mOut, pure ()), (m2sOut, lockWrite, inLock))
   where
    (s2mOut, m2sOut, lockWrite, inLock) =
      mealyB clk rst enableGen go1 Unlocked (m2sIn, s2mIn, lockWriteAck, currentValueIn)

  go1 ::
    LockState nWords wordSize ->
    ( WishboneM2S aw wordSize (Bytes wordSize)
    , WishboneS2M (Bytes wordSize)
    , Ack
    , Vec (SizeInWordsC wordSize a) (Bytes wordSize) -- Packed current register value
    ) ->
    ( LockState nWords wordSize
    , ( WishboneS2M (Bytes wordSize)
      , WishboneM2S aw wordSize (Bytes wordSize)
      , Maybe (Vec (SizeInWordsC wordSize a) (Bytes wordSize))
      , InLock
      )
    )
  go1 s (m2s, s2mIn, _, currentValue) =
    if
      -- Just passthrough if....
      --
      -- 1. The bus is not active
      | not (managerActive m2s)
          -- 2. If the bus is *not* accessing the lock register and we're not locked
          --    Note that if we *are* locked, we need to intercept reads and writes.
          || (not isCommitAddress && not locked)
          -- 3. If the bus is *not* accessing the lock register, we're locked, and
          --    the bus is reading. Note that we expect data to be stored in the
          --    normal register still. (Maybe we shouldn't?)
          || (not isCommitAddress && not m2s.writeEnable) ->
          passthrough
      -- If the bus is *not* accessing the lock register, we're locked, and the
      -- bus is writing, we need to update the lock register.
      | not isCommitAddress ->
          ( mapLockState (maskWriteData offset m2s.busSelect m2s.writeData) s
          , (ackWishboneS2M, emptyWishboneM2S, Nothing, locked)
          )
      -- Bus is writing to the lock register
      | m2s.writeEnable ->
          case (lockRequest, s) of
            (Lock, _) ->
              -- Lock the register - copy current value to shadow. We copy the
              -- current value to support partial updates while locked, for example
              -- vectors.
              (Locked currentValue, (ackWishboneS2M, emptyWishboneM2S, Nothing, locked))
            (Commit, Locked d) ->
              -- Commit the shadow writes
              (Unlocked, (ackWishboneS2M, emptyWishboneM2S, Just d, locked))
            (Commit, Unlocked) ->
              -- Error: trying to commit when not locked
              (Unlocked, (emptyWishboneS2M{err = True}, emptyWishboneM2S, Nothing, locked))
            (Clear, _) ->
              -- Clear the lock (works whether locked or not)
              (Unlocked, (ackWishboneS2M, emptyWishboneM2S, Nothing, locked))
      -- Bus is reading from the lock register (write-only), return error
      | otherwise ->
          (s, (emptyWishboneS2M{err = True}, emptyWishboneM2S, Nothing, locked))
   where
    passthrough = (s, (s2mIn, m2s, Nothing, locked))

    offset = unpack $ resize m2s.addr
    isCommitAddress = m2s.addr == commitAddress
    locked =
      case s of
        Unlocked -> False
        Locked _ -> True

    lockRequest :: LockRequest
    lockRequest =
      fromMaybe err
        . maybeUnpackWordC ?regByteOrder
        $ maskWriteData 0 m2s.busSelect m2s.writeData (repeat m2s.writeData)

    -- XXX: Quasiquoter doesn't work with implicit parameters nor record dot selectors
    regByteOrder = ?regByteOrder
    writeData = m2s.writeData
    busSelect = m2s.busSelect
    err =
      deepErrorX
        [I.i|
        Unpack failed for LockRequest in registerWbDf:
          wordSize:      #{natToInteger @wordSize}
          regByteOrder:  #{regByteOrder}
          m2s.writeData: #{writeData}
          m2s.busSelect: #{busSelect}
      |]

{- | Circuit internal to 'registerWbDf' that holds the actual register value. It
assumes that access control, byte ordering, and offset calculation have already
been taken care of. See 'accessC', 'orderC', and 'offsetC'.
-}
registerC ::
  forall a dom wordSize aw nWords.
  ( RegisterWbConstraints a dom wordSize aw
  , nWords ~ SizeInWordsC wordSize a
  , KnownNat nWords
  , 1 <= nWords
  ) =>
  Clock dom ->
  Reset dom ->
  -- | Configuration values
  RegisterConfig ->
  -- | Reset value
  a ->
  Circuit
    ( Wishbone dom Standard aw (Bytes wordSize)
    , -- \| Write from circuit
      CSignal dom (Maybe a)
    , -- \| Data from lock register
      Df dom (Vec (SizeInWordsC wordSize a) (Bytes wordSize))
    , -- \| In lock?
      CSignal dom InLock
    )
    ( CSignal dom a -- Current value
    , CSignal dom (Vec (SizeInWordsC wordSize a) (Bytes wordSize)) -- Current value (packed)
    , Df dom (BusActivity a)
    )
registerC clk rst regConfig resetValue = circuit $ \(wb, aIn, lockWrite, inLock) -> do
  let packedOut = regMaybe clk rst enableGen (packC resetValue) packedWrite
  (Fwd packedWrite, busActivity) <- goC -< (wb, aIn, lockWrite, inLock, Fwd packedOut)
  idC -< (Fwd (unpackC <$> packedOut), Fwd packedOut, busActivity)
 where
  goC ::
    Circuit
      ( Wishbone dom Standard aw (Bytes wordSize)
      , CSignal dom (Maybe a) -- Write from circuit
      , Df dom (Vec (SizeInWordsC wordSize a) (Bytes wordSize))
      , CSignal dom InLock
      , CSignal dom (Vec (SizeInWordsC wordSize a) (Bytes wordSize))
      )
      ( CSignal dom (Maybe (Vec (SizeInWordsC wordSize a) (Bytes wordSize)))
      , Df dom (BusActivity a)
      )
  goC = Circuit $ \((m2s, circuitWrite, lockWrite, inLock, registerValue), (_, busActivityAck)) -> do
    let
      (s2m, registerWrite, busActivity) =
        unbundle
          $ case divWithRemainder @wordSize @8 @7 of
            Dict ->
              go
                <$> m2s
                <*> (fmap packC <$> circuitWrite)
                <*> lockWrite
                <*> inLock
                <*> registerValue
                <*> busActivityAck

    ((s2m, pure (), busActivityAck, pure (), pure ()), (registerWrite, busActivity))

  go ::
    WishboneM2S aw wordSize (Bytes wordSize) ->
    -- \| Circuit write
    Maybe (Vec nWords (Bytes wordSize)) ->
    -- \| Lock write
    Maybe (Vec nWords (Bytes wordSize)) ->
    InLock ->
    Vec nWords (Bytes wordSize) ->
    Ack ->
    ( WishboneS2M (Bytes wordSize)
    , Maybe (Vec nWords (Bytes wordSize))
    , Maybe (BusActivity a)
    )
  go m2s circuitWrite lockWrite inLock registerValue ~(Ack acknowledge)
    | Just d <- lockWrite =
        ( emptyWishboneS2M
        , if acknowledge then lockWrite else Nothing
        , Just (BusWrite (unpackC d))
        )
    | m2s.strobe && m2s.busCycle = (s2m, registerWrite, busActivity)
    | inLock = (emptyWishboneS2M, Nothing, Nothing)
    | otherwise = (emptyWishboneS2M, circuitWrite, Nothing)
   where
    -- Note that every definition below this comment is only relevant when the
    -- bus is **active** (and therefore not handling a lock write, as these two
    -- events are mutually exclusive).
    busActivity
      | Just v <- registerValueAfterBusWrite = Just (BusWrite (unpackC v))
      | otherwise = (Just (BusRead (unpackC registerValue)))

    registerValueAfterBusWrite
      | m2s.writeEnable =
          Just (maskWriteData addressAsIndex m2s.busSelect m2s.writeData registerValue)
      | otherwise = Nothing

    registerWrite
      | acknowledge = circuitWrite <|> registerValueAfterBusWrite
      | otherwise = Nothing

    addressAsIndex = unpack @(Index nWords) (resize m2s.addr)
    s2m = (emptyWishboneS2M @nWords){acknowledge, readData}
    readData
      -- Though we could return a deepErrorX here, doing so would leak register data
      | m2s.writeEnable = 0
      | PreferRegister <- regConfig.busRead = registerValue !! addressAsIndex
      | PreferCircuit <- regConfig.busRead =
          (fromMaybe registerValue circuitWrite) !! addressAsIndex

  -- Alias for packing with the correct word size and byte order
  packC :: a -> Vec (SizeInWordsC wordSize a) (Bytes wordSize)
  packC = packWordC ?regByteOrder

  -- Alias for unpacking with the correct word size and byte order
  unpackC :: Vec (SizeInWordsC wordSize a) (Bytes wordSize) -> a
  unpackC packed = fromMaybe err . maybeUnpackWordC ?regByteOrder $ packed
   where
    -- XXX: Quasiquoter doesn't work with implicit parameters
    regByteOrder = ?regByteOrder

    err =
      deepErrorX
        [I.i|
        Unpack failed in registerWbDf:
          wordSize:     #{natToInteger @wordSize}
          regByteOrder: #{regByteOrder}
          packedOut:    #{packed}
      |]

{- | Circuit writes always take priority over bus writes, though circuit writes
may get ignored while a register is locked. Bus writes rejected with an error if
access rights are set to 'ReadOnly'. Similarly, bus reads are rejected
if access rights are set to 'WriteOnly'.

You can tie registers created using this function together with 'deviceWb'. If
you're looking to create a device with arbitrary offsets, use
'registerWithOffsetWbDf' instead.

The register is configurable in its byte order, both on the bus and internally
using '?busByteOrder' and '?regByteOrder' respectively. For VexRiscV, you'd want
to configure @?busByteOrder = BigEndian@ and @?regByteOrder = LittleEndian@. Also
see 'withBittideByteOrder'. Note that the bus byte order is a hack more than
anything else and only affects the data and byte enable fields.
-}
registerWbDf ::
  forall a dom wordSize aw.
  (RegisterWbConstraints a dom wordSize aw) =>
  Clock dom ->
  Reset dom ->
  -- | Configuration values
  RegisterConfig ->
  -- | Reset value
  a ->
  Circuit
    ( RegisterWb dom aw wordSize
    , CSignal dom (Maybe a)
    )
    ( CSignal dom a
    , Df dom (BusActivity a)
    )
registerWbDf clk rst regConfig resetValue =
  case SNat @(SizeInWordsC wordSize a) of
    (nWords@SNat :: SNat nWords) ->
      case d1 `compareSNat` nWords of
        SNatLE ->
          -- "Normal" register that actually holds data
          case divWithRemainder @wordSize @8 @7 of
            Dict -> circuit $ \((Fwd offset, meta, wb0), circuitWrite) -> do
              wb1 <- accessC regConfig.access -< wb0
              wb2 <- orderC -< wb1
              wb3 <- offsetC (Proxy @a) offset hasLock -< wb2
              (wb4, lockWrite, inLock) <- lockC (Proxy @a) clk rst hasLock -< (wb3, regValuePacked)
              Fwd hasLock <- metaC (SNat @wordSize) (Proxy @a) regConfig resetValue -< meta
              (regValue, regValuePacked, busActivity) <-
                registerC clk rst regConfig resetValue -< (wb4, circuitWrite, lockWrite, inLock)
              idC -< (regValue, busActivity)
        SNatGT ->
          -- Zero-width register that only provides bus activity information
          Circuit $ \(((_, _, m2s0), _), (_, ack)) ->
            let
              update m2s1 acknowledge
                | not (m2s1.strobe && m2s1.busCycle) = (Nothing, emptyWishboneS2M)
                | m2s1.writeEnable = (Just (BusWrite resetValue), emptyWishboneS2M{acknowledge})
                | otherwise =
                    ( Just (BusRead resetValue)
                    , (emptyWishboneS2M @wordSize){acknowledge}
                    )

              (unbundle -> (busActivity, s2m)) = update <$> m2s0 <*> coerce ack
             in
              ( (((), zeroWidthRegisterMeta @a Proxy regConfig, s2m), pure ())
              , (pure resetValue, busActivity)
              )

reverseBytes ::
  forall wordSize.
  (KnownNat wordSize) =>
  BitVector (wordSize * 8) ->
  BitVector (wordSize * 8)
reverseBytes = pack . reverse . unpack @(Vec wordSize (Bytes 1))

reverseBits ::
  forall n.
  (KnownNat n) =>
  BitVector n ->
  BitVector n
reverseBits = pack . reverse . unpack @(Vec n Bit)

{- | Takes the data from the bus and the register and combines them based on the
byte enables and word index we're writing to.
-}
maskWriteData ::
  forall wordSize nWords.
  ( KnownNat wordSize
  , KnownNat nWords
  ) =>
  -- | Offset from base address of register
  Index nWords ->
  -- | Mask for data on bus
  BitVector wordSize ->
  -- | Data from bus
  Bytes wordSize ->
  -- | Data from register
  Vec nWords (Bytes wordSize) ->
  -- | Combined data
  Vec nWords (Bytes wordSize)
maskWriteData offset mask busData regData = adjust offset regData $ \regWord ->
  pack
    $ mux
      (unpack mask :: Vec wordSize Bool)
      (unpack busData :: Vec wordSize (BitVector 8))
      (unpack regWord :: Vec wordSize (BitVector 8))
 where
  adjust ::
    forall n a.
    (KnownNat n) =>
    Index n ->
    Vec nWords a ->
    (a -> a) ->
    Vec nWords a
  adjust i xs f = replace i (f (xs !! i)) xs

{- | 'registerWbDf' already checks for illegal write or read operations and does not
acknowledge them. So there is no need to check for 'ReadOnly' or 'WriteOnly'.
-}
getBusActivity :: WishboneS2M bv -> a -> Maybe a -> Maybe (BusActivity a)
getBusActivity s2m regValue maybeUpdatedRegValue
  | not s2m.acknowledge = Nothing
  | Just v <- maybeUpdatedRegValue = Just (BusWrite v)
  | otherwise = Just (BusRead regValue)
