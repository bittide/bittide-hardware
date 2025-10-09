-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{- | Utilities for creating Wishbone devices and registers, while also creating
a memory map.
-}
module Protocols.MemoryMap.Registers.WishboneStandard (
  -- * Device creation
  deviceWb,
  deviceWithOffsetsWb,

  -- * Register creation (explicit clocks and resets)
  registerWb,
  registerWb_,
  registerWbDf,
  registerWithOffsetWb,
  registerWithOffsetWbDf,

  -- * Register creation (implicit clocks and resets)
  registerWbI,
  registerWbI_,
  registerWbDfI,
  registerWithOffsetWbI,
  registerWithOffsetWbDfI,

  -- * Supporting types and functions
  registerConfig,
  busActivityWrite,
  BusReadBehavior (..),
  BusActivity (..),
  DeviceConfig (..),
  RegisterConfig (..),

  -- * Internals
  RegisterMeta (..),
  zeroWidthRegisterMeta,
  maskWriteData,
  getBusActivity,
  replaceWith,
  reverseBytes,
  reverseBits,
) where

import Clash.Explicit.Prelude
import Protocols

import Clash.Class.BitPackC (BitPackC (..), ByteOrder, Bytes)
import Clash.Class.BitPackC.Padding (SizeInWordsC, maybeUnpackWordC, packWordC)
import Clash.Prelude (HiddenClock, HiddenReset, hasClock, hasReset)
import Clash.Sized.Internal.BitVector (BitVector (unsafeToNatural))
import Data.Coerce (coerce)
import Data.Constraint (Dict (Dict))
import Data.Constraint.Nat.Lemmas (divWithRemainder)
import Data.Data (Proxy (Proxy))
import Data.Maybe (fromMaybe, isJust, maybeToList)
import GHC.Stack (HasCallStack, SrcLoc, withFrozenCallStack)
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
  emptyWishboneS2M,
 )

import qualified Data.List as L
import qualified Data.Map as Map
import qualified Data.String.Interpolate as I
import qualified Protocols.Df as Df
import qualified Protocols.Vec as V

data BusActivity a = BusRead a | BusWrite a
  deriving (Show, Eq, Functor, Generic, NFDataX)

-- | Filters out only 'BusWrite's, mapping any other bus activity to 'Nothing'.
busActivityWrite :: Maybe (BusActivity a) -> Maybe a
busActivityWrite (Just (BusWrite a)) = Just a
busActivityWrite _ = Nothing

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

-- | Configuration for a device -- currently only the name.
data DeviceConfig = DeviceConfig
  { name :: String
  }

{- | Offset from the base address of a device. This really should be an 'Unsigned', but
we use 'BitVector' to avoid unnecessary conversions.
-}
type Offset aw = BitVector aw

data LockRequest
  = Lock
  | Clear
  | Commit
  deriving (Show, Eq)

data Lock
  = NoLock
  | Auto
  deriving (Show, Eq)

{- | Meta information about a register. Fields marked as 'SimOnly' are only used to
construct the memory map during simulation and are ignored during synthesis.
-}
data RegisterMeta aw = RegisterMeta
  { name :: SimOnly Name
  , srcLoc :: SimOnly SrcLoc
  , register :: SimOnly Register
  , lockRegister :: SimOnly (Maybe Register)
  , nWords :: BitVector (aw + 1)
  -- ^ Number of words occupied by this register. Note that it is (+1) because we need
  -- to be able to represent registers that occupy the full address space.
  }

{- | Meta information for a zero-width register. Zero-width registers do not take up
any flip-flops, but do need to be mapped on the bus to be able to observe bus
activity.
-}
zeroWidthRegisterMeta ::
  forall a aw.
  ( KnownNat aw
  , ToFieldType a
  , BitPackC a
  , BitPack a
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
    , -- BitPackC would report a size of 0, but we want to be able to observe
      -- the bus activity, so an address needs to be reserved anyway. For this,
      -- the number of words gets overwritten to 1 here.
      nWords = 1
    }

registerMetaToLockRegister ::
  forall aw.
  (KnownNat aw) =>
  -- | Base register
  RegisterMeta aw ->
  Register
registerMetaToLockRegister RegisterMeta{register = SimOnly reg} =
  Register
    { fieldType = regType @LockRequest
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
  , description :: String
  , tags :: [String]
  , access :: Access
  , busRead :: BusReadBehavior
  , lock :: Lock
  }
  deriving (Show)

registerConfig :: String -> RegisterConfig
registerConfig name =
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

{- | Tie a bunch of registers together to form a device. Note that @aw@ must be
chosen such that it can hold the addresses of all registers, including their
maximum byte address.

Example usage:

> deviceExample clk rst = circuit $ \(mm, wb) -> do
>   [reg1, reg2, reg3] <- deviceWb "example" -< (mm, wb)
>
>   registerWb_ clk rst (registerConfig "float") (0.0 :: Float)     -< (reg1, Fwd noWrite)
>   registerWb_ clk rst (registerConfig "u16")   (0 :: Unsigned 16) -< (reg2, Fwd noWrite)
>   registerWb_ clk rst (registerConfig "bool")  (False :: Bool)    -< (reg3, Fwd noWrite)
>
>   idC
>  where
>   noWrite = pure Nothing
-}
deviceWb ::
  forall n wordSize aw dom.
  ( HasCallStack
  , KnownNat n
  , KnownNat wordSize
  , KnownNat aw
  ) =>
  -- | Device name
  String ->
  Circuit
    ( ConstBwd MM
    , Wishbone dom 'Standard aw (Bytes wordSize)
    )
    ( Vec n (RegisterWb dom aw wordSize)
    )
deviceWb deviceName = circuit $ \(mm, wb) -> do
  (offsets0, metas0, wbs) <-
    V.unzip3 <| withFrozenCallStack deviceWithOffsetsWb deviceName -< (mm, wb)
  (offsets1, metas1) <- genOffsets -< (offsets0, metas0)
  V.zip3 -< (offsets1, metas1, wbs)
 where
  -- Generate offsets based on the sizes of the registers. The first address will be
  -- at offset 0, the next at the size of the first register, etc.
  --
  -- XXX: Handle alignment requirements. For example, a u64 should be aligned to a
  --      8-byte boundary. Because every register starts at a multiple of the word
  --      size, it auto-aligns at least at a `wordSize`-byte boundary.
  genOffsets ::
    Circuit
      (Vec n (ConstBwd (Offset aw)), Vec n (ConstBwd (RegisterMeta aw)))
      (Vec n (ConstFwd (Offset aw)), Vec n (ConstBwd (RegisterMeta aw)))
  genOffsets = Circuit go
   where
    go ::
      ((Vec n (), Vec n ()), (Vec n (), Vec n (RegisterMeta aw))) ->
      ((Vec n (BitVector aw), Vec n (RegisterMeta aw)), (Vec n (BitVector aw), Vec n ()))
    go (_, (_, metas)) = ((offsets, metas), (offsets, repeat ()))
     where
      sizes = fmap (.nWords) metas
      offsets = snd $ mapAccumL (\acc size -> (acc + size, resize acc)) 0 sizes

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
    metaToRegisters o m = baseRegister : maybeToList lockRegister
     where
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

{- | Circuit writes always take priority over bus writes. Bus writes rejected with
an error if access rights are set to 'ReadOnly'. Similarly, bus reads are rejected
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
  ( HasCallStack
  , ToFieldType a
  , BitPackC a
  , BitPack a
  , NFDataX a
  , KnownDomain dom
  , KnownNat wordSize
  , KnownNat aw
  , Show a
  , 1 <= wordSize
  , ?busByteOrder :: ByteOrder
  , ?regByteOrder :: ByteOrder
  ) =>
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
            Dict ->
              Circuit go
        SNatGT ->
          -- Zero-width register that only provides bus activity information
          Circuit $ \(((_, _, m2s0), _), (_, ack)) ->
            let
              update m2s1 acknowledge
                | not (m2s1.strobe && m2s1.busCycle) = (Nothing, emptyWishboneS2M)
                | m2s1.writeEnable = (Just (BusWrite resetValue), emptyWishboneS2M{acknowledge})
                | otherwise =
                    ( Just (BusRead resetValue)
                    , (emptyWishboneS2M @(BitVector (wordSize * 8))){acknowledge, readData = 0}
                    )

              (unbundle -> (busActivity, s2m)) = update <$> m2s0 <*> coerce ack
             in
              ( (((), zeroWidthRegisterMeta @a Proxy regConfig, s2m), pure ())
              , (pure resetValue, busActivity)
              )
 where
  regByteOrder = ?regByteOrder
  needReverse = ?busByteOrder /= regByteOrder

  unpackC :: Vec (SizeInWordsC wordSize a) (Bytes wordSize) -> a
  unpackC packed = fromMaybe err . maybeUnpackWordC ?regByteOrder $ packed
   where
    err =
      deepErrorX
        [I.i|
        Unpack failed in registerWbDf:
          wordSize:     #{natToInteger @wordSize}
          regByteOrder: #{regByteOrder}
          packedOut:    #{packed}
      |]

  -- This behemoth of a type signature because the inferred type signature is
  -- too general, confusing the type checker.
  go ::
    forall nWords.
    ( nWords ~ SizeInWordsC wordSize a
    , KnownNat nWords
    , 1 <= nWords
    ) =>
    ( ( (Offset aw, (), Signal dom (WishboneM2S aw wordSize (Bytes wordSize)))
      , Signal dom (Maybe a)
      )
    , (Signal dom (), Signal dom Ack)
    ) ->
    ( ( ((), RegisterMeta aw, Signal dom (WishboneS2M (Bytes wordSize)))
      , Signal dom ()
      )
    , (Signal dom a, Signal dom (Maybe (BusActivity a)))
    )
  go (((offset, _, wbM2S), aIn0), (_, coerce -> dfAck)) =
    ((((), reg, wbS2M2), pure ()), (aOut, busActivity))
   where
    relativeOffset = goRelativeOffset . addr <$> wbM2S
    aOut = unpackC <$> packedOut
    packedIn0 = fmap (packWordC @wordSize ?regByteOrder) <$> aIn0

    -- Construct register meta data. This information is only used for simulation,
    -- i.e., used to build the register map.
    reg =
      RegisterMeta
        { name = SimOnly $ Name{name = regConfig.name, description = regConfig.description}
        , srcLoc = SimOnly locCaller
        , nWords = natToNum @nWords
        , register =
            SimOnly
              $ Register
                { fieldType = regType @a
                , address = 0x0 -- Note: will be set by 'deviceWithOffsetsWb'
                , access = regConfig.access
                , tags = regConfig.tags
                , reset = Just (pack resetValue).unsafeToNatural
                }
        }

    -- Construct output to the bus and the user logic. Note that the bus activity will
    -- get fed to the user directly, but the output to the bus will only go to the bus
    -- once the user acknowledges the bus activity.
    busActivity = getBusActivity <$> wbS2M0 <*> aOut <*> aInFromBus0
    aInFromBus0 = fmap unpackC <$> packedInFromBus0
    (wbS2M0, packedInFromBus0) =
      unbundle
        $ goBus regConfig.access
        <$> relativeOffset
        <*> wbM2S
        <*> packedOut

    -- Drop any circuit writes while we're waiting for the bus activity to get
    -- acknowledged. This makes sure that 'packedOut' remains stable in turn making
    -- 'busActivity' stable which is a requirement for Df. Note that if the user *does*
    -- acknowledge we *do* allow circuit writes. This allows users to intercept bus
    -- activity and change any incoming values atomically.
    packedIn1 = mux (fmap isJust busActivity .&&. fmap not dfAck) (pure Nothing) packedIn0

    -- Only write to the register from the bus if the bus activity gets acknowledged
    ackBusActivity = fmap isJust busActivity .&&. dfAck
    packedInFromBus1 = mux ackBusActivity packedInFromBus0 (pure Nothing)
    packedOut =
      regMaybe
        clk
        rst
        enableGen
        (packWordC regByteOrder resetValue)
        (liftA2 (<|>) packedIn1 packedInFromBus1)

    -- Only acknowledge bus transactions if the bus activity gets acknowledged. Note that
    -- 'ackBusActivity' is defined in such a way that is always 'False' if there is no
    -- transaction to ack in the first place. I.e., we don't accidentally acknowledge in
    -- case of errors.
    wbS2M1 = setAck <$> wbS2M0 <*> ackBusActivity

    setAck :: WishboneS2M c -> Bool -> WishboneS2M c
    setAck s2m acknowledge = s2m{acknowledge}

    -- Override the read data based on 'BusReadBehavior'
    --
    -- XXX: We use this to allow use to have a register backed by a FIFO. Ideally we'd
    --      just have a 'pipeDf' that is *not* backed by registers, but merely passes
    --      on a 'Df' stream.
    wbS2M2 =
      case regConfig.busRead of
        PreferCircuit -> setReadData <$> wbS2M1 <*> aIn0 <*> relativeOffset
        PreferRegister -> wbS2M1

    setReadData ::
      WishboneS2M (Bytes wordSize) ->
      Maybe a ->
      Index nWords ->
      WishboneS2M (Bytes wordSize)
    setReadData s2m ma relOffset =
      case ma of
        Just a ->
          let
            packed = packWordC @wordSize ?regByteOrder a !! relOffset
            readData = if needReverse then reverseBytes packed else packed
           in
            s2m{readData}
        Nothing -> s2m

    goRelativeOffset :: BitVector aw -> Index nWords
    goRelativeOffset addr = fromMaybe 0 (elemIndex addrLsbs expectedOffsets)
     where
      expectedOffsets = iterateI succ offsetLsbs
      offsetLsbs = resize offset :: BitVector (BitSize (Index nWords))
      addrLsbs = resize addr :: BitVector (BitSize (Index nWords))

  -- Handle bus transactions. Note that this function assumes that the bus transaction
  -- is actually targeting this register. I.e., the address has already been checked.
  goBus ::
    forall nWords.
    ( nWords ~ SizeInWordsC wordSize a
    , KnownNat nWords
    ) =>
    Access ->
    Index nWords ->
    WishboneM2S aw wordSize (Bytes wordSize) ->
    Vec (SizeInWordsC wordSize a) (Bytes wordSize) ->
    ( WishboneS2M (Bytes wordSize)
    , Maybe (Vec (SizeInWordsC wordSize a) (Bytes wordSize))
    )
  goBus busAccess offset wbM2S packedFromReg =
    ()
      `seqX` offset
      `seqX` wbM2S
      `seqX` packedFromReg
      `seqX` readData
      `seqX` acknowledge
      `seqX` err
      `seqX` wbWrite
      `seqX` ( WishboneS2M
                { readData
                , acknowledge
                , err
                , retry = False
                , stall = False
                }
             , wbWrite
             )
   where
    managerActive = wbM2S.strobe && wbM2S.busCycle
    err = managerActive && accessFault
    acknowledge = managerActive && not err

    -- Note that these "faults" are only considered in context of an active bus
    readOnlyFault = busAccess == ReadOnly && wbM2S.writeEnable
    writeOnlyFault = busAccess == WriteOnly && not wbM2S.writeEnable
    accessFault = readOnlyFault || writeOnlyFault

    readData
      | not wbM2S.writeEnable
      , acknowledge =
          (if needReverse then reverseBytes else id) (packedFromReg !! offset)
      | otherwise = 0

    maskedWriteData =
      maskWriteData
        offset
        (if needReverse then reverseBits wbM2S.busSelect else wbM2S.busSelect)
        (if needReverse then reverseBytes wbM2S.writeData else wbM2S.writeData)
        packedFromReg

    wbWrite
      | wbM2S.writeEnable
      , acknowledge =
          Just maskedWriteData
      | otherwise = Nothing

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

-- | Like 'registerWbDf', but returns a 'CSignal' of 'Maybe' instead of a 'Df' stream.
registerWb ::
  forall a dom wordSize aw.
  ( HasCallStack
  , ToFieldType a
  , BitPackC a
  , BitPack a
  , NFDataX a
  , KnownDomain dom
  , KnownNat wordSize
  , KnownNat aw
  , Show a
  , 1 <= wordSize
  , ?busByteOrder :: ByteOrder
  , ?regByteOrder :: ByteOrder
  ) =>
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
    , CSignal dom (Maybe (BusActivity a))
    )
registerWb clk rst regConfig resetValue = circuit $ \i -> do
  (regValue, busActivityDf) <- registerWbDf clk rst regConfig resetValue -< i
  busActivity <- Df.toMaybe -< busActivityDf
  idC -< (regValue, busActivity)

-- | Like 'registerWbDf', but does not return the register value.
registerWb_ ::
  forall a dom wordSize aw.
  ( HasCallStack
  , ToFieldType a
  , BitPackC a
  , BitPack a
  , NFDataX a
  , KnownDomain dom
  , KnownNat wordSize
  , KnownNat aw
  , Show a
  , 1 <= wordSize
  , ?busByteOrder :: ByteOrder
  , ?regByteOrder :: ByteOrder
  ) =>
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
    ()
registerWb_ clk rst regConfig resetValue = circuit $ \i -> do
  _ignored <- registerWbDf clk rst regConfig resetValue -< i
  idC

{- | Same as 'registerWb', but also takes an offset. You can tie registers
created using this function together with 'deviceWithOffsetsWb'.
-}
registerWithOffsetWb ::
  forall a dom wordSize aw.
  ( HasCallStack
  , ToFieldType a
  , BitPackC a
  , BitPack a
  , NFDataX a
  , KnownDomain dom
  , KnownNat wordSize
  , KnownNat aw
  , Show a
  , BitSize a <= 8 * wordSize
  , 1 <= wordSize
  , ?busByteOrder :: ByteOrder
  , ?regByteOrder :: ByteOrder
  ) =>
  Clock dom ->
  Reset dom ->
  -- | Configuration values
  RegisterConfig ->
  -- | Offset
  BitVector aw ->
  -- | Reset value
  a ->
  Circuit
    ( RegisterWithOffsetWb dom aw wordSize
    , CSignal dom (Maybe a)
    )
    ( CSignal dom a
    , CSignal dom (Maybe (BusActivity a))
    )
registerWithOffsetWb clk rst regConfig offset resetValue = circuit $ \i -> do
  (regValue, busActivityDf) <-
    registerWithOffsetWbDf clk rst regConfig offset resetValue -< i
  busActivity <- Df.toMaybe -< busActivityDf
  idC -< (regValue, busActivity)

{- | Same as 'registerWbDf', but also takes an offset. You can tie registers
created using this function together with 'deviceWithOffsetsWb'.
-}
registerWithOffsetWbDf ::
  forall a dom wordSize aw.
  ( HasCallStack
  , ToFieldType a
  , BitPackC a
  , BitPack a
  , NFDataX a
  , KnownDomain dom
  , KnownNat wordSize
  , KnownNat aw
  , Show a
  , BitSize a <= 8 * wordSize
  , 1 <= wordSize
  , ?busByteOrder :: ByteOrder
  , ?regByteOrder :: ByteOrder
  ) =>
  Clock dom ->
  Reset dom ->
  -- | Configuration values
  RegisterConfig ->
  -- | Offset
  BitVector aw ->
  -- | Reset value
  a ->
  Circuit
    ( RegisterWithOffsetWb dom aw wordSize
    , CSignal dom (Maybe a)
    )
    ( CSignal dom a
    , Df dom (BusActivity a)
    )
registerWithOffsetWbDf clk rst regConfig offset resetValue =
  circuit $ \((offsetBwd, meta, wb), maybeA) -> do
    genOffset -< offsetBwd
    registerWbDf clk rst regConfig resetValue -< ((Fwd offset, meta, wb), maybeA)
 where
  genOffset :: Circuit (ConstBwd (BitVector aw)) ()
  genOffset = Circuit $ \_ -> (offset, ())

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

-- | 'registerWb' with a hidden clock and reset
registerWbI ::
  forall a dom wordSize aw.
  ( HasCallStack
  , HiddenClock dom
  , HiddenReset dom
  , ToFieldType a
  , BitPackC a
  , BitPack a
  , NFDataX a
  , KnownNat wordSize
  , KnownNat aw
  , Show a
  , 1 <= wordSize
  , ?busByteOrder :: ByteOrder
  , ?regByteOrder :: ByteOrder
  ) =>
  -- | Configuration values
  RegisterConfig ->
  -- | Reset value
  a ->
  Circuit
    ( RegisterWb dom aw wordSize
    , CSignal dom (Maybe a)
    )
    ( CSignal dom a
    , CSignal dom (Maybe (BusActivity a))
    )
registerWbI = withFrozenCallStack $ registerWb hasClock hasReset

-- | 'registerWbDf' with a hidden clock and reset
registerWbDfI ::
  forall a dom wordSize aw.
  ( HasCallStack
  , HiddenClock dom
  , HiddenReset dom
  , ToFieldType a
  , BitPackC a
  , BitPack a
  , NFDataX a
  , KnownNat wordSize
  , KnownNat aw
  , Show a
  , 1 <= wordSize
  , ?busByteOrder :: ByteOrder
  , ?regByteOrder :: ByteOrder
  ) =>
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
registerWbDfI = withFrozenCallStack $ registerWbDf hasClock hasReset

-- | 'registerWbDf_' with a hidden clock and reset
registerWbI_ ::
  forall a dom wordSize aw.
  ( HasCallStack
  , HiddenClock dom
  , HiddenReset dom
  , ToFieldType a
  , BitPackC a
  , BitPack a
  , NFDataX a
  , KnownNat wordSize
  , KnownNat aw
  , Show a
  , 1 <= wordSize
  , ?busByteOrder :: ByteOrder
  , ?regByteOrder :: ByteOrder
  ) =>
  -- | Configuration values
  RegisterConfig ->
  -- | Reset value
  a ->
  Circuit
    ( RegisterWb dom aw wordSize
    , CSignal dom (Maybe a)
    )
    ()
registerWbI_ = withFrozenCallStack $ registerWb_ hasClock hasReset

-- | 'registerWithOffsetWb' with a hidden clock and reset
registerWithOffsetWbI ::
  forall a dom wordSize aw.
  ( HasCallStack
  , HiddenClock dom
  , HiddenReset dom
  , ToFieldType a
  , BitPackC a
  , BitPack a
  , NFDataX a
  , KnownNat wordSize
  , KnownNat aw
  , Show a
  , BitSize a <= 8 * wordSize
  , 1 <= wordSize
  , ?busByteOrder :: ByteOrder
  , ?regByteOrder :: ByteOrder
  ) =>
  -- | Configuration values
  RegisterConfig ->
  -- | Offset
  BitVector aw ->
  -- | Reset value
  a ->
  Circuit
    ( RegisterWithOffsetWb dom aw wordSize
    , CSignal dom (Maybe a)
    )
    ( CSignal dom a
    , CSignal dom (Maybe (BusActivity a))
    )
registerWithOffsetWbI = withFrozenCallStack $ registerWithOffsetWb hasClock hasReset

-- | 'registerWithOffsetWbDf' with a hidden clock and reset
registerWithOffsetWbDfI ::
  forall a dom wordSize aw.
  ( HasCallStack
  , HiddenClock dom
  , HiddenReset dom
  , ToFieldType a
  , BitPackC a
  , BitPack a
  , NFDataX a
  , KnownNat wordSize
  , KnownNat aw
  , Show a
  , BitSize a <= 8 * wordSize
  , 1 <= wordSize
  , ?busByteOrder :: ByteOrder
  , ?regByteOrder :: ByteOrder
  ) =>
  -- | Configuration values
  RegisterConfig ->
  -- | Offset
  BitVector aw ->
  -- | Reset value
  a ->
  Circuit
    ( RegisterWithOffsetWb dom aw wordSize
    , CSignal dom (Maybe a)
    )
    ( CSignal dom a
    , Df dom (BusActivity a)
    )
registerWithOffsetWbDfI = withFrozenCallStack $ registerWithOffsetWbDf hasClock hasReset
