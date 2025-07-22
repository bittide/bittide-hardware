-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{- | Utilities for creating Wishbone devices and registers, while also creating
a memory map.
-}
module Protocols.MemoryMap.Registers.WishboneStandard (
  -- * Device creation
  deviceWbC,
  deviceWithOffsetsWbC,
  -- Register creation (explicit clocks and resets)
  registerWbC,
  registerWbC_,
  registerWithOffsetWbC,

  -- * Register creation (implicit clocks and resets)
  registerWbCI,
  registerWbCI_,
  registerWithOffsetWbCI,

  -- * Supporting types and functions
  registerConfig,
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
import Data.Constraint (Dict (Dict))
import Data.Constraint.Nat.Lemmas (divWithRemainder)
import Data.Maybe (catMaybes, fromMaybe)
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
import qualified Protocols.Vec as V

data BusActivity a = BusIdle | BusRead a | BusWrite a
  deriving (Show, Eq, Functor, Generic, NFDataX)

data DeviceConfig = DeviceConfig
  { name :: String
  }

type Offset aw = BitVector aw

data RegisterMeta aw = RegisterMeta
  { name :: SimOnly Name
  , srcLoc :: SimOnly SrcLoc
  , register :: SimOnly Register
  , nWords :: BitVector (aw + 1)
  }

zeroWidthRegisterMeta :: (KnownNat aw) => RegisterMeta aw
zeroWidthRegisterMeta =
  RegisterMeta
    { name = SimOnly (Name{name = "", description = ""})
    , srcLoc = SimOnly locHere
    , register =
        SimOnly
          Register
            { -- XXX: There is no regType for unit, so we make it something else
              fieldType = regType @(Signed 0)
            , address = 0x0
            , access = ReadWrite
            , tags = []
            , reset = Nothing
            }
    , nWords = 0
    }

data RegisterConfig = RegisterConfig
  { name :: String
  , description :: String
  , tags :: [String]
  , access :: Access
  }
  deriving (Show)

registerConfig :: String -> RegisterConfig
registerConfig name =
  RegisterConfig
    { name
    , description = ""
    , tags = []
    , access = ReadWrite
    }

-- These have no business being in this module :)
replaceWith :: (KnownNat n) => Index n -> (a -> a) -> Vec n a -> Vec n a
replaceWith i f xs = replace i (f (xs !! i)) xs

{- | Tie a bunch of registers together to form a device. Note that @aw@ must be
chosen such that it can hold the addresses of all registers, including their
maximum byte address.

Example usage:

> deviceExample clk rst = circuit $ \(mm, wb) -> do
>   [reg1, reg2, reg3] <- deviceWbC "example" -< (mm, wb)
>
>   _reg1o <- registerWbC clk rst (registerConfig "float") (0.0 :: Float)     -< (reg1, Fwd noWrite)
>   _reg2o <- registerWbC clk rst (registerConfig "u16")   (0 :: Unsigned 16) -< (reg2, Fwd noWrite)
>   _reg3o <- registerWbC clk rst (registerConfig "bool")  (False :: Bool)    -< (reg3, Fwd noWrite)
>
>   idC
>  where
>   noWrite = pure Nothing
-}
deviceWbC ::
  forall n wordSize aw dom.
  ( HasCallStack
  , KnownNat n
  , KnownNat wordSize
  , KnownNat aw
  ) =>
  String ->
  Circuit
    ( ConstBwd MM
    , Wishbone dom 'Standard aw (Bytes wordSize)
    )
    ( Vec
        n
        ( ConstFwd (Offset aw)
        , ConstBwd (RegisterMeta aw)
        , Wishbone dom 'Standard aw (Bytes wordSize)
        )
    )
deviceWbC deviceName = circuit $ \(mm, wb) -> do
  (offsets0, metas0, wbs) <-
    V.unzip3 <| withFrozenCallStack deviceWithOffsetsWbC deviceName -< (mm, wb)
  (offsets1, metas1) <- genOffsets -< (offsets0, metas0)
  V.zip3 -< (offsets1, metas1, wbs)
 where
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

{- | Like 'deviceWbC', but allows you to set offsets of registers manually. This
can be important in cases where you'd like gaps between the registers. You can
either pass in the offsets manually or use 'registerWithOffsetWbC'.
-}
deviceWithOffsetsWbC ::
  forall n wordSize aw dom.
  (HasCallStack, KnownNat n, KnownNat aw, KnownNat wordSize) =>
  String ->
  Circuit
    ( ConstBwd MM
    , Wishbone dom 'Standard aw (Bytes wordSize)
    )
    ( Vec
        n
        ( ConstBwd (Offset aw)
        , ConstBwd (RegisterMeta aw)
        , Wishbone dom 'Standard aw (Bytes wordSize)
        )
    )
deviceWithOffsetsWbC deviceName =
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
    metaToRegister :: Offset aw -> RegisterMeta aw -> Maybe (NamedLoc Register)
    metaToRegister o m
      | m.nWords == 0 = Nothing
      | otherwise =
          Just
            $ NamedLoc
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
                , registers = catMaybes (L.zipWith metaToRegister (toList offsets) (toList metas))
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
      BitVector aw ->
      -- Subordinate meta data
      RegisterMeta aw ->
      -- Current address on the bus
      BitVector aw ->
      -- Active?
      Bool
    isActiveSubordinate offset RegisterMeta{nWords} addr
      -- Zero-width registers can never be selected -- they don't have an address
      | nWords == 0 = False
      -- Optimization case of single word registers: no need to involve `Ord`
      | nWords == 1 = offset == addr
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

You can tie registers created using this function together with 'deviceWbC'. If
you're looking to create a device with arbitrary offsets, use
'registerWithOffsetWbC' instead.

The register is configurable in its byte order, both on the bus and internally
using '?busByteOrder' and '?regByteOrder' respectively. For VexRiscV, you'd want
to configure @?busByteOrder = BigEndian@ and @?regByteOrder = LittleEndian@. Note
that the bus byte order is a hack more than anything else and only affects the
data and byte enable fields.
-}
registerWbC ::
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
    ( ( ConstFwd (Offset aw)
      , ConstBwd (RegisterMeta aw)
      , Wishbone dom 'Standard aw (Bytes wordSize)
      )
    , CSignal dom (Maybe a)
    )
    ( CSignal dom a
    , CSignal dom (BusActivity a)
    )
registerWbC clk rst regConfig resetValue =
  case SNat @(SizeInWordsC wordSize a) of
    (nWords@SNat :: SNat nWords) ->
      case d1 `compareSNat` nWords of
        SNatLE ->
          case divWithRemainder @wordSize @8 @7 of
            Dict ->
              Circuit go
        SNatGT ->
          Circuit $ \_ ->
            ( (((), zeroWidthRegisterMeta, pure emptyWishboneS2M), pure ())
            , (pure resetValue, pure BusIdle)
            )
 where
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
    , (Signal dom (), Signal dom ())
    ) ->
    ( ( ((), RegisterMeta aw, Signal dom (WishboneS2M (Bytes wordSize)))
      , Signal dom ()
      )
    , (Signal dom a, Signal dom (BusActivity a))
    )
  go (((offset, _, wbM2S), aIn), (_, _)) =
    ((((), reg, wbS2M), pure ()), (aOut, busActivity))
   where
    relativeOffset = goRelativeOffset . addr <$> wbM2S

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
                , address = 0x0 -- Note: will be set by 'deviceWithOffsetsWbC'
                , access = regConfig.access
                , tags = regConfig.tags
                , reset = Just (pack resetValue).unsafeToNatural
                }
        }

    -- Construct hardware for register. The bulk of this is handled by 'goReg',
    -- which acts combinationally on the inputs
    aOut = regMaybe clk rst enableGen resetValue (liftA2 (<|>) aIn aInFromBus)
    busActivity = getBusActivity <$> wbS2M <*> aOut <*> aInFromBus
    (wbS2M, aInFromBus) =
      unbundle
        $ goReg regConfig.access
        <$> relativeOffset
        <*> wbM2S
        <*> aOut

    goRelativeOffset :: BitVector aw -> Index nWords
    goRelativeOffset addr = fromMaybe 0 (elemIndex addrLsbs expectedOffsets)
     where
      expectedOffsets = iterateI succ offsetLsbs
      offsetLsbs = resize offset :: BitVector (BitSize (Index nWords))
      addrLsbs = resize addr :: BitVector (BitSize (Index nWords))

  goReg ::
    forall nWords.
    ( 1 <= nWords
    , nWords ~ SizeInWordsC wordSize a
    , KnownNat nWords
    ) =>
    Access ->
    Index nWords ->
    WishboneM2S aw wordSize (Bytes wordSize) ->
    a ->
    (WishboneS2M (Bytes wordSize), Maybe a)
  goReg busAccess offset wbM2S aFromReg =
    ()
      `seqX` offset
      `seqX` wbM2S
      `seqX` aFromReg
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

    wordSize = SNat @wordSize

    readData
      | not wbM2S.writeEnable
      , acknowledge =
          (if needReverse then reverseBytes else id)
            $ packWordC ?regByteOrder aFromReg
            !! offset
      | otherwise = 0

    needReverse = ?busByteOrder /= ?regByteOrder

    maskedWriteData =
      maskWriteData
        offset
        (if needReverse then reverseBits wbM2S.busSelect else wbM2S.busSelect)
        (if needReverse then reverseBytes wbM2S.writeData else wbM2S.writeData)
        (packWordC ?regByteOrder aFromReg)

    wbWrite
      | wbM2S.writeEnable
      , acknowledge =
          Just
            -- TODO: Handle unpack failures
            $ fromMaybe
              ( deepErrorX
                  $ "Unpack failed in registerWbC: wordSize="
                  <> show wordSize
                  <> ", regByteOrder="
                  <> show ?regByteOrder
                  <> ", maskedWriteData="
                  <> show maskedWriteData
              )
            $ maybeUnpackWordC ?regByteOrder maskedWriteData
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

-- | Like 'registerWbC', but does not return the register value.
registerWbC_ ::
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
    ( ( ConstFwd (Offset aw)
      , ConstBwd (RegisterMeta aw)
      , Wishbone dom 'Standard aw (Bytes wordSize)
      )
    , CSignal dom (Maybe a)
    )
    ()
registerWbC_ clk rst regConfig resetValue = circuit $ \i -> do
  _ignored <- registerWbC clk rst regConfig resetValue -< i
  idC

{- | Same as 'registerWbC', but also takes an offset. You can tie registers
created using this function together with 'deviceWithOffsetsWbC'.
-}
registerWithOffsetWbC ::
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
    ( ( ConstBwd (BitVector aw)
      , ConstBwd (RegisterMeta aw)
      , Wishbone dom 'Standard aw (Bytes wordSize)
      )
    , CSignal dom (Maybe a)
    )
    ( CSignal dom a
    , CSignal dom (BusActivity a)
    )
registerWithOffsetWbC clk rst regConfig offset resetValue =
  circuit $ \((offsetBwd, meta, wb), maybeA) -> do
    genOffset -< offsetBwd
    registerWbC clk rst regConfig resetValue -< ((Fwd offset, meta, wb), maybeA)
 where
  genOffset :: Circuit (ConstBwd (BitVector aw)) ()
  genOffset = Circuit $ \_ -> (offset, ())

maskWriteData ::
  forall wordSize nWords.
  (KnownNat wordSize, KnownNat nWords, 1 <= nWords) =>
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

{- | 'registerWbC' already checks for illegal write or read operations and does not
acknowledge them. So there is no need to check for 'ReadOnly' or 'WriteOnly'.
-}
getBusActivity :: WishboneS2M bv -> a -> Maybe a -> BusActivity a
getBusActivity s2m regValue maybeUpdatedRegValue
  | not s2m.acknowledge = BusIdle
  | Just v <- maybeUpdatedRegValue = BusWrite v
  | otherwise = BusRead regValue

-- | 'registerWbC' with a hidden clock and reset
registerWbCI ::
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
    ( ( ConstFwd (Offset aw)
      , ConstBwd (RegisterMeta aw)
      , Wishbone dom 'Standard aw (Bytes wordSize)
      )
    , CSignal dom (Maybe a)
    )
    ( CSignal dom a
    , CSignal dom (BusActivity a)
    )
registerWbCI = withFrozenCallStack $ registerWbC hasClock hasReset

-- | 'registerWbC_' with a hidden clock and reset
registerWbCI_ ::
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
    ( ( ConstFwd (Offset aw)
      , ConstBwd (RegisterMeta aw)
      , Wishbone dom 'Standard aw (Bytes wordSize)
      )
    , CSignal dom (Maybe a)
    )
    ()
registerWbCI_ = withFrozenCallStack $ registerWbC_ hasClock hasReset

-- | 'registerWithOffsetWbC' with a hidden clock and reset
registerWithOffsetWbCI ::
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
    ( ( ConstBwd (BitVector aw)
      , ConstBwd (RegisterMeta aw)
      , Wishbone dom 'Standard aw (Bytes wordSize)
      )
    , CSignal dom (Maybe a)
    )
    ( CSignal dom a
    , CSignal dom (BusActivity a)
    )
registerWithOffsetWbCI = withFrozenCallStack $ registerWithOffsetWbC hasClock hasReset
