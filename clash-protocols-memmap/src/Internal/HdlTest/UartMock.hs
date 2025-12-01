-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}
--
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -fplugin Protocols.Plugin #-}

{- | Internal module used to test whether memory mapped circuits generate
valid HDL.

This contains mostly non-sense code to test various features.
-}
module Internal.HdlTest.UartMock where

import Clash.Prelude

import Clash.Class.BitPackC
import GHC.Stack (HasCallStack, callStack, getCallStack)
import Protocols (CSignal, Circuit (..))
import Protocols.MemoryMap (
  Access (ReadWrite),
  Address,
  DeviceDefinition (DeviceDefinition),
  MemoryMap (..),
  MemoryMapTree (DeviceInstance, Interconnect),
  Mm,
  Name (Name),
  NamedLoc (..),
  Register (Register, access, address, fieldType, reset, tags),
  ToConstBwd,
  locHere,
  mergeDeviceDefs,
  regType,
  withAbsAddr,
  withMemoryMap,
  withName,
  withPrefix,
  withTag,
 )
import Protocols.MemoryMap.TypeDescription
import Protocols.Wishbone (
  Wishbone,
  WishboneM2S (..),
  WishboneMode (Standard),
  WishboneS2M (..),
  emptyWishboneM2S,
  emptyWishboneS2M,
 )

import qualified Data.Map.Strict as Map

data FakeyTypy a b = FakeyTypy a b
  deriving (Generic, BitPackC)

data FakeType a b
  = FakeA a
  | FakeB b
  deriving (Generic, BitPackC)
deriveTypeDescription ''FakeType

magicUart ::
  (HasCallStack, HiddenClockResetEnable dom, KnownNat addrWidth) =>
  Circuit (ToConstBwd Mm, Wishbone dom 'Standard addrWidth (BitVector 32)) ()
magicUart = Circuit go
 where
  ((_, callLoc) : _) = getCallStack callStack

  go ::
    (((), Signal dom (WishboneM2S addrWidth 4 (BitVector 32))), ()) ->
    ((SimOnly MemoryMap, Signal dom (WishboneS2M (BitVector 32))), ())
  go (((), _m2s), ()) = ((SimOnly memoryMap, pure s2m), ())
   where
    deviceName = "MagicUART"
    memoryMap =
      MemoryMap
        { deviceDefs = Map.singleton deviceName deviceDef
        , tree = DeviceInstance callLoc deviceName
        }
    s2m = emptyWishboneS2M
    deviceDef =
      DeviceDefinition
        (Name deviceName "")
        [ NamedLoc
            { name = Name "test" ""
            , loc = locHere
            , value =
                Register
                  { tags = []
                  , reset = Nothing
                  , fieldType = regType @(FakeType Bool (BitVector 32))
                  , address = 0
                  , access = ReadWrite
                  }
            }
        , NamedLoc
            { name = Name "test" ""
            , loc = locHere
            , value =
                Register
                  { tags = []
                  , reset = Nothing
                  , fieldType = regType @(FakeType Bool (BitVector 32))
                  , address = 0
                  , access = ReadWrite
                  }
            }
        , NamedLoc
            { name = Name "another_test" ""
            , loc = locHere
            , value =
                Register
                  { tags = []
                  , reset = Nothing
                  , fieldType = regType @(Either Bool (BitVector 32))
                  , address = 0
                  , access = ReadWrite
                  }
            }
        ]
        callLoc
        []

someCircuit ::
  (HasCallStack, HiddenClockResetEnable dom, HasCallStack) =>
  Circuit (ToConstBwd Mm, Wishbone dom 'Standard 32 (BitVector 32)) ()
someCircuit = circuit $ \(mm, master) -> do
  [a, b] <- interconnect -< (mm, master)
  withPrefix 0b01 (withName "A" magicUart) -< a
  withPrefix 0b00 (withName "B" magicUart) -< b

someCircuit' ::
  (HasCallStack, HiddenClockResetEnable dom) =>
  Circuit (ToConstBwd Mm, Wishbone dom 'Standard 32 (BitVector 32)) ()
someCircuit' = withName "top" $ circuit $ \(mm, master) -> do
  [a, b] <- interconnectImplicit -< (mm, master)
  withName "A" magicUart -< a
  withName "B" magicUart -< b

someOtherCircuit ::
  (HasCallStack, HiddenClockResetEnable dom) =>
  Circuit
    ( ToConstBwd Mm
    , ( Wishbone dom 'Standard 32 (BitVector 32)
      , CSignal dom Bit
      )
    )
    (CSignal dom Bit)
someOtherCircuit = withName "MyCircuit" $ circuit $ \(mm, (master, rx)) -> do
  [(prA, (mmA, a)), (prB, (mmB, b)), (prI, (mmI, i))] <- interconnect -< (mm, master)

  withPrefix 0b00
    $ withName "fakeUART"
    $ withTag "no-generate"
    $ withAbsAddr
      0x0000_0000
      magicUart
    -< (prB, (mmB, b))

  (tx, _status) <-
    withPrefix 0b01
      $ withName "realUART"
      $ withAbsAddr 0x4000_0000
      $ moreRealUart' d16 d16
      -< (prA, (mmA, (a, rx)))

  [(prC, (mmC, c))] <- withPrefix 0b10 interconnect -< (prI, (mmI, i))
  withPrefix 0b00 $ withAbsAddr 0x8000_0000 $ magicUart -< (prC, (mmC, c))

  idC -< tx

moreRealUart' ::
  forall dom addrW nBytes transmitBufferDepth receiveBufferDepth.
  ( HiddenClockResetEnable dom
  , 1 <= transmitBufferDepth
  , 1 <= receiveBufferDepth
  , KnownNat addrW
  , KnownNat nBytes
  , HasCallStack
  ) =>
  SNat transmitBufferDepth ->
  SNat receiveBufferDepth ->
  Circuit
    (ToConstBwd Mm, (Wishbone dom 'Standard addrW (BitVector (8 * nBytes)), CSignal dom Bit))
    (CSignal dom Bit, CSignal dom (Bool, Bool))
moreRealUart' tD rD = withMemoryMap mm (moreRealUart tD rD)
 where
  ((_, callLoc) : _) = getCallStack callStack
  deviceName = "MoreRealUART"
  mm =
    MemoryMap
      { deviceDefs = Map.singleton deviceName deviceDef
      , tree = DeviceInstance callLoc deviceName
      }
  deviceDef = DeviceDefinition (Name deviceName "") [] callLoc []

moreRealUart ::
  forall dom addrW nBytes transmitBufferDepth receiveBufferDepth.
  ( HiddenClockResetEnable dom
  , 1 <= transmitBufferDepth
  , 1 <= receiveBufferDepth
  , KnownNat addrW
  , KnownNat nBytes
  -- , 1 <= nBytes
  ) =>
  -- | Recommended value: 16. This seems to be a good balance between resource
  -- usage and usability.
  SNat transmitBufferDepth ->
  -- | Recommended value: 16. This seems to be a good balance between resource
  -- usage and usability.
  SNat receiveBufferDepth ->
  Circuit
    (Wishbone dom 'Standard addrW (BitVector (8 * nBytes)), CSignal dom Bit)
    (CSignal dom Bit, CSignal dom (Bool, Bool))
moreRealUart SNat SNat = Circuit go
 where
  go ::
    ( ( Signal dom (WishboneM2S addrW (Div (8 * nBytes + 7) 8) (BitVector (8 * nBytes)))
      , Signal dom Bit
      )
    , ((), ())
    ) ->
    ( ( Signal dom (WishboneS2M (BitVector (8 * nBytes)))
      , ()
      )
    , (Signal dom Bit, Signal dom (Bool, Bool))
    )
  go ((m2s, rx), (_, _)) = ((s2m, ()), (tx, status))
   where
    s2m = pure emptyWishboneS2M
    tx = pure low
    status = pure (False, False)

interconnectImplicit ::
  forall dom addrWidth a n.
  ( HasCallStack
  , KnownNat n
  , KnownNat addrWidth
  , NFDataX a
  , KnownNat (BitSize a)
  , (CLog 2 n <= addrWidth)
  , 1 <= n
  , KnownNat (addrWidth - CLog 2 n)
  ) =>
  Circuit
    ( ToConstBwd Mm
    , Wishbone dom 'Standard addrWidth a
    )
    ( Vec
        n
        ( ToConstBwd Mm
        , Wishbone dom 'Standard (addrWidth - CLog 2 n) a
        )
    )
interconnectImplicit = error ""

interconnect ::
  forall dom addrWidth a n.
  ( HasCallStack
  , KnownNat n
  , KnownNat addrWidth
  , NFDataX a
  , KnownNat (BitSize a)
  , (CLog 2 n <= addrWidth)
  , 1 <= n
  , KnownNat (addrWidth - CLog 2 n)
  ) =>
  Circuit
    ( ToConstBwd Mm
    , Wishbone dom 'Standard addrWidth a
    )
    ( Vec
        n
        ( ToConstBwd (BitVector (CLog 2 n))
        , ( ToConstBwd Mm
          , Wishbone dom 'Standard (addrWidth - CLog 2 n) a
          )
        )
    )
interconnect = Circuit go
 where
  (_, loc) : _ = getCallStack callStack

  go ::
    ( ((), Signal dom (WishboneM2S addrWidth (Div (BitSize a + 7) 8) a))
    , Vec n (BitVector (CLog 2 n), (SimOnly MemoryMap, Signal dom (WishboneS2M a)))
    ) ->
    ( (SimOnly MemoryMap, Signal dom (WishboneS2M a))
    , Vec
        n
        ((), ((), Signal dom (WishboneM2S (addrWidth - CLog 2 n) (Div (BitSize a + 7) 8) a)))
    )
  go (((), m2s), unzip -> (prefs, unzip -> (mms, s2ms))) = ((SimOnly memoryMap, s2m), m2ss)
   where
    memoryMap =
      MemoryMap
        { deviceDefs = mergeDeviceDefs ((.deviceDefs) . unSim <$> toList mms)
        , tree = Interconnect loc (toList descs)
        }
    descs = zip relAddrs ((.tree) . unSim <$> mms)
    relAddrs = prefixToAddr <$> prefs
    unSim (SimOnly x) = x

    (unbundle -> (s2m, m2ss0)) = inner prefs <$> m2s <*> bundle s2ms
    m2ss = (\x -> ((), ((), x))) <$> unbundle m2ss0

    prefixToAddr :: BitVector (CLog 2 n) -> Address
    prefixToAddr prefix = toInteger prefix `shiftL` fromInteger shift'
     where
      shift' = snatToInteger $ SNat @(addrWidth - CLog 2 n)

  inner ::
    Vec n (BitVector (CLog 2 n)) ->
    WishboneM2S addrWidth (Div (BitSize a + 7) 8) a ->
    Vec n (WishboneS2M a) ->
    (WishboneS2M a, Vec n (WishboneM2S (addrWidth - CLog 2 n) (Div (BitSize a + 7) 8) a))
  inner prefixes m2s@WishboneM2S{..} s2ms
    | not (busCycle && strobe) = (emptyWishboneS2M, m2ss)
    -- no valid index selected
    | Nothing <- selected = (emptyWishboneS2M{err = True}, m2ss)
    | Just idx <- selected = (s2ms !! idx, replace idx m2sStripped m2ss)
   where
    m2ss = repeat emptyWishboneM2S
    m2sStripped = m2s{addr = internalAddr}
    (compIdx, internalAddr) = split @_ @(CLog 2 n) @(addrWidth - CLog 2 n) addr
    selected = elemIndex compIdx prefixes
