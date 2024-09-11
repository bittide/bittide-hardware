-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}
--
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
-- {-# OPTIONS -fplugin-opt=Protocols.Plugin:debug #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=20 #-}
{-# OPTIONS_GHC -fplugin Protocols.Plugin #-}

{- | Internal module used to test whether memory mapped circuits generate
valid HDL.

This contains mostly non-sense code to test various features.
-}
module Internal.HdlTest.UartMock where

import Clash.Annotations.TH
import Clash.Prelude

import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)

import Protocols (Circuit (..), toSignals)
import Protocols.Wishbone (
  Wishbone,
  WishboneM2S (..),
  WishboneMode (Standard),
  WishboneS2M (..),
  emptyWishboneM2S,
  emptyWishboneS2M,
 )

import BitPackC
import GHC.Stack (HasCallStack, callStack, getCallStack)
import Protocols.MemoryMap
import Protocols.MemoryMap.FieldType hiding (name)

someCircuit ::
  (HasCallStack, HiddenClockResetEnable dom, HasCallStack) =>
  Circuit (MemoryMapped (Wishbone dom 'Standard 32 (BitVector 32))) ()
someCircuit = circuit $ \master -> do
  [iMem, dMem, io] <- interconnect (Just 0x0000_0000) -< master
  withPrefix
    0b01
    (memory "instruction_memory" (Just 0x4000_0000) ReadOnly (SNat @(128 * 1024)))
    -< iMem
  withPrefix 0b10 (memory "data_memory" (Just 0x8000_0000) ReadWrite (SNat @(128 * 1024)))
    -< dMem

  [uartEchoBus, uartProgBus, timerBus] <-
    withPrefix 0b00 (interconnect (Just 0x0000_0000)) -< io
  withPrefix 0b00 (uart "UART_ECHO" (Just 0x0000_0000)) -< uartEchoBus
  withPrefix 0b01 (uart "UART_PROG" (Just 0x1000_0000)) -< uartProgBus
  withPrefix 0b10 (timer "global_timer" (Just 0x2000_0000)) -< timerBus

data UartStatus
  = Transmitting
  | Full
  | Ready
  deriving (NFDataX, Generic, BitPackC, ToFieldType)

data SomeGenericType a
  = NoData
  | Pending a
  | Done (Either (BitVector 8) a)
  deriving (NFDataX, Generic)

instance
  ( BitPackC a
  , Mod 1 (Max 1 (AlignmentC a)) <= 1
  , 1 <= Max (AlignmentC a) (Max 1 (AlignmentC a))
  , Mod 1 (AlignmentC a) <= AlignmentC a
  , Mod 1 (Max 1 (AlignmentC a)) <= Max 1 (AlignmentC a)
  , Mod 1 (AlignmentC a) <= 1
  , Mod 1 (Max 1 (Max (AlignmentC a) (Max 1 (AlignmentC a)))) <= 1
  , Mod 1 (Max 1 (Max (AlignmentC a) (Max 1 (AlignmentC a))))
      <= Max 1 (Max (AlignmentC a) (Max 1 (AlignmentC a)))
  ) =>
  BitPackC (SomeGenericType a)

instance (ToFieldType a) => ToFieldType (SomeGenericType a) where
  type Generics (SomeGenericType a) = 1
  type WithVars (SomeGenericType a) = SomeGenericType (Var 0)
  args = toFieldType @a :> Nil

uart ::
  (HasCallStack, HiddenClockResetEnable dom, KnownNat addrWidth) =>
  String ->
  AbsoluteAddress ->
  Circuit (MemoryMapped (Wishbone dom 'Standard addrWidth (BitVector 32))) ()
uart instanceName absAddr = circuit $ \master -> do
  let name' = Name{name = "UART", description = "Universal Asynchronous Receiver Transmitter"}
  [dataReg, status, flag] <- deviceDefinition instanceName absAddr name' -< master
  deviceReg' "data" ReadWrite 0x0 (0 :: BitVector 32) -< dataReg
  deviceReg' "status" ReadOnly 0x1 Ready -< status
  deviceReg' "some_flag" ReadOnly 0x4 (Nothing :: Maybe (BitVector 8, BitVector 8)) -< flag

timer ::
  (HasCallStack, HiddenClockResetEnable dom, KnownNat addrWidth) =>
  String ->
  AbsoluteAddress ->
  Circuit (MemoryMapped (Wishbone dom 'Standard addrWidth (BitVector 32))) ()
timer instanceName absAddr = circuit $ \master -> do
  let name' = Name{name = "Timer", description = "Component that keeps a timing counter"}
  [timeReg, flagReg, tupleReg, progressReg] <-
    deviceDefinition instanceName absAddr name' -< master
  deviceReg' "time" ReadOnly 0x0 (0 :: BitVector 32) -< timeReg
  deviceReg' "some_flag" ReadOnly 0x4 (Nothing :: Maybe (BitVector 16)) -< flagReg
  deviceReg' "some_tuple" ReadWrite 0x8 (0 :: BitVector 16, 14 :: Signed 16) -< tupleReg
  deviceReg' "some_progress" ReadWrite 0xA (Pending (0 :: BitVector 8)) -< progressReg

memory ::
  forall dom addrWidth size.
  (HasCallStack, HiddenClockResetEnable dom, KnownNat addrWidth, KnownNat size) =>
  String ->
  AbsoluteAddress ->
  Access ->
  SNat size ->
  Circuit (MemoryMapped (Wishbone dom 'Standard addrWidth (BitVector 32))) ()
memory instanceName absAddr access' size' = circuit $ \master -> do
  let name' = Name{name = "Memory_" <> instanceName, description = ""}
  [reg] <- deviceDefinition instanceName absAddr name' -< master
  deviceVecField' "memory" access' 0x0 size' (0 :: BitVector 8) -< reg

--
-- example components
--

deviceDef' ::
  forall dom a n addrWidth.
  Circuit
    (MemoryMapped (Wishbone dom 'Standard addrWidth a))
    (Vec n (Wishbone dom 'Standard addrWidth ()))
deviceDef' = undefined

deviceDefinition ::
  forall dom a n addrWidth.
  ( HasCallStack
  , ToFieldType a
  , NFDataX a
  , KnownNat (BitSize a)
  , KnownNat n
  , KnownNat addrWidth
  , HiddenClockResetEnable dom
  , 1 <= n
  ) =>
  String ->
  AbsoluteAddress ->
  Name ->
  Circuit
    (MemoryMapped (Wishbone dom 'Standard addrWidth a))
    ( Vec
        n
        ( BackwardAnnotated
            (BitVector addrWidth, SimOnly (NamedLoc Register))
            (Wishbone dom 'Standard addrWidth a)
        )
    )
deviceDefinition instanceName absAddr' deviceDesc = Circuit $ \(m2s, bwds) ->
  let
    annotations = fst <$> bwds
    s2ms = bundle $ snd <$> bwds
    addrs = fst <$> annotations
    regs = (\(_, SimOnly x) -> x) <$> annotations
    memMap =
      MemoryMap
        { deviceDefs = Map.singleton (name deviceDesc) definition
        , tree = DeviceInstance instanceLoc absAddr' instanceName (name deviceDesc)
        }
    definition =
      DeviceDefinition
        { deviceName = deviceDesc
        , registers = toList regs
        , defLocation = defLoc
        }

    (s2m, m2ss) = go addrs m2s s2ms
   in
    ((SimOnly memMap, s2m), m2ss)
 where
  ((_, defLoc) : (_, instanceLoc) : _) = getCallStack callStack

  go addrs m2s s2ms = (s2m, m2ss)
   where
    active :: Signal dom (Maybe (Index n))
    active = register Nothing newActive
    (unbundle -> (s2m, unbundle -> m2ss, newActive)) = inner (addrRange addrs) <$> m2s <*> s2ms <*> active

  inner ranges m2s@WishboneM2S{..} s2ms active
    | not (busCycle && strobe) = (emptyWishboneS2M, m2ss, Nothing)
    -- no existing transaction, no new transaction
    -- => ERROR, we're in a cycle but couldn't find a subordinate that matches the address
    -- which shouldn't ever happen, but if it does it should be an error!
    | Nothing <- active
    , Nothing <- newActive =
        (emptyWishboneS2M{err = True}, m2ss, Nothing)
    -- no previous transaction, but a new one!
    | Nothing <- active
    , Just idx <- newActive =
        (s2ms !! idx, replace idx m2s m2ss, Just idx)
    -- have an existing transaction, use that one
    | Just idx <- active = (s2ms !! idx, replace idx m2s m2ss, active)
   where
    m2ss = repeat emptyWishboneM2S
    newActive = findIndex (\(start, end) -> addr >= start && addr < end) ranges

deviceReg' ::
  forall dom a addrWidth.
  ( HasCallStack
  , ToFieldType a
  , NFDataX a
  , BitPackC a
  , KnownNat addrWidth
  , HiddenClockResetEnable dom
  ) =>
  String ->
  Access ->
  BitVector addrWidth ->
  a ->
  Circuit
    ( BackwardAnnotated
        (BitVector addrWidth, SimOnly (NamedLoc Register))
        (Wishbone dom 'Standard addrWidth (BitVector 32))
    )
    ()
deviceReg' name' access' addr' def' = Circuit $ \(m2s0, ()) ->
  (((addr', SimOnly reg'), go m2s0), ())
 where
  defBytes :: BitVector 32
  defBytes = resize $ packC def'

  (_, loc) : _ = getCallStack callStack
  reg' =
    ( Name
        { name = name'
        , description = ""
        }
    , loc
    , Register
        { access = access'
        , address = toInteger addr'
        , fieldType = toFieldType @a
        , fieldSize = snatToInteger (SNat @(ByteSizeC a))
        , reset = Nothing
        }
    )
  go m2s = s2m
   where
    registerVal = register defBytes $ fromMaybe <$> registerVal <*> nextVal
    (unbundle -> (s2m, nextVal)) = inner <$> m2s <*> registerVal

  inner WishboneM2S{..} val
    | not (busCycle && strobe) = (emptyWishboneS2M, Nothing)
    | writeEnable = (emptyWishboneS2M{acknowledge = True}, Just writeData)
    | otherwise = ((emptyWishboneS2M @a){acknowledge = True, readData = 0}, Nothing)

deviceReg ::
  forall dom a addrWidth.
  ( HasCallStack
  , ToFieldType a
  , NFDataX a
  , KnownNat addrWidth
  , HiddenClockResetEnable dom
  ) =>
  String ->
  Access ->
  BitVector addrWidth ->
  a ->
  Circuit
    ( BackwardAnnotated
        (BitVector addrWidth, SimOnly (NamedLoc Register))
        (Wishbone dom 'Standard addrWidth a)
    )
    ()
deviceReg name' access' addr' def' = Circuit $ \(m2s0, ()) ->
  (((addr', SimOnly reg'), go m2s0), ())
 where
  (_, loc) : _ = getCallStack callStack
  reg' =
    ( Name
        { name = name'
        , description = ""
        }
    , loc
    , Register
        { access = access'
        , address = toInteger addr'
        , fieldType = toFieldType @a
        , fieldSize = error "use deviceReg' with BitPackC"
        , reset = Nothing
        }
    )
  go m2s = s2m
   where
    registerVal = register def' $ fromMaybe <$> registerVal <*> nextVal
    (unbundle -> (s2m, nextVal)) = inner <$> m2s <*> registerVal

  inner WishboneM2S{..} val
    | not (busCycle && strobe) = (emptyWishboneS2M, Nothing)
    | writeEnable = (emptyWishboneS2M{acknowledge = True}, Just writeData)
    | otherwise = ((emptyWishboneS2M @a){acknowledge = True, readData = val}, Nothing)

deviceVecField' ::
  forall dom n a addrWidth.
  ( HasCallStack
  , KnownNat n
  , ToFieldType a
  , NFDataX a
  , BitPackC a
  , BitPackC (Vec n a)
  , KnownNat addrWidth
  , HiddenClockResetEnable dom
  ) =>
  String ->
  Access ->
  BitVector addrWidth ->
  SNat n ->
  a ->
  Circuit
    ( BackwardAnnotated
        (BitVector addrWidth, SimOnly (NamedLoc Register))
        (Wishbone dom 'Standard addrWidth (BitVector 32))
    )
    ()
deviceVecField' name' access' addr' _size def' = Circuit $ \(m2s0, ()) ->
  (((addr', SimOnly reg'), go m2s0), ())
 where
  (_, loc) : _ = getCallStack callStack

  defBytes :: BitVector 32
  defBytes = resize $ packC def'

  reg' =
    ( Name
        { name = name'
        , description = ""
        }
    , loc
    , Register
        { access = access'
        , address = toInteger addr'
        , fieldType = toFieldType @(Vec n a)
        , fieldSize = snatToInteger (SNat @(ByteSizeC (Vec n a)))
        , reset = Nothing
        }
    )
  go m2s = s2m
   where
    registerVal = register defBytes $ fromMaybe <$> registerVal <*> nextVal
    (unbundle -> (s2m, nextVal)) = inner <$> m2s <*> registerVal

  inner WishboneM2S{..} val
    | not (busCycle && strobe) = (emptyWishboneS2M, Nothing)
    | writeEnable = (emptyWishboneS2M{acknowledge = True}, Just writeData)
    | otherwise = ((emptyWishboneS2M @a){acknowledge = True, readData = val}, Nothing)

deviceVecField ::
  forall dom n a addrWidth.
  ( HasCallStack
  , KnownNat n
  , ToFieldType a
  , NFDataX a
  , KnownNat addrWidth
  , HiddenClockResetEnable dom
  ) =>
  String ->
  Access ->
  BitVector addrWidth ->
  SNat n ->
  a ->
  Circuit
    ( BackwardAnnotated
        (BitVector addrWidth, SimOnly (NamedLoc Register))
        (Wishbone dom 'Standard addrWidth a)
    )
    ()
deviceVecField name' access' addr' _size def' = Circuit $ \(m2s0, ()) ->
  (((addr', SimOnly reg'), go m2s0), ())
 where
  (_, loc) : _ = getCallStack callStack
  reg' =
    ( Name
        { name = name'
        , description = ""
        }
    , loc
    , Register
        { access = access'
        , address = toInteger addr'
        , fieldType = toFieldType @(Vec n a)
        , fieldSize = error "use deviceVecField' with BitPackC"
        , reset = Nothing
        }
    )
  go m2s = s2m
   where
    registerVal = register def' $ fromMaybe <$> registerVal <*> nextVal
    (unbundle -> (s2m, nextVal)) = inner <$> m2s <*> registerVal

  inner WishboneM2S{..} val
    | not (busCycle && strobe) = (emptyWishboneS2M, Nothing)
    | writeEnable = (emptyWishboneS2M{acknowledge = True}, Just writeData)
    | otherwise = ((emptyWishboneS2M @a){acknowledge = True, readData = val}, Nothing)

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
  AbsoluteAddress ->
  Circuit
    (MemoryMapped (Wishbone dom 'Standard addrWidth a))
    ( Vec
        n
        ( BackwardAnnotated
            (BitVector (CLog 2 n), SimOnly MemoryMap)
            (Wishbone dom 'Standard (addrWidth - CLog 2 n) a)
        )
    )
interconnect absAddr' = Circuit go
 where
  (_, loc) : _ = getCallStack callStack

  go ::
    ( Signal dom (WishboneM2S addrWidth (Div (BitSize a + 7) 8) a)
    , Vec n ((BitVector (CLog 2 n), SimOnly MemoryMap), Signal dom (WishboneS2M a))
    ) ->
    ( (SimOnly MemoryMap, Signal dom (WishboneS2M a))
    , Vec n (Signal dom (WishboneM2S (addrWidth - CLog 2 n) (Div (BitSize a + 7) 8) a))
    )
  go (m2s, unzip -> (unzip -> (prefixes, mmaps), s2ms)) = ((SimOnly memoryMap, s2m), unbundle m2ss)
   where
    memoryMap =
      MemoryMap
        { deviceDefs = mergeDeviceDefs (deviceDefs . unSim <$> toList mmaps)
        , tree = Interconnect loc absAddr' (toList descs)
        }

    size = maxBound :: BitVector (addrWidth - CLog 2 n)
    relAddrs = prefixToAddr <$> prefixes
    descs = zip3 relAddrs (repeat (toInteger size)) (tree . unSim <$> mmaps)
    unSim (SimOnly x) = x

    prefixToAddr :: BitVector (CLog 2 n) -> Address
    prefixToAddr prefix = toInteger prefix `shiftL` fromInteger shift'
     where
      shift' = snatToInteger $ SNat @(addrWidth - CLog 2 n)

    (unbundle -> (s2m, m2ss)) = inner prefixes <$> m2s <*> bundle s2ms

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

addrRange :: forall a n. (Bounded a) => Vec n a -> Vec n (a, a)
addrRange startAddrs = zip startAddrs bounds
 where
  bounds = startAddrs <<+ maxBound

topLevel ::
  "CLK" ::: Clock System ->
  "RST" ::: Reset System ->
  "M2S" ::: Signal System (WishboneM2S 32 4 (BitVector 32)) ->
  "S2M" ::: Signal System (WishboneS2M (BitVector 32))
topLevel clk rst m2s =
  withClockResetEnable clk rst enableGen
    $ let f = toSignals someCircuit
          ((_, x), _) = f (m2s, ())
       in x

{-# CLASH_OPAQUE topLevel #-}
makeTopEntity 'topLevel
