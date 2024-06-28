-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE CPP #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE UndecidableInstances #-}
-- {-# OPTIONS -fplugin-opt=Protocols.Plugin:debug #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=20 #-}
{-# OPTIONS_GHC -fplugin Protocols.Plugin #-}

module Internal.HdlTest.UartMock where

import Clash.Annotations.TH
import Clash.Prelude
import Protocols (Circuit, toSignals)
import Protocols.Wishbone (Wishbone, WishboneM2S, WishboneMode (Standard), WishboneS2M)

import BitPackC (BitPackC (AlignmentC))
import GHC.Stack (HasCallStack)
import Protocols.MemoryMap
import Protocols.MemoryMap.FieldType hiding (description, name)

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
