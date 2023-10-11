-- SPDX-FileCopyrightText: 2022-2023 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}
module Utils.Cpu where

import Clash.Prelude

import Protocols.Wishbone
import VexRiscv
import VexRiscv.JtagTcpBridge as JTag

import GHC.Stack (HasCallStack)

import Utils.ProgramLoad (Memory)
import Utils.Interconnect (interconnectTwo)
import System.IO.Unsafe (unsafePerformIO)

emptyInput :: Input
emptyInput =
  Input
    { timerInterrupt = low,
      externalInterrupt = low,
      softwareInterrupt = low,
      iBusWbS2M = (emptyWishboneS2M @(BitVector 32)) {readData = 0},
      dBusWbS2M = (emptyWishboneS2M @(BitVector 32)) {readData = 0}
    }


{-
Address space

0b0000 0x0000_0000 Character device / Debug addresses
0b0010 0x2000_0000 instruction memory
0b0100 0x4000_0000 data memory
-}
cpu ::
  (HasCallStack, HiddenClockResetEnable dom) =>
  Maybe Integer ->
  Memory dom ->
  Memory dom ->
  ( Signal dom Output,
    -- writes
    Signal dom (Maybe (BitVector 32, BitVector 32)),
    -- iBus responses
    Signal dom (WishboneS2M (BitVector 32)),
    -- dBus responses
    Signal dom (WishboneS2M (BitVector 32))
  )
cpu jtagPort bootIMem bootDMem = (output, writes, iS2M, dS2M)
  where
    (output, jtagOut) = vexRiscv hasClock hasReset hasClock jtagEnable input jtagIn

    (jtagEnable, jtagIn) = case jtagPort of
      Just port -> unsafePerformIO $ jtagTcpBridge' (fromInteger port) jtagOut
      Nothing -> (toEnable (pure False), pure JTag.defaultIn)
    -- (unbundle -> (jtagIn', _debugReset)) = unsafePerformIO $ jtagTcpBridge' 7894 hasReset (jtagOut <$> output) 

    dM2S = dBusWbM2S <$> output

    iM2S = unBusAddr . iBusWbM2S <$> output

    iS2M = bootIMem (mapAddr (\x -> x - 0x2000_0000) <$> iM2S)

    dummy = dummyWb

    dummyS2M = dummy dummyM2S
    bootDS2M = bootDMem bootDM2S

    (dS2M, unbundle -> (dummyM2S :> bootDM2S :> Nil)) = interconnectTwo
      ((\x ->
          -- trace (printf "DBUS %08X" (toInteger (addr x)))
          x) <$> (unBusAddr <$> dM2S))
      ((0x0000_0000, dummyS2M) :> (0x4000_0000, bootDS2M) :> Nil)

    input =
      ( \iBus dBus ->
          Input
            { timerInterrupt = low,
              externalInterrupt = low,
              softwareInterrupt = low,
              iBusWbS2M = makeDefined iBus,
              dBusWbS2M = makeDefined dBus
            }
      )
        <$> iS2M
        <*> dS2M

    unBusAddr = mapAddr ((`shiftL` 2) . extend @_ @_ @2)

    writes =
      mux
        ( (busCycle <$> dM2S)
            .&&. (strobe <$> dM2S)
            .&&. (writeEnable <$> dM2S)
            .&&. (acknowledge <$> dS2M)
        )
        ( do
            dM2S' <- dM2S
            pure $ Just (extend $ addr dM2S' `shiftL` 2, writeData dM2S')
        )
        (pure Nothing)

mapAddr :: (BitVector aw1 -> BitVector aw2) -> WishboneM2S aw1 selWidth a -> WishboneM2S aw2 selWidth a
mapAddr f wb = wb {addr = f (addr wb)}


-- | Wishbone circuit that always acknowledges every request
--
-- Used for the character device. The character device address gets mapped to this
-- component because if it were to be routed to the data memory (where this address is
-- not in the valid space) it would return ERR and would halt execution.
dummyWb :: (HiddenClockResetEnable dom) => Memory dom
dummyWb m2s' = delayControls m2s' (reply <$> m2s')
  where
    reply WishboneM2S {..} =
      (emptyWishboneS2M @(BitVector 32)) {acknowledge = acknowledge, readData = 0}
      where
        acknowledge = busCycle && strobe

    -- \| Delays the output controls to align them with the actual read / write timing.
    delayControls ::
      (HiddenClockResetEnable dom, NFDataX a) =>
      Signal dom (WishboneM2S addressWidth selWidth a) -> -- current M2S signal
      Signal dom (WishboneS2M a) ->
      Signal dom (WishboneS2M a)
    delayControls m2s s2m0 = mux inCycle s2m1 (pure emptyWishboneS2M)
      where
        inCycle = (busCycle <$> m2s) .&&. (strobe <$> m2s)

        -- It takes a single cycle to lookup elements in a block ram. We can therefore
        -- only process a request every other clock cycle.
        ack = (acknowledge <$> s2m0) .&&. (not <$> delayedAck) .&&. inCycle
        err1 = (err <$> s2m0) .&&. inCycle
        delayedAck = register False ack
        delayedErr1 = register False err1
        s2m1 =
          (\wb newAck newErr -> wb {acknowledge = newAck, err = newErr})
            <$> s2m0
            <*> delayedAck
            <*> delayedErr1
