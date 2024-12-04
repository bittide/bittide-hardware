-- SPDX-FileCopyrightText: 2022-2023 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Utils.Cpu where

import Clash.Prelude

import Protocols.Wishbone
import VexRiscv
import VexRiscv.JtagTcpBridge as JTag
import VexRiscv.VecToTuple (vecToTuple)

import GHC.Stack (HasCallStack)

import Utils.ProgramLoad (Memory, DMemory)
import Utils.Interconnect (interconnectTwo)
import Clash.Explicit.Prelude (unsafeOrReset)
import Data.Maybe (fromMaybe)

createDomain vXilinxSystem{vName="Basic50", vPeriod= hzToPeriod 50_000_000}

{-
Address space

0b0000 0x0000_0000 Character device / Debug addresses
0b0010 0x2000_0000 instruction memory
0b0100 0x4000_0000 data memory
-}
cpu ::
  ( HasCallStack
  , HiddenClockResetEnable dom
  -- XXX: VexRiscv responds asynchronously to the reset signal. Figure out how
  --      convenient it is to use this within a design with synchronous resets.
  -- , HasAsynchronousReset dom
  ) =>
  DumpVcd ->
  Maybe (Signal dom JtagIn) ->
  DMemory dom ->
  DMemory dom ->
  ( Signal dom CpuOut
  , Signal dom JtagOut
  , -- writes
    Signal dom (Maybe (BitVector 32, BitVector 32))
  , -- iBus responses
    Signal dom (WishboneS2M (BitVector 32))
  , -- dBus responses
    Signal dom (WishboneS2M (BitVector 32))
  )
cpu dumpVcd jtagIn0 bootIMem bootDMem =
  ( cpuOut
  , jtagOut
  , writes
  , iS2M
  , dS2M
  )
  where
    (cpuOut, jtagOut) = vexRiscv dumpVcd hasClock (hasReset `unsafeOrReset` jtagReset) input jtagIn1

    jtagReset =
      unsafeFromActiveHigh $ register False $
        bitToBool . debugReset <$> jtagOut

    jtagIn1 = fromMaybe (pure JTag.defaultIn) jtagIn0

    {-
    00000000 - dummy area
    20000000 - instruction memory
    40000000 - data memory
    -}

    -- The I-bus is only split once, for the D-mem and everything below.
    (iS2M, vecToTuple . unbundle -> (iMemIM2S, dMemIM2S)) = interconnectTwo
      (unBusAddr . iBusWbM2S <$> cpuOut)
      ((0x0000_0000, iMemIS2M) :> (0x4000_0000, dMemIS2M) :> Nil)

    -- Because the dummy region should never be accessed by the instruction bus
    -- it's just "ignored"
    (iMemIS2M, iMemDS2M) = bootIMem (mapAddr (\x -> complement 0x2000_0000 .&. x) <$> iMemIM2S) iMemDM2S

    -- needed for 'writes' below
    dM2S = dBusWbM2S <$> cpuOut

    -- because of the memory map having the dummy at 0x0.., then instructions
    -- and then data memory, the D-bus is split in an "upper" and "lower" region,
    -- where the "upper" region is just the D-mem, the "lower" region gets split
    -- again for the instruction memory and the dummy
    (dS2M, vecToTuple . unbundle -> (dLowerRegionM2S, dUpperRegionM2S)) = interconnectTwo
      (unBusAddr <$> dM2S)
      ((0x0000_0000, dLowerRegionS2M) :> (0x4000_0000, dUpperRegionS2M) :> Nil)

    (dLowerRegionS2M, vecToTuple . unbundle -> (dDummyM2S, iMemDM2S)) = interconnectTwo
      dLowerRegionM2S
      ((0x0000_0000, dDummyS2M) :> (0x2000_0000, iMemDS2M) :> Nil)

    (dUpperRegionS2M, dMemIS2M) = bootDMem dUpperRegionM2S dMemIM2S
    dDummyS2M = dummyWb dDummyM2S

    input =
      ( \iBus dBus ->
          CpuIn
            { timerInterrupt = low,
              externalInterrupt = low,
              softwareInterrupt = low,
              iBusWbS2M = iBus,
              dBusWbS2M = dBus
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
