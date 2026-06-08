-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{- | User core for the wire demo: a wire-demo processing element that taps
the raw elastic-buffer output, and a programmable mux that sits in series
on the TX wire. By default the mux forwards the handshake's TX output;
once business logic is armed it switches to the PE's output.
-}
module Bittide.Instances.Hitl.WireDemo.UserCore (
  UserCoreBusses,
  RingBufferDepth,
  ringBufferDepth,
  mkUserCore,
) where

import Clash.Explicit.Prelude
import Clash.Prelude (HiddenClockResetEnable, withClock, withClockResetEnable)
import Protocols

import Bittide.Instances.Hitl.GenericDemo.BringUp (NmuRemBusWidth, UserCoreCircuit)
import Bittide.Instances.Hitl.Setup (LinkCount)
import Bittide.ProgrammableMux (programmableMux)
import Bittide.SharedTypes (BitboneMm)
import Bittide.WireDemoProcessingElement (wireDemoPe, wireDemoPeConfig)

import Clash.Class.BitPackC (ByteOrder)
import GHC.Stack (HasCallStack)
import Protocols.MemoryMap (Access (ReadOnly))
import Protocols.MemoryMap.Registers.WishboneStandard (
  RegisterConfig (access),
  deviceConfig,
  deviceWbI,
  registerConfig,
  registerWbI_,
 )

type UserCoreBusses = 3

type RingBufferDepth = 200

ringBufferDepth :: SNat RingBufferDepth
ringBufferDepth = SNat

{- | A read-only scratch peripheral exposing the application counter so the host can
verify, after the schedule has run, that the timed-reset relabel landed: at any moment
@localCounter - appCounter == tReset@ for that node, so the per-node release timing (the
UGN-grooming relabel @q@) can be checked directly over GDB.
-}
appCounterReadbackWb ::
  forall dom addrW.
  ( HasCallStack
  , HiddenClockResetEnable dom
  , KnownNat addrW
  , ?byteOrder :: ByteOrder
  ) =>
  Signal dom (Unsigned 64) ->
  Circuit (BitboneMm dom addrW) ()
appCounterReadbackWb appCounter = circuit $ \bus -> do
  [wbAppCounter] <- deviceWbI (deviceConfig "AppCounterReadback") -< bus
  registerWbI_ (registerConfig "app_counter"){access = ReadOnly} (0 :: Unsigned 64)
    -< (wbAppCounter, Fwd (Just <$> appCounter))

mkUserCore :: UserCoreCircuit UserCoreBusses (NmuRemBusWidth UserCoreBusses)
mkUserCore bitClk bitRst bitEna localCounter maybeDna appReset =
  circuit $ \(userCoreBusses, Fwd rxs2Raw, handshakeOut) -> do
    [muProgrammableMuxBus, peConfigBus, appCounterBus] <- idC -< userCoreBusses

    -- The application counter is the relabeled time base: it starts counting when
    -- the management unit releases 'appReset' (see 'timedResetWb' in the MU), so the
    -- per-node release timing realizes the UGN-grooming relabel. The reset itself
    -- lives outside the user core (in the layer with the management unit).
    let appCounter = register bitClk appReset bitEna 0 (appCounter + 1)

    -- Expose the application counter read-only so the host can confirm the relabel
    -- actually landed on hardware (localCounter - appCounter == tReset per node).
    withClockResetEnable bitClk bitRst bitEna (appCounterReadbackWb appCounter)
      -< appCounterBus

    -- Start business logic
    (readLinkI, writeLinkI) <-
      withClockResetEnable bitClk bitRst bitEna (wireDemoPeConfig @_ @_ @_ @LinkCount)
        -< (peConfigBus, peWrittenData)
    (Fwd txsBl, peWrittenData) <-
      withClock bitClk
        $ wireDemoPe businessLogicReset maybeDna localCounter
        -< (Fwd (unbundle rxs2Raw), readLinkI, writeLinkI)
    -- Stop business logic

    -- Start programmable mux
    -- The programmable mux is driven by the relabeled application counter (started
    -- by the timed reset), so a static schedule (fixed `first_b_cycle`) fires at the
    -- correct physical moment once the node has been relabeled to the reference.
    (Fwd businessLogicReset, txsOut) <-
      withClockResetEnable bitClk bitRst bitEna
        $ programmableMux appCounter
        -< (muProgrammableMuxBus, handshakeOut, Fwd (bundle txsBl))
    -- Stop programmable mux

    idC -< txsOut
