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
import Clash.Prelude (withClock, withClockResetEnable)
import Protocols

import Bittide.Instances.Hitl.GenericDemo.BringUp (NmuRemBusWidth, UserCoreCircuit)
import Bittide.Instances.Hitl.Setup (LinkCount)
import Bittide.ProgrammableMux (programmableMux)
import Bittide.WireDemoProcessingElement (wireDemoPe, wireDemoPeConfig)

type UserCoreBusses = 2

type RingBufferDepth = 200

ringBufferDepth :: SNat RingBufferDepth
ringBufferDepth = SNat

mkUserCore :: UserCoreCircuit UserCoreBusses (NmuRemBusWidth UserCoreBusses)
mkUserCore bitClk bitRst bitEna localCounter maybeDna =
  circuit $ \(userCoreBusses, Fwd rxs2Raw, handshakeOut) -> do
    [muProgrammableMuxBus, peConfigBus] <- idC -< userCoreBusses

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
    (Fwd businessLogicReset, txsOut) <-
      withClockResetEnable bitClk bitRst bitEna
        $ programmableMux localCounter
        -< (muProgrammableMuxBus, handshakeOut, Fwd (bundle txsBl))
    -- Stop programmable mux

    idC -< txsOut
