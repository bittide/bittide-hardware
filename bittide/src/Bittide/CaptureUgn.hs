-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Bittide.CaptureUgn (captureUgn, sendUgn, sendUgnC) where

import Clash.Explicit.Prelude

import Bittide.ElasticBuffer (ElasticBufferData (Data), fromData)
import Bittide.SharedTypes (BitboneMm)
import Bittide.Shutter (shutter)
import Data.Maybe (fromJust)
import GHC.Stack (HasCallStack)
import Protocols
import Protocols.MemoryMap (Access (ReadOnly, ReadWrite))
import Protocols.MemoryMap.Registers.WishboneStandard (
  RegisterConfig (access),
  deviceWb,
  registerConfig,
  registerWbI_,
 )

import Clash.Class.BitPackC (ByteOrder)

import qualified Clash.Prelude as C

data HasCaptured = HasCaptured | HasNotCaptured
  deriving (Eq, Show, Generic, NFDataX)

{- | Captures the remote counter from a bittide link and pairs it with the corresponding
local counter. All frames except for the first frame will be forwarded to the output. If
a link goes down, this component should be reset.

Assumes that:
- When a link is not up, it produces `Nothing` consistently
- When a link comes up, it will at some point switch from only producing `Nothing`,
  to only producing `Just data`
- When a link comes up, the first frame will contain the remote counter value.
- When a link goes down, it will start producing consistently `Nothing`

When doing UGN capture while clock control is still running the captured UGN needs to be
compensated for the number of frames added or removed from the elastic buffer between the
time the frame was received and the time clock control is finished. This delta can be
stored in the register 'elastic_buffer_delta' for easy access. This information is not
used on hardware, but since it is needed for UGN calculation it is exposed here.
-}
captureUgn ::
  forall dom addrW.
  ( HasCallStack
  , C.HiddenClockResetEnable dom
  , KnownNat addrW
  , ?busByteOrder :: ByteOrder
  , ?regByteOrder :: ByteOrder
  ) =>
  -- | Local counter
  Signal dom (Unsigned 64) ->
  -- | Data from link
  Signal dom (ElasticBufferData (Maybe (BitVector 64))) ->
  Circuit
    (BitboneMm dom addrW)
    (CSignal dom (BitVector 64))
captureUgn localCounter (C.dflipflop -> linkIn) = circuit $ \bus -> do
  [wbLocalCounter, wbRemoteCounter, wbEbDelta, wbHasCaptured] <-
    deviceWb "CaptureUgn" -< bus
  let
    rawLinkIn = fromJust . fromData <$> linkIn
    trigger = C.mealy goTrigger HasNotCaptured linkIn
  capturedLocalCounter <- shutter trigger -< Fwd localCounter
  capturedRemoteCounter <- shutter trigger -< Fwd rawLinkIn
  capturedHasCaptured <- shutter trigger -< Fwd (pure True)

  registerWbI_ localCounterConfig 0 -< (wbLocalCounter, capturedLocalCounter)
  registerWbI_ remoteCounterConfig 0 -< (wbRemoteCounter, capturedRemoteCounter)
  registerWbI_ elasticBufferDeltaConfig (0 :: Signed 32) -< (wbEbDelta, Fwd noWrite)
  registerWbI_ hasCapturedConfig False -< (wbHasCaptured, capturedHasCaptured)

  idC -< Fwd rawLinkIn
 where
  noWrite = pure Nothing
  localCounterConfig = (registerConfig "local_counter"){access = ReadOnly}
  remoteCounterConfig = (registerConfig "remote_counter"){access = ReadOnly}
  elasticBufferDeltaConfig = (registerConfig "elastic_buffer_delta"){access = ReadWrite}
  hasCapturedConfig = (registerConfig "has_captured"){access = ReadOnly}

  goTrigger ::
    HasCaptured ->
    ElasticBufferData (Maybe (BitVector 64)) ->
    (HasCaptured, Bool)
  goTrigger HasNotCaptured (Data (Just _)) = (HasCaptured, True)
  goTrigger s _ = (s, False)

{- | Outputs the local counter when the link is *not* sampling and the very first
cycle that it is. Otherwise, outputs the switch data. In effect, this makes sure
that the first frame received by the neighbor contains the local counter. This
can subsequently be captured using 'captureUgn' and used to calculate UGNs/IGNs.
-}
sendUgn ::
  forall dom.
  ( HasCallStack
  , C.HiddenClock dom
  ) =>
  -- | Local counter
  Signal dom (Unsigned 64) ->
  -- | Sampling? Typically driven by 'Bittide.Transceiver.Output.txSampling'.
  Signal dom Bool ->
  -- | Switch data
  Signal dom (BitVector 64) ->
  -- | Data to transceiver
  Signal dom (BitVector 64)
sendUgn localCounter sampling switchData =
  mux samplingDelayed switchData (pack <$> localCounter)
 where
  samplingDelayed =
    C.withEnable enableGen
      $ C.delay (errorX "sendUgn: first samplingDelayed") sampling

-- | Like 'sendUgn', but more convenient to use in our current setup.
sendUgnC ::
  forall dom n.
  ( HasCallStack
  , C.HiddenClock dom
  , KnownNat n
  ) =>
  -- | Local counter
  Signal dom (Unsigned 64) ->
  -- | Sampling? Typically driven by 'Bittide.Transceiver.Outputs.txSamplings'.
  Vec n (Signal dom Bool) ->
  Circuit
    (Vec n (CSignal dom (BitVector 64)))
    (Vec n (CSignal dom (BitVector 64)))
sendUgnC localCounter txSamplings = Circuit go
 where
  go ::
    (Vec n (Signal dom (BitVector 64)), Vec n ()) ->
    (Vec n (), Vec n (Signal dom (BitVector 64)))
  go (txDatas, _) = (repeat (), sendUgn localCounter <$> txSamplings <*> txDatas)
