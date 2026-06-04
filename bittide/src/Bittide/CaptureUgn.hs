-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Bittide.CaptureUgn (captureUgns, sendUgn) where

import Clash.Explicit.Prelude

import Bittide.Extra.Maybe (toMaybe)
import Bittide.SharedTypes (BitboneMm)
import Clash.Class.BitPackC (ByteOrder)
import Clash.Functor.Extra ((<<$>>), (<<*>>))
import Data.Maybe (fromJust)
import GHC.Stack (HasCallStack)
import Protocols
import Protocols.MemoryMap (Access (ReadOnly, ReadWrite))
import Protocols.MemoryMap.Mask (Mask, fromVec)
import Protocols.MemoryMap.Registers.WishboneStandard (
  RegisterConfig (access),
  deviceConfig,
  deviceWbI,
  registerConfig,
  registerWbI_,
  registerWbVecI_,
 )

import qualified Clash.Prelude as C

data HasCaptured = HasCaptured | HasNotCaptured
  deriving (Eq, Show, Generic, NFDataX)

{- | Captures the remote counter from a number of bittide links and pairs each with
the corresponding local counter. All frames except for the first frame will be
forwarded to the output. If a link goes down, this component should be reset.

Each captured value is exposed in a per-link entry of a vector-valued Wishbone
register, indexed by link number.

Assumes that, for each link:
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
captureUgns ::
  forall n dom addrW.
  ( HasCallStack
  , C.HiddenClockResetEnable dom
  , KnownNat n
  , KnownNat addrW
  , ?byteOrder :: ByteOrder
  ) =>
  -- | Local counter (shared across all links)
  Signal dom (Unsigned 64) ->
  -- | Per-link data from links
  Signal dom (Vec n (Maybe (BitVector 64))) ->
  Circuit
    (BitboneMm dom addrW)
    (CSignal dom (Vec n (BitVector 64)))
captureUgns localCounter (C.dflipflop -> linkIns) = circuit $ \bus -> do
  [wbLocalCounter, wbRemoteCounter, wbEbDelta, wbHasCaptured] <-
    deviceWbI (deviceConfig "CaptureUgns") -< bus

  registerWbVecI_ localCounterConfig (0 :: Unsigned 64) -< (wbLocalCounter, Fwd localCounters)
  registerWbVecI_ remoteCounterConfig (0 :: Unsigned 64) -< (wbRemoteCounter, Fwd remoteCounters)
  registerWbVecI_ elasticBufferDeltaConfig (0 :: Signed 32) -< (wbEbDelta, Fwd ebDeltas)
  registerWbI_ hasCapturedConfig zeroMask -< (wbHasCaptured, Fwd (Just <$> hasCapturedMask))

  idC -< Fwd rawLinkIns
 where
  (triggers, hasCaptureds) =
    unbundle
      $ fmap unzip
      $ bundle
      $ map (C.mealy goCapture HasNotCaptured)
      $ unbundle linkIns

  rawLinkIns = fromJust <<$>> linkIns

  hasCapturedMask :: Signal dom (Mask n)
  hasCapturedMask = fromVec <$> hasCaptureds

  -- For each link, on the trigger cycle write 'Just' the captured value to that
  -- link's slot; otherwise 'Nothing' (no update).
  localCounters :: Signal dom (Vec n (Maybe (Unsigned 64)))
  localCounters = toMaybe <<$>> triggers <<*>> fmap repeat localCounter

  remoteCounters :: Signal dom (Vec n (Maybe (Unsigned 64)))
  remoteCounters = toMaybe <<$>> triggers <<*>> ((fromJustX . maybeUnpack) <<$>> rawLinkIns)

  -- 'elastic_buffer_delta' is firmware-writable only; no hardware updates.
  ebDeltas :: Signal dom (Vec n (Maybe (Signed 32)))
  ebDeltas = pure (repeat Nothing)

  zeroMask :: Mask n
  zeroMask = fromVec (repeat False)

  localCounterConfig = (registerConfig "local_counter"){access = ReadOnly}
  remoteCounterConfig = (registerConfig "remote_counter"){access = ReadOnly}
  elasticBufferDeltaConfig = (registerConfig "elastic_buffer_delta"){access = ReadWrite}
  hasCapturedConfig = (registerConfig "has_captured"){access = ReadOnly}

  -- Output is @(trigger, haveCaptured)@: 'trigger' is a one-cycle pulse on the
  -- first 'Just', 'haveCaptured' is a level high from that cycle onwards.
  goCapture ::
    HasCaptured ->
    Maybe (BitVector 64) ->
    (HasCaptured, (Bool, Bool))
  goCapture HasCaptured _ = (HasCaptured, (False, True))
  goCapture _ Nothing = (HasNotCaptured, (False, False))
  goCapture _ (Just _) = (HasCaptured, (True, False))

{- | Outputs the local counter when the link is *not* sampling and the very first
cycle that it is. Otherwise, outputs the switch data. In effect, this makes sure
that the first frame received by the neighbor contains the local counter. This
can subsequently be captured using 'captureUgns' and used to calculate UGNs/IGNs.
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
