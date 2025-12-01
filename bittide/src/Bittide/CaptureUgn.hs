-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Bittide.CaptureUgn (captureUgn) where

import Clash.Explicit.Prelude

import Bittide.SharedTypes (Bytes)
import Bittide.Shutter (shutter)
import Data.Maybe (fromJust, fromMaybe, isJust)
import GHC.Stack (HasCallStack)
import Protocols
import Protocols.MemoryMap (Access (ReadOnly), Mm)
import Protocols.MemoryMap.Registers.WishboneStandard (
  RegisterConfig (access),
  deviceWb,
  registerConfig,
  registerWbI_,
 )
import Protocols.Wishbone (Wishbone, WishboneMode (Standard))

import Clash.Class.BitPackC (ByteOrder)
import qualified Clash.Prelude as C

{- | Captures the remote counter from a bittide link and pairs it with the corresponding
local counter. All frames except for the first frame will be forwarded to the output.

Assumes that:
- When a link is not up, it produces `Nothing` consistently
- When a link comes down, it will at some point switch from only producing `Nothing`,
  to only producing `Just data`
- When a link comes up, the first frame will contain the remote counter value.
- When a link goes down, it will start producing consistently `Nothing`

The register layout is as follows:
- Address 0: lsbs local counter
- Address 1: msbs local counter
- Address 2: lsbs remote counter
- Address 3: msbs remote counter
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
  Signal dom (Maybe (BitVector 64)) ->
  Circuit
    (ToConstBwd Mm, Wishbone dom 'Standard addrW (Bytes 4))
    (CSignal dom (BitVector 64))
captureUgn localCounter linkIn = circuit $ \bus -> do
  [wbLocalCounter, wbRemoteCounter, wbHasCaptured] <- deviceWb "CaptureUgn" -< bus

  let trigger = C.isRising False (isJust <$> linkIn)
  capturedLocalCounter <- shutter trigger -< Fwd localCounter
  capturedRemoteCounter <- shutter trigger -< Fwd (fromMaybe 0 <$> linkIn)
  capturedHasCaptured <- shutter trigger -< Fwd (isJust <$> linkIn)

  registerWbI_ localCounterConfig 0 -< (wbLocalCounter, capturedLocalCounter)
  registerWbI_ remoteCounterConfig 0 -< (wbRemoteCounter, capturedRemoteCounter)
  registerWbI_ hasCapturedConfig False -< (wbHasCaptured, capturedHasCaptured)

  idC -< Fwd (fromJust <$> linkIn)
 where
  localCounterConfig = (registerConfig "local_counter"){access = ReadOnly}
  remoteCounterConfig = (registerConfig "remote_counter"){access = ReadOnly}
  hasCapturedConfig = (registerConfig "has_captured"){access = ReadOnly}
