-- SPDX-FileCopyrightText: 2023 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE UndecidableInstances #-}

module Bittide.ClockControl.Registers where

import Clash.Prelude hiding (PeriodToCycles)

import Protocols
import Protocols.Wishbone

import Bittide.ClockControl
import Bittide.ClockControl.StabilityChecker
import Clash.Class.BitPackC (ByteOrder)
import Clash.Signal.TH.Extra (deriveSignalHasFields)
import GHC.Stack (HasCallStack)
import Protocols.MemoryMap (Access (ReadOnly, WriteOnly), ConstBwd, MM)
import Protocols.MemoryMap.Registers.WishboneStandard (
  BusActivity (BusWrite),
  RegisterConfig (access),
  deviceWbC,
  registerConfig,
  registerWbCI,
  registerWbCI_,
 )

data ClockControlData (nLinks :: Nat) = ClockControlData
  { clockMod :: Maybe SpeedChange
  , stabilityIndications :: Vec nLinks StabilityIndication
  , allStable :: Bool
  , allSettled :: Bool
  }
  deriving (Generic, NFDataX, ShowX, Show)

deriveSignalHasFields ''ClockControlData

-- | Replace all elements in a vector with a default value where the mask is unset.
applyMask ::
  (KnownNat n) =>
  a ->
  Signal dom (BitVector n) ->
  Signal dom (Vec n a) ->
  Signal dom (Vec n a)
applyMask dflt mask xs = mux <$> fmap unpack mask <*> xs <*> pure (repeat dflt)

{- | A wishbone accessible clock control interface.
This interface receives the link mask and 'RelDataCount's from all links.
Furthermore it produces FINC/FDEC pulses for the clock control boards.

__NB__: the `Maybe SpeedChange` part of the output is only asserted for a single cycle.
This must be stickied or otherwise held for the minimum pulse width specified by the
clock board this register is controlling.
-}
clockControlWb ::
  forall dom addrW nLinks m margin framesize.
  ( HiddenClockResetEnable dom
  , HasCallStack
  , KnownNat addrW
  , 1 <= framesize
  , 1 <= nLinks
  , KnownNat nLinks
  , KnownNat m
  , m <= 32
  , nLinks <= 32
  , ?busByteOrder :: ByteOrder
  , ?regByteOrder :: ByteOrder
  ) =>
  -- | Maximum number of elements the incoming buffer occupancy is
  -- allowed to deviate from the current @target@ for it to be
  -- considered "stable".
  SNat margin ->
  -- | Minimum number of clock cycles the incoming buffer occupancy
  -- must remain within the @margin@ for it to be considered "stable".
  SNat framesize ->
  -- | Link mask
  Signal dom (BitVector nLinks) ->
  -- | Counters
  Vec nLinks (Signal dom (RelDataCount m)) ->
  -- | Wishbone accessible clock control circuitry
  Circuit
    (ConstBwd MM, Wishbone dom 'Standard addrW (BitVector 32))
    (CSignal dom (ClockControlData nLinks))
clockControlWb margin frameSize linkMask (bundle -> counters) = circuit $ \(mm, wb) -> do
  [ wbNumLinks
    , wbLinkMask
    , wbUpLinks
    , wbChangeSpeed
    , wbLinksStable
    , wbLinksSettled
    , wbDataCounts
    ] <-
    deviceWbC "ClockControl" -< (mm, wb)

  (_cs, Fwd changeSpeed) <-
    registerWbCI wbChangeSpeedConfig NoChange -< (wbChangeSpeed, Fwd noWrite)

  registerWbCI_ wbNumLinksConfig numberOfLinks -< (wbNumLinks, Fwd noWrite)
  registerWbCI_ wbLinkMaskConfig 0 -< (wbLinkMask, Fwd (Just <$> linkMask))
  registerWbCI_ wbUpLinksConfig 0 -< (wbUpLinks, Fwd (Just <$> linkMaskPopCount))
  registerWbCI_ wbLinksStableConfig 0 -< (wbLinksStable, Fwd linksStableWrite)
  registerWbCI_ wbLinksSettledConfig 0 -< (wbLinksSettled, Fwd linksSettledWrite)
  registerWbCI_ wbDataCountsConfig (repeat 0)
    -< (wbDataCounts, Fwd (Just <$> maskedCounters))

  let
    clockControlData :: Signal dom (ClockControlData nLinks)
    clockControlData =
      ClockControlData
        <$> fmap busActivityToMaybeSpeedChange changeSpeed
        <*> stabilityIndications
        <*> allLinksStable
        <*> allLinksSettled

  idC -< Fwd clockControlData
 where
  noWrite = pure Nothing

  wbNumLinksConfig = (registerConfig "n_links"){access = ReadOnly}
  wbLinkMaskConfig = (registerConfig "link_mask"){access = ReadOnly}
  wbUpLinksConfig = (registerConfig "n_up_links"){access = ReadOnly}
  wbChangeSpeedConfig = (registerConfig "change_speed"){access = WriteOnly}
  wbLinksStableConfig = (registerConfig "links_stable"){access = ReadOnly}
  wbLinksSettledConfig = (registerConfig "links_settled"){access = ReadOnly}
  wbDataCountsConfig = (registerConfig "data_counts"){access = ReadOnly}

  numberOfLinks :: Unsigned 8
  numberOfLinks = natToNum @nLinks

  linkMaskPopCount :: Signal dom (Unsigned 8)
  linkMaskPopCount = fromIntegral . popCount <$> linkMask

  maskedCounters :: Signal dom (Vec nLinks (RelDataCount m))
  maskedCounters = applyMask 0 linkMask counters

  stabilityIndications :: Signal dom (Vec nLinks StabilityIndication)
  stabilityIndications = bundle $ stabilityChecker margin frameSize <$> unbundle maskedCounters

  linksStable = map (.stable) <$> stabilityIndications
  linksStableWrite = Just . pack <$> linksStable
  maskedLinksStable = applyMask True linkMask linksStable
  allLinksStable = and <$> maskedLinksStable

  linksSettled = map (.settled) <$> stabilityIndications
  linksSettledWrite = Just . pack <$> linksSettled
  maskedLinksSettled = applyMask True linkMask linksSettled
  allLinksSettled = and <$> maskedLinksSettled

  busActivityToMaybeSpeedChange :: BusActivity SpeedChange -> Maybe SpeedChange
  busActivityToMaybeSpeedChange (BusWrite change) = Just change
  busActivityToMaybeSpeedChange _ = Nothing
