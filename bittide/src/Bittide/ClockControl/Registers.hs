-- SPDX-FileCopyrightText: 2023 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE UndecidableInstances #-}

module Bittide.ClockControl.Registers where

import Clash.Prelude hiding (PeriodToCycles)

import Protocols
import Protocols.Wishbone

import Bittide.ClockControl (RelDataCount, SpeedChange (NoChange))
import Bittide.ClockControl.Callisto.Types (Stability (..))
import Bittide.ClockControl.Config (CcConf (CcConf, callisto, topology), defCcConf)
import Clash.Class.BitPackC (ByteOrder)
import Clash.Explicit.Reset (unsafeOrReset)
import Clash.Functor.Extra ((<<$>>), (<<*>>))
import GHC.Stack (HasCallStack)
import Protocols.MemoryMap (Access (ReadOnly, WriteOnly), ConstBwd, MM)
import Protocols.MemoryMap.Registers.WishboneStandard (
  BusActivity (BusWrite),
  RegisterConfig (access, description),
  deviceWbC,
  registerConfig,
  registerWbC,
  registerWbCI,
  registerWbCI_,
 )

data ClockControlData (nLinks :: Nat) = ClockControlData
  { clockMod :: Maybe SpeedChange
  , stabilities :: Vec nLinks Stability
  , allStable :: Bool
  , allSettled :: Bool
  }
  deriving (Generic, NFDataX, ShowX, Show)

-- | Replace all elements in a vector with a default value where the mask is unset.
applyMask ::
  (KnownNat n) =>
  a ->
  Signal dom (BitVector n) ->
  Signal dom (Vec n a) ->
  Signal dom (Vec n a)
applyMask dflt mask xs = mux <$> fmap unpack mask <*> xs <*> pure (repeat dflt)

defCcConfLinks :: forall nLinks. (KnownNat nLinks) => CcConf (BitVector nLinks)
defCcConfLinks =
  case defCcConf (natToNum @nLinks) of
    CcConf{callisto} ->
      CcConf{callisto, topology = maxBound}

{- | A wishbone accessible clock control interface.
This interface receives the link mask and 'RelDataCount's from all links.
Furthermore it produces FINC/FDEC pulses for the clock control boards.

__NB__: the `Maybe SpeedChange` part of the output is only asserted for a single cycle.
This must be stickied or otherwise held for the minimum pulse width specified by the
clock board this register is controlling.
-}
clockControlWb ::
  forall dom addrW nLinks m.
  ( HiddenClockResetEnable dom
  , HasCallStack
  , KnownNat addrW
  , KnownNat nLinks
  , KnownNat m
  , ?busByteOrder :: ByteOrder
  , ?regByteOrder :: ByteOrder
  ) =>
  -- | Link mask
  Signal dom (BitVector nLinks) ->
  -- | Links suitable for clock control (i.e., recovered clocks won't go down)
  Signal dom (BitVector nLinks) ->
  -- | Counters
  Vec nLinks (Signal dom (RelDataCount m)) ->
  -- | Wishbone accessible clock control circuitry
  Circuit
    (ConstBwd MM, Wishbone dom 'Standard addrW (BitVector 32))
    (CSignal dom (ClockControlData nLinks))
clockControlWb linkMask linksOk (bundle -> counters) = circuit $ \(mm, wb) -> do
  [ wbNumLinks
    , wbLinkMask
    , wbLinkMaskPopCount
    , wbLinkMaskRev
    , wbLinksOk
    , wbChangeSpeed
    , wbLinksStable
    , wbLinksSettled
    , wbMinDataCountSeen
    , wbMaxDataCountSeen
    , wbClearDataCountsSeen
    , wbDataCounts
    , wbConfig
    ] <-
    deviceWbC "ClockControl" -< (mm, wb)

  (_cs, Fwd changeSpeed) <-
    registerWbCI changeSpeedConfig NoChange -< (wbChangeSpeed, Fwd noWrite)

  -- Link configuration
  registerWbCI_ numLinksConfig numberOfLinks -< (wbNumLinks, Fwd noWrite)
  registerWbCI_ linkMaskConfig 0 -< (wbLinkMask, Fwd (Just <$> linkMask))
  registerWbCI_ linkMaskPopCountConfig 0
    -< (wbLinkMaskPopCount, Fwd (Just <$> linkMaskPopCount))
  registerWbCI_ linkMaskRevConfig 0 -< (wbLinkMaskRev, Fwd (Just <$> linkMaskRev))
  registerWbCI_ linksOkConfig 0 -< (wbLinksOk, Fwd (Just <$> linksOk))
  (Fwd linksStable, _l0) <-
    registerWbCI linksStableConfig (0 :: BitVector nLinks) -< (wbLinksStable, Fwd noWrite)
  (Fwd linksSettled, _l1) <-
    registerWbCI linksSettledConfig (0 :: BitVector nLinks) -< (wbLinksSettled, Fwd noWrite)

  -- Data count tracking
  registerWbCI_ dataCountsConfig (repeat 0)
    -< (wbDataCounts, Fwd (Just <$> maskedCounters))
  (Fwd minDataCountsSeen0, _i0) <-
    registerWbC hasClock dataCountsSeenReset minDataCountsSeenConfig (repeat maxBound)
      -< (wbMinDataCountSeen, Fwd (Just <$> minDataCountsSeen1))
  (Fwd maxDataCountsSeen0, _i1) <-
    registerWbC hasClock dataCountsSeenReset maxDataCountsSeenConfig (repeat minBound)
      -< (wbMaxDataCountSeen, Fwd (Just <$> maxDataCountsSeen1))
  (_cd, Fwd clearDataCountsSeen) <-
    registerWbCI clearDataCountsSeenConfig False -< (wbClearDataCountsSeen, Fwd noWrite)

  -- Clock control configuration
  registerWbCI_ configConfig (defCcConfLinks @nLinks) -< (wbConfig, Fwd noWrite)

  let
    maskedLinksStable = applyMask True linkMask (unpack <$> linksStable)
    allLinksStable = and <$> maskedLinksStable

    maskedLinksSettled = applyMask True linkMask (unpack <$> linksSettled)
    allLinksSettled = and <$> maskedLinksSettled

    dataCountsSeenReset :: Reset dom
    dataCountsSeenReset =
      unsafeOrReset
        hasReset
        (unsafeFromActiveHigh (clearDataCountsSeen .== BusWrite True))

    minDataCountsSeen1 :: Signal dom (Vec nLinks (RelDataCount m))
    minDataCountsSeen1 = zipWith min <$> minDataCountsSeen0 <*> maskedCounters

    maxDataCountsSeen1 :: Signal dom (Vec nLinks (RelDataCount m))
    maxDataCountsSeen1 = zipWith max <$> maxDataCountsSeen0 <*> maskedCounters

    clockControlData :: Signal dom (ClockControlData nLinks)
    clockControlData =
      ClockControlData
        <$> fmap busActivityToMaybeSpeedChange changeSpeed
        <*> (Stability <<$>> maskedLinksStable <<*>> maskedLinksSettled)
        <*> allLinksStable
        <*> allLinksSettled

  idC -< Fwd clockControlData
 where
  noWrite = pure Nothing

  numLinksConfig = (registerConfig "n_links"){access = ReadOnly}
  linkMaskConfig = (registerConfig "link_mask"){access = ReadOnly}
  linkMaskPopCountConfig = (registerConfig "link_mask_pop_count"){access = ReadOnly}
  linkMaskRevConfig = (registerConfig "link_mask_rev"){access = ReadOnly}
  linksOkConfig = (registerConfig "links_ok"){access = ReadOnly}
  changeSpeedConfig = (registerConfig "change_speed"){access = WriteOnly}
  linksStableConfig = registerConfig "links_stable"
  linksSettledConfig = registerConfig "links_settled"
  minDataCountsSeenConfig = (registerConfig "min_data_counts_seen"){access = ReadOnly}
  maxDataCountsSeenConfig = (registerConfig "max_data_counts_seen"){access = ReadOnly}
  clearDataCountsSeenConfig = (registerConfig "clear_data_counts_seen"){access = WriteOnly}
  dataCountsConfig = (registerConfig "data_counts"){access = ReadOnly}
  configConfig =
    (registerConfig "config")
      { description =
          "Clock control configuration. By default, this just runs clock control on all available links with, hopefully, sensible control settings."
      }

  linkMaskRev :: Signal dom (BitVector nLinks)
  linkMaskRev = pack . reverse . unpack @(Vec nLinks Bit) <$> linkMask

  numberOfLinks :: Unsigned 8
  numberOfLinks = natToNum @nLinks

  linkMaskPopCount :: Signal dom (Unsigned 8)
  linkMaskPopCount = fromIntegral . popCount <$> linkMask

  maskedCounters :: Signal dom (Vec nLinks (RelDataCount m))
  maskedCounters = applyMask 0 linkMask counters

  busActivityToMaybeSpeedChange :: BusActivity SpeedChange -> Maybe SpeedChange
  busActivityToMaybeSpeedChange (BusWrite change) = Just change
  busActivityToMaybeSpeedChange _ = Nothing
