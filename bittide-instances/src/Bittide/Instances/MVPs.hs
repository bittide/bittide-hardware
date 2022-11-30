-- SPDX-FileCopyrightText: 2022-2023 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
module Bittide.Instances.MVPs where

import Clash.Prelude

import Bittide.Instances.Domains
import Bittide.ElasticBuffer
import Bittide.ClockControl.Callisto
import Bittide.ClockControl
import Clash.Annotations.TH (makeTopEntity)
import Bittide.ClockControl.StabilityChecker (stabilityChecker)

type FINC = Bool
type FDEC = Bool

speedChangeToPins :: SpeedChange -> (FINC, FDEC)
speedChangeToPins = \case
 SpeedUp -> (True, False)
 SlowDown -> (False, True)
 NoChange -> (False, False)

clockControlDemo2 ::
  "clkA" ::: Clock Basic200A ->
  "clkB" ::: Clock Basic200B ->
  "clkC" ::: Clock Basic200C ->
  "rstA" ::: Reset Basic200A ->
  "rstB" ::: Reset Basic200B ->
  "rstC" ::: Reset Basic200C ->
  "drainA_B" ::: Reset Basic200A ->
  "drainA_C" ::: Reset Basic200A ->
  "drainB_A" ::: Reset Basic200B ->
  "drainB_C" ::: Reset Basic200B ->
  "drainC_A" ::: Reset Basic200C ->
  "drainC_B" ::: Reset Basic200C ->
  ( "domA" ::: Signal Basic200A ("" ::: ("FINC" ::: FINC, "FDEC" ::: FDEC), "StableB" ::: Bool, "StableC" ::: Bool)
  , "domB" ::: Signal Basic200B ("" ::: ("FINC" ::: FINC, "FDEC" ::: FDEC), "StableA" ::: Bool, "StableC" ::: Bool)
  , "domC" ::: Signal Basic200C ("" ::: ("FINC" ::: FINC, "FDEC" ::: FDEC), "StableA" ::: Bool, "StableB" ::: Bool))
clockControlDemo2
  clkA clkB clkC
  rstA rstB rstC
  drainA_B drainA_C
  drainB_A drainB_C
  drainC_A drainC_B= (domA, domB, domC)
 where
  domA = genericClockControlDemo2 clockConfigA clkA clkB clkC rstA drainA_B drainA_C
  domB = genericClockControlDemo2 clockConfigB clkB clkA clkC rstB drainB_A drainB_C
  domC = genericClockControlDemo2 clockConfigC clkC clkA clkB rstC drainC_A drainC_B

  clockConfigA :: ClockControlConfig Basic200A 12
  clockConfigA = $(lift (defClockConfig @Basic200A))

  clockConfigB :: ClockControlConfig Basic200B 12
  clockConfigB = $(lift (defClockConfig @Basic200B))

  clockConfigC :: ClockControlConfig Basic200C 12
  clockConfigC = $(lift (defClockConfig @Basic200B))


genericClockControlDemo2 ::
  forall local externalA externalB dataCountBits .
  ( KnownDomain local
  , KnownDomain externalA
  , KnownDomain externalB
  , KnownNat dataCountBits
  , 4 <= dataCountBits
  , dataCountBits <= 17) =>
  ClockControlConfig local dataCountBits ->
  Clock local ->
  Clock externalA ->
  Clock externalB ->
  Reset local ->
  Reset local ->
  Reset local ->
  Signal local ((FINC, FDEC), Bool, Bool)
genericClockControlDemo2 config clkInt clkExtA clkExtB rst drainFifoA drainFifoB
  = bundle (withClockResetEnable clkInt rst enableGen stickyBits d15 $ speedChangeToPins <$> speedChange, isStableA, isStableB)
 where

  speedChange = callistoClockControl @2 clkInt rst enableGen
    config (bufferOccupancyA1 :> bufferOccupancyB1 :> Nil)

  bufferOccupancyA1 = mux ((==Pass) <$> ebModeA) bufferOccupancyA0 (pure targetDataCount)
  bufferOccupancyB1 = mux ((==Pass) <$> ebModeB) bufferOccupancyB0 (pure targetDataCount)

  (bufferOccupancyA0, _, _, ebModeA) = withReset rst resettableXilinxElasticBuffer clkInt clkExtA drainFifoA
  (bufferOccupancyB0, _, _, ebModeB) = withReset rst resettableXilinxElasticBuffer clkInt clkExtB drainFifoB

  isStableA = withClockResetEnable clkInt (unsafeFromLowPolarity $ (==Pass) <$> ebModeA) enableGen
    stabilityChecker d3 (SNat @20_000_000) bufferOccupancyA0

  isStableB = withClockResetEnable clkInt (unsafeFromLowPolarity $ (==Pass) <$> ebModeB) enableGen
    stabilityChecker d3 (SNat @20_000_000) bufferOccupancyB0

clockControlDemo1 ::
  "clkA" ::: Clock Basic200A ->
  "clkB" ::: Clock Basic200B ->
  "drainFifoA" ::: Reset Basic200A ->
  "drainFifoB" ::: Reset Basic200B ->
  ( "domA" ::: Signal Basic200A
    ( "" :::
      ( "FINC" ::: FINC
      , "FDEC" ::: FDEC
      )
    , "Underflowed" ::: Bool
    , "Overflowed" ::: Bool
    , "isStable" ::: Bool
    , "EbMode" ::: EbMode
    )
  ,  "domB" ::: Signal Basic200B
    (
      "" :::
      ( "FINC" ::: FINC
      , "FDEC" ::: FDEC
      )
    , "Underflowed" ::: Bool
    , "Overflowed" ::: Bool
    , "isStable" ::: Bool
    , "EbMode" ::: EbMode
    )
  )
clockControlDemo1 clkA clkB drainFifoA drainFifoB = (demoA, demoB)
 where
  demoA =
    genericClockControlDemo0 clockConfigA clkB clkA (unsafeFromHighPolarity $ pure False)
    drainFifoA (unsafeFromHighPolarity $ pure False)
  demoB =
    genericClockControlDemo0 clockConfigB clkA clkB (unsafeFromHighPolarity $ pure False)
    drainFifoB (unsafeFromHighPolarity $ pure False)

  clockConfigA :: ClockControlConfig Basic200A 12
  clockConfigA = $(lift (defClockConfig @Basic200A))

  clockConfigB :: ClockControlConfig Basic200B  12
  clockConfigB = $(lift (defClockConfig @Basic200B))

genericClockControlDemo0 ::
  forall recovered controlled  dataCountBits .
  ( KnownDomain recovered
  , KnownDomain controlled
  , KnownNat dataCountBits
  , 4 <= dataCountBits
  , dataCountBits <= 17) =>
  ClockControlConfig controlled  dataCountBits ->
  Clock recovered ->
  Clock controlled ->
  Reset controlled ->
  Reset controlled->
  Reset controlled->
  Signal controlled ((FINC, FDEC), Bool, Bool, Bool, EbMode)
genericClockControlDemo0 config clkRecovered clkControlled rstControlled drainFifo stabilityCheckReset =
  bundle (speedChangeSticky, underFlowed, overFlowed, isStable, ebMode)
 where
  speedChangeSticky =
    withClockResetEnable clkControlled rstControlled enableGen $
      stickyBits d15 (speedChangeToPins <$> speedChange)
  availableLinkMask = pure $ complement 0 -- all links available
  speedChange = callistoClockControl @1 clkControlled clockControlReset enableGen
    config availableLinkMask (bufferOccupancy :> Nil)
  clockControlReset = unsafeFromLowPolarity $ (==Pass) <$> ebMode

  writeData = pure (0 :: Unsigned 8)

  (bufferOccupancy, underFlowed, overFlowed, ebMode, _) =
    withReset rstControlled $
      resettableXilinxElasticBuffer clkControlled clkRecovered drainFifo writeData

  isStable =
    withClockResetEnable clkControlled stabilityCheckReset enableGen $
      stabilityChecker d2 (SNat @20_000_000) bufferOccupancy

clockControlDemo0 ::
  "clkRecovered" ::: Clock Internal ->
  "clkControlled" ::: Clock External ->
  "rstExternal" ::: Reset External ->
  "drainFifo" ::: Reset External ->
  "stabilityCheckReset" ::: Reset External ->
  "" :::Signal External
    (
      "" :::
      ( "FINC" ::: FINC
      , "FDEC" ::: FDEC
      )
    , "Underflowed" ::: Bool
    , "Overflowed" ::: Bool
    , "isStable" ::: Bool
    , "EbMode" ::: EbMode)
clockControlDemo0 = genericClockControlDemo0 clockConfig
 where
  clockConfig :: ClockControlConfig External 12
  clockConfig = $(lift (defClockConfig @External))

-- | Holds any @a@ which has any bits set for @stickyCycles@ clock cycles.
-- On receiving a new @a@ with non-zero bits, it sets the new incoming value as it output
-- and holds it for @stickyCycles@ clock cycles.
stickyBits ::
  forall dom stickyCycles a .
  ( HiddenClockResetEnable dom
  , KnownNat (BitSize a)
  , NFDataX a
  , BitPack a
  , 1 <= stickyCycles) =>
  SNat stickyCycles ->
  Signal dom a ->
  Signal dom a
stickyBits SNat = mealy go (0 , unpack 0)
 where
  go :: (Index stickyCycles, a) -> a -> ((Index stickyCycles, a), a)
  go (count, storedBits) incomingBits = ((nextCount, nextStored), storedBits)
   where
    newIncoming = pack incomingBits /= 0
    predCount = satPred SatZero count
    holdingBits = count /= 0
    (nextStored, nextCount)
      | newIncoming = (incomingBits, maxBound)
      | holdingBits = (storedBits, predCount)
      | otherwise   = (unpack 0, predCount)

makeTopEntity 'clockControlDemo0
makeTopEntity 'clockControlDemo1
