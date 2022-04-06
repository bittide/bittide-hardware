-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Bittide.ElasticBuffer ( elasticBuffer ) where

import Clash.Explicit.Prelude

import Clash.Cores.Xilinx.DcFifo.Explicit (XilinxFifo (..), dcFifo, DcConfig (..), defConfig)

push :: a -> Vec (n+1) (Maybe a) -> Vec (n+1) (Maybe a)
push x xs = Just x `Cons` init xs

type Overflow = Bool
type Underflow = Bool

-- Xilinx FIFO IP core
xFifo ::
  forall depth write read n.
  ( KnownNat depth
  , KnownNat n
  , KnownDomain write
  , KnownDomain read
  , depth <= 17
  , 4 <= depth
  ) =>
  -- | Buffer depth; i.e. number of elements in the FIFO.
  SNat depth ->

  -- | Write domain clock/reset
  Clock write ->

  -- | Read domain clock/reset
  Clock read -> Reset read -> Enable read ->

  -- | Maybe write frame
  Signal write (Maybe (BitVector n)) ->

  -- | Read a frame?
  Signal read Bool ->

  -- | Occupancy count, underflow, word read
  ( Signal write Overflow
  , Signal read (Signed depth, Underflow, BitVector n)
  )
xFifo d wClk rClk rRst rEna dat rd =
  -- TODO: rEna doesn't get used as rd
  (over, bundle (ix, under, fifoDat))
 where
  ~(XilinxFifo _ _ over _ _ _ under cnt fifoDat) =
    dcFifo ((defConfig @depth) { dcOverflow = True, dcUnderflow = True }) wClk rClk rRst dat rd
  ix = subtract (snatToNum d `div` 2) . fromIntegral <$> cnt

data DFEBController depth = TrackWait -- wait til half full
                          | Measure (Vec 5 (Maybe (Signed depth))) -- allow both but wait to see if distance is a problem
                          | Sync -- hope it doesn't over-/underflow
                          deriving (Generic)

instance NFDataX (DFEBController depth) where

elasticBuffer ::
  ( KnownDomain core
  , KnownDomain recovered
  , KnownNat depth
  , KnownNat n
  , 4 <= depth
  , depth <= 17
  ) =>
  SNat depth ->
  Clock recovered -> Reset recovered -> Enable recovered ->
  Clock core -> Reset core -> Enable core ->
  Signal recovered (BitVector n) ->
  ( Signal core (BitVector n) -- frames out of elastic buffer
  , Signal core Bool -- assert stable
  , Signal core Bool -- over-/underflow
  , Signal core (Signed depth) -- distance from center
  )
elasticBuffer d rClk rRst rEna cClk cRst cEna fromSerdes =
  (datum, expectStable, isU .||. isOCore, dist)
  where

    (isO, rest) =
      fifo d rClk rRst rEna cClk cRst cEna fromSerdes controlState

    (dist, isU, datum) = unbundle rest

    isOCore = dualFlipFlopSynchronizer rClk cClk cRst cEna False isO

    dOverflow :: (KnownNat depth, 1 <= depth) => Signed depth
    dOverflow = snatToNum d -- TODO: does this coerce wrong?

    (expectStable, controlState) = elasticBufferControl d cClk cRst cEna dist

elasticBufferControl ::
  ( KnownNat depth
  , 1 <= depth
  , KnownDomain core
  ) =>
  SNat depth ->
  Clock core -> Reset core -> Enable core ->
  -- | Distance from middle
  Signal core (Signed depth) ->
  ( Signal core Bool -- bool: expect stable
  , Signal core WaitReset
  )
elasticBufferControl d cClk cRst cEna =
  mealyB cClk cRst cEna step start
  where
    start :: DFEBController depth
    start = TrackWait

    dSus :: (KnownNat depth, 1 <= depth) => Signed depth
    dSus = snatToNum d `div` 4

    step :: (KnownNat depth, 1 <= depth) => DFEBController depth -> Signed depth -> (DFEBController depth, (Bool, WaitReset))
    step TrackWait dist | dist < 0 = (TrackWait, (False, DisableReads))
                        | dist > 0 = (TrackWait, (False, DisableWrites))
                        | otherwise = (Measure (Nothing :> Nothing :> Nothing :> Nothing :> Nothing :> Nil), (False, Don'tWait))

    step (Measure (Just 0 :> Just 0 :> Just 0 :> Just 0 :> Just 0 :> Nil)) 0 = (Sync, (False, Don'tWait))
    -- if it's wildly out of wack, go to TrackWait
    step Measure{} dist
      | dist >= dSus || dist <= negate dSus
        = (TrackWait, (False, if dist > 0 then DisableWrites else DisableReads))
    step (Measure v) x = (Measure (push x v), (False, Don'tWait))
    step Sync _ = (Sync, (True, Don'tWait))

data WaitReset = DisableReads | DisableWrites | Don'tWait deriving (Generic)

instance NFDataX WaitReset where

fifo ::
  ( KnownNat depth
  , KnownNat n
  , KnownDomain core
  , KnownDomain recovered
  , 4 <= depth
  , depth <= 17
  ) =>
  -- | Buffer depth; i.e. number of elements in the FIFO.
  SNat depth ->

  -- | Clock/Reset/Enable belonging to the recovered clock from the SERDES
  Clock recovered -> Reset recovered -> Enable recovered ->

  -- | Clock/Reset/Enable belonging to the core clock
  Clock core -> Reset core -> Enable core ->

  -- | Frame coming from the SERDES
  Signal recovered (BitVector n) ->

  -- | Read a frame?
  Signal core WaitReset ->

  ( Signal recovered Overflow
  ,
    Signal core
      ( Signed depth -- ^ Distance from middle
      , Underflow
      , BitVector n
      )
  )
fifo sd rClk rRst rEna cClk cRst cEna dat rd =
  xFifo sd rClk cClk cRst cEna dat' rd'
 where
  rd' = (\case { DisableReads -> False ; _ -> True }) <$> rd
  rdRecovered = dualFlipFlopSynchronizer cClk rClk rRst rEna Don'tWait rd
  dat' =
    (\x st -> case st of { DisableWrites -> Nothing ; _ -> Just x })
      <$> dat
      <*> rdRecovered
