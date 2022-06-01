module Bittide.ElasticBuffer ( elasticBuffer ) where

import Clash.Explicit.Prelude

import Clash.Cores.Xilinx.DcFifo.Explicit (XilinxFifo (..), dcFifo, defConfig)

push :: a -> Vec (n+1) (Maybe a) -> Vec (n+1) (Maybe a)
push x xs = Just x `Cons` init xs

type Overflow = Bool
type Underflow = Bool

-- Xilinx FIFO IP core
xFifo ::
  forall depth write read n.
  (KnownNat depth, KnownNat n, KnownDomain write, KnownDomain read, depth <= 17, 4 <= depth) =>
  -- | Buffer depth; i.e. number of elements in the FIFO.
  SNat depth ->

  -- | Write domain clock/reset
  Clock write -> Reset write ->

  -- | Read domain clock/reset
  Clock read -> Reset read ->

  -- | Maybe write frame
  Signal write (Maybe (BitVector n)) ->

  -- | Read a frame?
  Signal read Bool ->

  -- | Occupancy count, overflow, underflow, word read
  Signal read (Index depth, Overflow, Underflow, BitVector n)
xFifo _ wClk _ rClk rRst dat wr =
  bundle (ix, undefined, under, fifoDat)
 where
  (XilinxFifo _ _ _ _ _ _ under cnt fifoDat) = dcFifo (defConfig @depth) wClk rClk rRst dat wr
  ix = fromIntegral <$> cnt

data DFEBController depth = TrackWait -- wait til half full
                          | Measure (Vec 5 (Maybe (Signed (CLog 2 depth)))) -- allow both but wait to see if distance is a problem
                          | Sync -- hope it doesn't over-/underflow
                          deriving (Generic)

instance NFDataX (DFEBController depth) where

elasticBuffer ::
  (KnownDomain core, KnownNat depth, 1 <= depth) =>
  SNat depth ->
  Clock recovered -> Reset recovered -> Enable recovered ->
  Clock core -> Reset core -> Enable core ->
  Signal recovered (BitVector n) ->
  ( Signal core (Maybe (BitVector n)) -- frames out of elastic buffer
  , Signal core Bool -- assert stable
  , Signal core Bool -- over-/underflow
  , Signal core (Signed (CLog 2 depth)) -- distance from center
  )
elasticBuffer d rClk rRst rEna cClk cRst cEna fromSerdes =
  (mDatum, expectStable, isO .&&. isU, dist)
  where

    (dist, isO, isU, mDatum) = unbundle $ fifo d rClk rRst cClk cRst fromSerdes rControlState

    rDist = register cClk cRst cEna 0 dist
    rControlState = register cClk cRst cEna Don'tWait controlState

    (expectStable, controlState) = elasticBufferControl d cClk cRst cEna rDist

elasticBufferControl ::
  (KnownNat depth, 1 <= depth, KnownDomain core) =>
  SNat depth ->
  Clock core -> Reset core -> Enable core ->
  -- | Distance from middle
  Signal core (Signed (CLog 2 depth)) ->
  ( Signal core Bool -- bool: expect stable
  , Signal core WaitReset
  )
elasticBufferControl d cClk cRst cEna =
  mealyB cClk cRst cEna step start
  where
    start :: DFEBController depth
    start = TrackWait

    dSus :: (KnownNat depth, 1 <= depth) => Signed (CLog 2 depth)
    dSus = snatToNum d `div` 4

    step :: (KnownNat depth, 1 <= depth) => DFEBController depth -> Signed (CLog 2 depth) -> (DFEBController depth, (Bool, WaitReset))
    step TrackWait dist | dist < 0 = (TrackWait, (False, DisableReads))
                        | dist > 0 = (TrackWait, (False, DisableWrites))
                        | otherwise = (Measure (Nothing :> Nothing :> Nothing :> Nothing :> Nothing :> Nil), (False, Don'tWait))
    step (Measure (Just 0 :> Just 0 :> Just 0 :> Just 0 :> Just 0 :> Nil)) 0 = (Sync, (False, Don'tWait))
    -- if it's wildly out of wack, go to TrackWait
    step Measure{}  dist
      | dist >= dSus || dist <= negate dSus
        = (TrackWait, (False, if dist > 0 then DisableWrites else DisableReads))
    step (Measure v) x = (Measure (push x v), (False, Don'tWait))
    step Sync _ = (Sync, (True, Don'tWait))

data WaitReset = DisableReads | DisableWrites | Don'tWait deriving (Generic)

instance NFDataX WaitReset where

-- use dualFlipFlopSynchronizer to get overflow in the core domain
fifo ::
  -- | Buffer depth; i.e. number of elements in the FIFO.
  SNat depth ->

  -- | Clock/Reset/Enable belonging to the recovered clock from the SERDES
  Clock recovered -> Reset recovered ->

  -- | Clock/Reset/Enable belonging to the core clock
  Clock core -> Reset core ->

  -- | Frame coming from the SERDES
  Signal recovered (BitVector n) ->

  -- | Read a frame?
  Signal core WaitReset ->

  Signal core
    ( Signed (CLog 2 depth) -- ^ Distance from middle
    , Overflow
    , Underflow
    , Maybe (BitVector n) -- ^ 'Just' @word@ or 'Nothing' if overflow/underflow
    )
fifo = undefined
