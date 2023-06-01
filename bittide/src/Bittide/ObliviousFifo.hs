-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Bittide.ObliviousFifo where

import GHC.Stack

import Clash.Explicit.Prelude
import Clash.Cores.Xilinx.DcFifo hiding (DataCount)
import qualified Clash.Cores.Extra as CE

import Data.Constraint.Nat.Extra (Dict(..), leMaxLeft, leMaxRight)

import Bittide.ClockControl (DataCount, targetDataCount)
import Bittide.ElasticBuffer (EbMode(..), ebModeToReadWrite)
import Bittide.Counter (domainDiffCounter)

-- | A 'Frame' separates data frames into valid data or accumulated
-- lost data. Furthermore, it is also used to indicate an empty buffer.
data Frame n a =
    Data a
    -- ^ a single frame holding some data
  | Empty
    -- ^ the empty buffer indication frame
  | Lost (Unsigned n)
    -- ^ some number of frames whose data got lost
  deriving (Eq, Ord, Show, Generic, NFDataX)

instance (BitPack a, KnownNat n) => BitPack (Frame n a) where
  -- 'Frame' either holds some counted lost frames or valid data. The
  -- state space is shared among the two.
  type BitSize(Frame n a) = 1 + Max n (BitSize a)

  -- The most significant bit is used to distinguish between
  --
  --   * data (the MSB is not set) and
  --   * lost frame counters or the empty buffer indication frame
  --     (the MSB is set).
  --
  -- The empty buffer indication frame is bit-mapped to zero (except
  -- for the MSB), while lost counters are mapped to everything above
  -- zero (except for the MSB). Note that there must be always at
  -- least one frame that got lost.
  pack = \case
    Empty  -> (`setBit` (natToNum @(Max n (BitSize a)))) 0
    Lost n -> (`setBit` (natToNum @(Max n (BitSize a)))) $
      case leMaxLeft @n @(BitSize a) @0 of
        Dict -> ( zeroExtend ::
                    ( 1 + Max n (BitSize a) - n + n
                    ~ 1 + Max n (BitSize a)
                    ) =>
                    BitVector n ->
                    BitVector (1 + Max n (BitSize a))
                ) $ pack n
    Data x ->
      case leMaxRight @n @(BitSize a) @0 of
        Dict ->
          ( zeroExtend ::
              ( (1 + Max n (BitSize a) - BitSize a) + BitSize a
              ~ 1 + Max n (BitSize a)
              ) =>
              BitVector (BitSize a) ->
              BitVector (1 + Max n (BitSize a))
          ) $ pack x

  unpack bv
    | testBit bv (natToNum @(Max n (BitSize a))) =
        let tbv = case leMaxLeft @n @(BitSize a) @0 of
                    Dict ->
                      ( truncateB ::
                          ( n + (1 + Max n (BitSize a) - n)
                          ~ 1 + Max n (BitSize a)
                          ) =>
                          BitVector (1 + Max n (BitSize a)) ->
                          BitVector n
                      ) bv
        in case unpack tbv of
          0 -> Empty
          n -> Lost n
    | otherwise = Data $ unpack $
        case leMaxRight @n @(BitSize a) @0 of
          Dict ->
            ( truncateB ::
                ( BitSize a + (1 +  Max n (BitSize a) - BitSize a)
                ~ 1 + Max n (BitSize a)
                ) =>
                BitVector (1 + Max n (BitSize a)) ->
                BitVector (BitSize a)
            ) bv

-- | This FIFO combines 'Clash.Cores.Xilinx.DcFifo.dcFifo' with
-- 'Bittide.Counter.domainDiffCounter' to create an oblivious FIFO
-- that supports a much larger _virtual_ data counter than the number
-- of elements that it can actually hold. The FIFO is able to hold
-- @2^n-1@ virtual elements and @2^m-1@ real data elements, where @n@
-- can be much larger than @m@. When its real size @m@ is exceeded,
-- then the FIFO does not loose the frames, but only forgets about
-- their data. This means, that the frames are still counted,
-- "traverse" the FIFO and are output at the other end in the same
-- order as they are written to the component. The only difference is
-- that frames, which are written to the FIFO, when its real capacity
-- is already utilized, are output with a @lost@ status instead.
--
-- Due to the maximum depth of 17 of
-- 'Clash.Cores.Xilinx.DcFifo.dcFifo', the current implementation
-- requires the real FIFO to be not continuously full for more than
-- 2^16-1 (= 65535) cycles. Note that this would require a read domain
-- whose clock is 65535 times faster than the one of the write
-- domain. Hence, this should only rarely be a limitation in practice.

{-# NOINLINE xilinxObliviousFifo #-}
xilinxObliviousFifo ::
  forall n m readDom writeDom a.
  ( HasCallStack
  , KnownDomain readDom
  , KnownDomain writeDom
  , NFDataX a
  , KnownNat n  -- the FIFO is able to hold 2^n-1 virtual elements
  , KnownNat m  -- the FIFO is able to hold 2^m-1 real data elements
  , 4 <= m, m <= 17
  , 16 <= n, n <= 65
  ) =>
  SNat m ->
  Clock readDom ->
  Clock writeDom ->
  Reset readDom ->
  Signal readDom EbMode ->
  Signal writeDom a ->
  ( Signal readDom (DataCount n)
  , Signal readDom (Frame 16 a)
  )
xilinxObliviousFifo SNat clkRead clkWrite rstRead ebMode wData =
  -- the FIFO remains passive until the 'domainDiffCounter' gets ready
  unbundle $ mux ready (pure (0, Empty)) $ bundle
    ( -- Note that this is chosen to work for 'DataCount' either being
      -- set to 'Signed' with 'targetDataCount' equals 0 or set to
      -- 'Unsigned' with 'targetDataCount' equals 'shiftR maxBound 1 + 1'.
      -- This way, the representation can be easily switched without
      -- introducing major code changes.
      (+ targetDataCount) . bitCoerce . (+ (-1 - shiftR maxBound 1))
        <$> diffCount
    , -- always return the empty frame if the FIFO is empty
      readData
    )
 where
  FifoOut{readCount = _readCount, isEmpty, isFull, fifoData = rData} =
    dcFifo (defConfig @16)
      clkWrite noResetWrite
      clkRead noResetRead
      writeData fifoRead

  (diffCount, ready) = unbundle $
    domainDiffCounter @n
      clkWrite resetGen (toEnable writeEnableSynced)
      clkRead resetGen (toEnable readEnable)

  -- We don't reset the Xilix FIFO: its reset documentation is self-contradictory
  -- and mentions situations where the FIFO can end up in an unrecoverable state.
  noResetWrite = unsafeFromHighPolarity (pure False)
  noResetRead = unsafeFromHighPolarity (pure False)

  (readEnable, writeEnable) = unbundle (ebModeToReadWrite <$> ebMode)

  rstWriteSynced = unsafeToReset $
    CE.safeDffSynchronizer clkRead clkWrite False (not <$> ready)

  writeEnableSynced =
    CE.safeDffSynchronizer clkRead clkWrite False writeEnable

  ---------------------

  writeData =
    mealy clkWrite rstWriteSynced enableGen goW 0
      $ bundle (wData, isFull, writeEnableSynced)

  goW ::
    (KnownNat n, n <= 65) =>
    Unsigned n -> (a, Bool, Bool) ->
    (Unsigned n, Maybe (Frame 16 a))

  --  c  wData  isFull  writeEnableSynced  c'         writeData
  goW n (_    , _     , False         ) = (n        , Nothing             )
  goW 0 (d    , False , True          ) = (0        , Just (Data d)       )
  goW 0 (_    , True  , True          ) = (1        , Nothing             )
  goW n (_    , True  , True          ) = (n+1      , Nothing             )
  goW n (_    , False , True          )
    | n > 65534                         = (n - 65534, Just (Lost 65535))
    | otherwise                         = (0        , Just (Lost (tb n+1)))

  tb :: (KnownNat n, (n - 16) + 16 ~ n) => Unsigned n -> Unsigned 16
  tb = truncateB

  ---------------------

  (readData, fifoRead) = unbundle $
    mealy clkRead rstRead enableGen goR (0, True)
      $ bundle (mux isEmpty (pure Empty) rData, readEnable)

  -- for simplicity only 'Lost 1' gets currently reported in case of a
  -- lost frame, while technically the number of consecutively lost
  -- packages can be reported up to 2%16.
  goR ::
    (KnownNat n, n <= 65) =>
    (Unsigned n, Bool) -> (Frame 16 a, Bool) ->
    ((Unsigned n, Bool), (Frame 16 a, Bool))

  --   c  _re     rData   re      c'     re    readData    fifoRead
  goR (n, True ) (Lost m, re) = ((n+ze m-1, re), (Lost 1    , n+ze m == 1 && re))
  goR (n, False) (Lost m, re) = ((n+ze m  , re), (undefined , False            ))
  goR (0, _    ) (mdata , re) = ((0       , re), (mdata     , re               ))
  goR (n, True ) (_     , re) = ((n-1     , re), (Lost 1    , n == 1           ))
  goR (n, False) (_     , re) = ((n       , re), (undefined , False            ))


  ze :: (KnownNat n, (n - 16) + 16 ~ n) => Unsigned 16 -> Unsigned n
  ze = zeroExtend

  ---------------------

  -- the FIFO should preserve the following invariant:
  -- readCount < (maxbound :: Unsigned 16) `implies` readCount == diffCount
