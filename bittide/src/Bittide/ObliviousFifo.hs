-- SPDX-FileCopyrightText: 2023-2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}

module Bittide.ObliviousFifo where

import GHC.Stack

import Clash.Explicit.Prelude
import Clash.Cores.Xilinx.DcFifo hiding (DataCount)
import Clash.Cores.Xilinx.Xpm (xpmCdcSingle)

import Control.Arrow (second)

import Data.Coerce (coerce)

import Bittide.ClockControl (DataCount)
import Bittide.Counter (domainDiffCounter)
import Bittide.ObliviousFifo.Frame

type Ready = Bool

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
-- requires the real FIFO to not be continuously full for more than
-- 2^16-1 (= 65535) cycles. Note that this would require a read domain
-- whose clock is 65535 times faster than the one of the write
-- domain, which will be rarely a limitation in practice.
--
-- The FIFO supports enable signals for both domains, which can be
-- used to simulate fill and drain behavior.
--
--   * source enabled  && destination enabled  -> FIFO @Pass@
--   * source disabled && destination enabled  -> FIFO @Drain@
--   * source enabled  && destination disabled -> FIFO @Fill@
--   * source disabled && destination disabled -> disabled FIFO
{-# NOINLINE xilinxObliviousFifo #-}
xilinxObliviousFifo ::
  forall n m writeDom readDom a.
  ( HasCallStack
  , KnownDomain readDom
  , KnownDomain writeDom
  , NFDataX a
  , BitPack a
  , KnownNat n
    -- ^ the FIFO is able to hold 2^n-1 virtual elements
  , KnownNat m
    -- ^ the FIFO is able to hold 2^m-1 real data elements
  , 4 <= m, m <= 17
  , m <= n, n <= 65
  ) =>
  Clock writeDom ->
  Enable writeDom ->
  Clock readDom ->
  Reset readDom ->
  Enable readDom ->
  Signal writeDom a ->
  ( Signal readDom (DataCount n)
    -- ^ the virtual elements count
  , Signal readDom (Unsigned m)
    -- ^ the real data elements count
  , Signal readDom (Frame a)
    -- ^ the data frame
  , Signal readDom Ready
    -- ^ a Boolean ready flag indicating that the component has been
    -- initialized
  )
xilinxObliviousFifo clkW enbW clkR rstR enbR inputData =
  (ddcCount, readCount, readData, ddcReadyR)
 where
  FifoOut{..} =
    dcFifo (defConfig @m)
      clkW noResetW
      clkR noResetR
      (coerce <$> dcFifoWriteData)
      readFromDcFifo

  (ddcCount, ddcReadyR) = unbundle $
    domainDiffCounter @n
      clkW rstW enbW
      clkR rstR enbR

  -- We don't reset the Xilix FIFO: its reset documentation is self-contradictory
  -- and mentions situations where the FIFO can end up in an unrecoverable state.
  noResetW = unsafeFromActiveHigh $ pure False
  noResetR = unsafeFromActiveHigh $ pure False

  rstW = unsafeFromActiveLow $ xpmCdcSingle clkR clkW $ unsafeToActiveLow rstR

  ddcReadyW = xpmCdcSingle clkR clkW ddcReadyR

  rstReadyR = unsafeFromActiveLow ddcReadyR
  rstReadyW = unsafeFromActiveLow ddcReadyW

  ---------------------

  dcFifoWriteData =
    mux (not <$> (fromEnable enbW .&&. ddcReadyW)) (pure NoWrite)
      $ mealy clkW rstReadyW enbW transW 0
          $ mux isFull (pure DcFifoIsFull) (SafeToWrite <$> inputData)
   where
    transW :: (BitPack a, KnownNat n) =>
      Unsigned n ->
      -- ^ the internal state holding the accumulated number of lost
      -- frames
      WriteGuard a ->
      -- ^ guarded input data, which may be forwarded to the dcFifo
      ( Unsigned n
        -- ^ the new internal state
      , WriteData (Frame a)
        -- ^ data to be written to the dcFifo
      )
    transW n =
      let n' = satSucc SatBound n
          mb = case compareSNat (SNat @n) (SNat @(BitSize a)) of
            SNatLE -> maxBound
            SNatGT ->
              (zeroExtend ::
                  ( (n - (BitSize a)) + (BitSize a) ~ n
                  , (BitSize a) + (n - (BitSize a)) ~ n
                  ) => Unsigned (BitSize a) -> Unsigned n
              ) maxBound
       in \case
         -- increase the number of lost frames, if the dcFifo is full
         DcFifoIsFull -> (n', NoWrite)
         SafeToWrite d
           -- send the data, if no lost frames have accumulated
           | n == 0    -> (0, Write $ Data d)
           -- send the number of lost frames otherwise,
           | n < mb    -> (0, Write $ Lost $ bitCoerce $ resize n')
           -- where they are split into multiple frames, if they exceed
           -- the maximum of 2^(BitSize a)-1
           | otherwise -> (n - mb + 1, Write $ Lost maxBound)

  ---------------------

  (readData, readFromDcFifo) =
    muxB (not <$> ddcReadyR) (pure Empty, pure False)
      $ second (.&&. (fromEnable enbR))
      $ mealyB clkR rstReadyR enableGen transR 0
          $ rFrame <$> fifoData
                   <*> register clkR rstReadyR enableGen (True, True)
                         (bundle (readFromDcFifo, isEmpty))
   where
    muxB b t f = unbundle $ mux b (bundle t) (bundle f)

    --     fifoData  readFromDcFifo  isEmpty
    rFrame _        (True          , True   ) = Read Empty
    rFrame d        (True          , _      ) = Read d
    rFrame _        (_             , _      ) = NoRead

    -- for simplicity only 'Lost 1' gets currently reported in case of a
    -- lost frame, while technically the number of consecutively lost
    -- packages can be reported up to 2^(BitSize a)-1.
    transR :: (BitPack a, KnownNat n) =>
      Unsigned n ->
      -- ^ the internal state holding the accumulated number of lost
      -- frames
      ReadData (Frame a) ->
      -- ^ the data that is read from the FIFO, if 'fifoRead' was
      -- enabled in the previous cycle
      ( Unsigned n
        -- ^ the new internal state
      , (Frame a, ReadCondition)
        -- ^ the returned frame and some potential `fifoRead` enable
        -- restricton
      )

    -- accumulate lost data via the internal state and disable the
    -- dcFifo until all lost entries have been reported
    -- [INVARIANT: m > 0]
    transR n (Read (Lost m)) =
      let n' = n + (resize @_ @(BitSize a) @n $ bitCoerce $ satPred SatBound m)
       in (n', (LostFrame, OnlyReadIfEnabledAnd (n' == 0)))
    -- if there are no lost frames left to report and data arrives, then
    -- we report the data
    transR 0 (Read dataFrame) =
      (0, (dataFrame, ReadIfEnabled))
    -- we disable the dcFifo in case of any outstanding frames present,
    -- hence it cannot happen that we receive any data in that case
    transR _ (Read _) =
      deepErrorX "impossible"
    -- if there is neither data nor outstanding lost frames, then the
    -- oblivious FIFO must not be enabled
    transR 0 NoRead =
      (0, (deepErrorX "Oblivious FIFO - Enable off", ReadIfEnabled))
    -- report lost frames still to be reported, even if there is no data
    -- from the dcFifo
    transR n NoRead =
      let n' = satPred SatBound n in
      (n', (LostFrame, OnlyReadIfEnabledAnd (n' == 0)))


newtype WriteGuard a = WriteGuard (Maybe a) deriving (Bundle)

pattern DcFifoIsFull :: WriteGuard a
pattern DcFifoIsFull = WriteGuard Nothing

pattern SafeToWrite :: a -> WriteGuard a
pattern SafeToWrite d = WriteGuard (Just d)

{-# COMPLETE DcFifoIsFull, SafeToWrite #-}

newtype WriteData a = WriteData (Maybe a) deriving (Bundle)

pattern NoWrite :: WriteData a
pattern NoWrite = WriteData Nothing

pattern Write :: a -> WriteData a
pattern Write d = WriteData (Just d)

{-# COMPLETE NoWrite, Write #-}

newtype ReadData a = ReadData (Maybe a) deriving (Bundle)

pattern NoRead :: ReadData a
pattern NoRead = ReadData Nothing

pattern Read :: a -> ReadData a
pattern Read d = ReadData (Just d)

{-# COMPLETE NoRead, Read #-}

type ReadCondition = Bool

pattern OnlyReadIfEnabledAnd :: Bool -> Bool
pattern OnlyReadIfEnabledAnd a = a

pattern ReadIfEnabled :: Bool
pattern ReadIfEnabled = True

pattern LostFrame :: (NFDataX a, BitPack a) => Frame a
pattern LostFrame = Lost 1
