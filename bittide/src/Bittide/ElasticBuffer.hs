-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE NamedFieldPuns #-}

module Bittide.ElasticBuffer where

import Clash.Prelude
import Clash.Cores.Xilinx.DcFifo
import GHC.Stack

import Bittide.ClockControl (targetDataCount)

import qualified Clash.Explicit.Prelude as E
import qualified Clash.Cores.Extra as CE


data EbMode
  -- | Disable write, enable read
  = Drain
  -- | Enable write, disable read
  | Fill
  -- | Enable write, enable read
  | Pass
 deriving (Generic, NFDataX, Eq, Show)

type Underflow = Bool
type Overflow = Bool

ebModeToReadWrite :: EbMode -> (Bool, Bool)
ebModeToReadWrite = \case
  Fill  -> (False, True)
  Drain -> (True, False)
  Pass  -> (True, True)

-- | Create a sticky version of a boolean signal.
sticky ::
  KnownDomain dom =>
  Clock dom ->
  Reset dom ->
  Signal dom Bool ->
  Signal dom Bool
sticky clk rst a = stickyA
 where
  stickyA = E.register clk rst enableGen False (stickyA .||. a)

-- | An elastic buffer backed by a Xilinx FIFO. It exposes all its control and
-- monitor signals in its read domain.
xilinxElasticBuffer ::
  forall n readDom writeDom.
  ( HasCallStack
  , KnownDomain readDom
  , KnownDomain writeDom
  , KnownNat n
  , 4 <= n, n <= 17
  ) =>
  Clock readDom ->
  Clock writeDom ->
  -- | Resetting resets the 'Underflow' and 'Overflow' signals, but not the 'DataCount'
  -- ones. Make sure to hold the reset at least 3 cycles in both clock domains.
  Reset readDom ->
  Signal readDom EbMode ->
  ( Signal readDom (DataCount n)

  -- Indicates whether the FIFO under or overflowed. This signal is sticky: it
  -- will only deassert upon reset.
  , Signal readDom Underflow
  , Signal writeDom Overflow
  )
xilinxElasticBuffer clkRead clkWrite rstRead ebMode =
  (readCount, isUnderflowSticky, isOverflowSticky)
 where
  rstWrite = unsafeFromHighPolarity rstWriteBool
  rstWriteBool =
    CE.tripleFlipFlopSynchronizer clkRead clkWrite False (unsafeToHighPolarity rstRead)

  FifoOut{readCount, isUnderflow, isOverflow} = dcFifo
    (defConfig @n){dcOverflow=True, dcUnderflow=True}
    clkWrite noResetWrite clkRead noResetRead
    writeData readEnable


  -- We make sure to "stickify" the signals in their original domain. The
  -- synchronizer might lose samples depending on clock configurations.
  isUnderflowSticky = sticky clkRead rstRead isUnderflow
  isOverflowSticky = sticky clkWrite rstWrite isOverflow

  -- We don't reset the Xilix FIFO: its reset documentation is self-contradictory
  -- and mentions situations where the FIFO can end up in an unrecoverable state.
  noResetWrite = unsafeFromHighPolarity (pure False)
  noResetRead = unsafeFromHighPolarity (pure False)

  (readEnable, writeEnable) = unbundle (ebModeToReadWrite <$> ebMode)

  writeEnableSynced = CE.tripleFlipFlopSynchronizer clkRead clkWrite False writeEnable

  -- For the time being, we're only interested in data counts, so we feed the
  -- FIFO with static data when writing (@0@).
  writeData = mux writeEnableSynced (pure (Just (0 :: Unsigned 8))) (pure Nothing)


resettableXilinxElasticBuffer ::
  forall readDom writeDom n.
  ( KnownDomain writeDom
  , KnownDomain readDom
  , KnownNat n
  , 4 <= n, n <= 17) =>
  Clock readDom ->
  Clock writeDom ->
  -- | Resetting resets the 'Underflow' and 'Overflow' signals, but not the 'DataCount'
  -- ones. Make sure to hold the reset at least 3 cycles in both clock domains.
  Reset readDom ->
  ( Signal readDom (DataCount n)
  , Signal readDom Underflow
  , Signal readDom Overflow
  , Signal readDom EbMode
  )
resettableXilinxElasticBuffer clkRead clkWrite rstRead =
  (dataCount, under, over1, ebMode)
 where
  (dataCount, under, over) = xilinxElasticBuffer @n clkRead clkWrite fifoReset ebMode
  fifoReset = unsafeFromHighPolarity $ not <$> stable
  over1 = CE.tripleFlipFlopSynchronizer clkWrite clkRead False over

  controllerReset = unsafeFromHighPolarity (unsafeToHighPolarity rstRead .||. under .||. over1)

  (ebMode, stable) = unbundle $
    withClockResetEnable clkRead controllerReset enableGen $
     mealy goControl Drain dataCount

  goControl :: EbMode -> DataCount n -> (EbMode, (EbMode, Bool))
  goControl state0 datacount = (state1, (state0, stable0))
   where
    state1 =
      case state0 of
        Drain
          | datacount == 0 -> Fill
          | otherwise -> Drain
        Fill
          | datacount < targetDataCount -> Fill
          | otherwise -> Pass
        Pass -> state0

    stable0 = state0 == Pass
