-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Bittide.ElasticBuffer where

import Clash.Cores.Xilinx.DcFifo
import Clash.Prelude
import GHC.Stack

import Bittide.ClockControl (RelDataCount, targetDataCount)

import qualified Clash.Cores.Extra as CE
import qualified Clash.Explicit.Prelude as E

data EbMode
  = -- | Disable write, enable read
    Drain
  | -- | Enable write, disable read
    Fill
  | -- | Enable write, enable read
    Pass
  deriving (Generic, NFDataX, Eq, Show)

type Underflow = Bool
type Overflow = Bool

ebModeToReadWrite :: EbMode -> (Bool, Bool)
ebModeToReadWrite = \case
  Fill -> (False, True)
  Drain -> (True, False)
  Pass -> (True, True)

{-# OPAQUE sticky #-}

-- | Create a sticky version of a boolean signal.
sticky ::
  (KnownDomain dom) =>
  Clock dom ->
  Reset dom ->
  Signal dom Bool ->
  Signal dom Bool
sticky clk rst a = stickyA
 where
  stickyA = E.register clk rst enableGen False (stickyA .||. a)

{-# OPAQUE xilinxElasticBuffer #-}

{- | An elastic buffer backed by a Xilinx FIFO. It exposes all its control and
monitor signals in its read domain.
-}
xilinxElasticBuffer ::
  forall n readDom writeDom a.
  ( HasCallStack
  , KnownDomain readDom
  , KnownDomain writeDom
  , NFDataX a
  , KnownNat n
  , 4 <= n
  , n <= 17
  ) =>
  Clock readDom ->
  Clock writeDom ->
  -- | Resetting resets the 'Underflow' and 'Overflow' signals, but not the 'RelDataCount'
  -- ones. Make sure to hold the reset at least 3 cycles in both clock domains.
  Reset readDom ->
  Signal readDom EbMode ->
  Signal writeDom a ->
  ( Signal readDom (RelDataCount n)
  , -- Indicates whether the FIFO under or overflowed. This signal is sticky: it
    -- will only deassert upon reset.
    Signal readDom Underflow
  , Signal writeDom Overflow
  , Signal readDom a
  )
xilinxElasticBuffer clkRead clkWrite rstRead ebMode wdata =
  ( -- Note that this is chosen to work for 'RelDataCount' either being
    -- set to 'Signed' with 'targetDataCount' equals 0 or set to
    -- 'Unsigned' with 'targetDataCount' equals 'shiftR maxBound 1 + 1'.
    -- This way, the representation can be easily switched without
    -- introducing major code changes.
    (+ targetDataCount)
      . bitCoerce
      . (+ (-1 - shiftR maxBound 1))
      <$> readCount
  , isUnderflowSticky
  , isOverflowSticky
  , fifoData
  )
 where
  rstWrite = unsafeFromActiveHigh rstWriteBool
  rstWriteBool =
    CE.safeDffSynchronizer clkRead clkWrite False (unsafeToActiveHigh rstRead)

  FifoOut{readCount, isUnderflow, isOverflow, fifoData} =
    dcFifo
      (defConfig @n){dcOverflow = True, dcUnderflow = True}
      clkWrite
      noResetWrite
      clkRead
      noResetRead
      writeData
      readEnable

  -- We make sure to "stickify" the signals in their original domain. The
  -- synchronizer might lose samples depending on clock configurations.
  isUnderflowSticky = sticky clkRead rstRead isUnderflow
  isOverflowSticky = sticky clkWrite rstWrite isOverflow

  -- We don't reset the Xilix FIFO: its reset documentation is self-contradictory
  -- and mentions situations where the FIFO can end up in an unrecoverable state.
  noResetWrite = unsafeFromActiveHigh (pure False)
  noResetRead = unsafeFromActiveHigh (pure False)

  (readEnable, writeEnable) = unbundle (ebModeToReadWrite <$> ebMode)

  writeEnableSynced = CE.safeDffSynchronizer clkRead clkWrite False writeEnable

  writeData = mux writeEnableSynced (Just <$> wdata) (pure Nothing)

{-# OPAQUE resettableXilinxElasticBuffer #-}

{- | Wrapper around 'xilinxElasticBuffer' that contains a state machine to fill the
buffer half-full. When it is, it switches to pass-through mode, which it exports
through its 'EbMode' output. If an underflow or overflow occurs, it switches back
to fill/drain mode.
-}
resettableXilinxElasticBuffer ::
  forall n readDom writeDom a.
  ( KnownDomain writeDom
  , KnownDomain readDom
  , NFDataX a
  , KnownNat n
  , 4 <= n
  , n <= 17
  ) =>
  Clock readDom ->
  Clock writeDom ->
  -- | Resetting resets the 'Underflow' and 'Overflow' signals, but not the 'RelDataCount'
  -- ones. Make sure to hold the reset at least 3 cycles in both clock domains.
  Reset readDom ->
  Signal writeDom a ->
  ( Signal readDom (RelDataCount n)
  , Signal readDom Underflow
  , Signal readDom Overflow
  , Signal readDom EbMode
  , Signal readDom a
  )
resettableXilinxElasticBuffer clkRead clkWrite rstRead wdata =
  (dataCount, under, over1, ebMode, readData)
 where
  (dataCount, under, over, readData) =
    xilinxElasticBuffer @n clkRead clkWrite fifoReset ebMode wdata
  over1 = CE.safeDffSynchronizer clkWrite clkRead False over

  -- Note that resetting the FIFO only affects the under/overflow signals, not
  -- the data count. This means that we need to reset the FIFO only in case of
  -- errors.
  fifoReset = unsafeFromActiveLow stable

  controllerReset = unsafeFromActiveHigh (unsafeToActiveHigh rstRead .||. under .||. over1)

  (ebMode, stable) =
    unbundle
      $ withClockResetEnable clkRead controllerReset enableGen
      $ mealy goControl Drain dataCount

  goControl :: EbMode -> RelDataCount n -> (EbMode, (EbMode, Bool))
  goControl state0 datacount = (state1, (state0, stable0))
   where
    state1 =
      case state0 of
        Drain
          | datacount == minBound -> Fill
          | otherwise -> Drain
        Fill
          | datacount < targetDataCount -> Fill
          | otherwise -> Pass
        Pass -> state0

    stable0 = state0 == Pass
