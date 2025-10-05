-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Bittide.ElasticBuffer where

import Bittide.ClockControl (RelDataCount, targetDataCount)
import Bittide.Extra.Maybe (orNothing)
import Clash.Cores.Xilinx.DcFifo
import Clash.Cores.Xilinx.Xpm.Cdc.Extra (safeXpmCdcHandshake)
import Clash.Prelude
import GHC.Stack
import Protocols (Ack (..))

import qualified Clash.Cores.Extra as CE
import qualified Clash.Explicit.Prelude as E

data EbCommand
  = -- | Disable write, enable read
    Drain
  | -- | Enable write, disable read
    Fill
  deriving (Generic, NFDataX, Eq, Show)

type Underflow = Bool
type Overflow = Bool
type Stable = Bool

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
  -- | Operating mode of the elastic buffer. Must remain stable until an acknowledgement
  -- is received.
  Signal readDom (Maybe EbCommand) ->
  Signal writeDom a ->
  ( Signal readDom (RelDataCount n)
  , -- Indicates whether the FIFO under or overflowed. This signal is sticky: it
    -- will only deassert upon reset.
    Signal readDom Underflow
  , Signal writeDom Overflow
  , Signal readDom a
  , -- Acknowledgement for EbMode
    Signal readDom Ack
  )
xilinxElasticBuffer clkRead clkWrite rstRead command wdata =
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
  , commandAck
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

  drain = command .== Just Drain
  commandAck = mux drain drainAck otherAck
  otherAck = pure $ Ack True
  readEnable = command ./= Just Fill

  -- TODO: Instead of hoisting over a flag to drain a single element, we could
  --       hoist over a count to drain multiple elements at once. This would
  --       significantly reduce the overhead induced by the handshake.
  drainFlag = orNothing <$> drain <*> pure high
  drainSynced = drainFlagSynced .== Just high
  writeData = mux drainSynced (pure Nothing) (Just <$> wdata)
  (drainAck, drainFlagSynced) =
    safeXpmCdcHandshake
      clkRead
      E.noReset
      clkWrite
      E.noReset
      drainFlag
      (pure (Ack True))

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
  , Signal readDom Stable
  , Signal readDom a
  )
resettableXilinxElasticBuffer clkRead clkWrite rstRead wdata =
  (dataCount, under, over1, stable, readData)
 where
  (dataCount, under, over, readData, commandAck) =
    xilinxElasticBuffer @n clkRead clkWrite fifoReset ebMode wdata
  over1 = CE.safeDffSynchronizer clkWrite clkRead False over

  -- Note that resetting the FIFO only affects the under/overflow signals, not
  -- the data count. This means that we need to reset the FIFO only in case of
  -- errors.
  fifoReset = unsafeFromActiveLow stable

  controllerReset = unsafeFromActiveHigh (unsafeToActiveHigh rstRead .||. under .||. over1)

  (ebMode, stable) =
    withClockResetEnable clkRead controllerReset enableGen
      $ mealyB goControl InReset (commandAck, dataCount)

  -- TODO: Moving the elastic buffers to their midpoints should happen in software
  goControl ::
    FillControlState ->
    (Ack, RelDataCount n) ->
    ( FillControlState
    , (Maybe EbCommand, Stable)
    )
  goControl state0 (~(Ack ack), datacount) = (state1, (command, state1 == Stable))
   where
    command = case state0 of
      Filling -> Just Fill
      Draining -> Just Drain
      _ -> Nothing

    state1 =
      case state0 of
        InReset -> Filling
        Filling | datacount > targetDataCount && ack -> Draining
        Draining | datacount <= targetDataCount && ack -> Stable
        _ -> state0

data FillControlState
  = InReset
  | Filling
  | Draining
  | Stable
  deriving (Eq, Show, Generic, NFDataX)
