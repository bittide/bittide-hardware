-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Bittide.ElasticBuffer where

import Clash.Cores.Xilinx.DcFifo
import Clash.Cores.Xilinx.Xpm.Cdc
import Clash.Prelude
import GHC.Stack
import Data.String.Interpolate (i)

import Bittide.ClockControl (RelDataCount, targetDataCount)
import qualified Clash.Cores.Extra as CE
import qualified Clash.Explicit.Prelude as E

data EbControl
  = -- | Disable write, enable read
    AddFrame
  | -- | Enable write, disable read
    RemoveFrame
  deriving (Generic, NFDataX, Eq, Show)

type Underflow = Bool
type Overflow = Bool
type Stable = Bool

ebControlToReadWrite :: Maybe EbControl -> (Bool, Bool)
ebControlToReadWrite = \case
  Just AddFrame -> (False, True)
  Just RemoveFrame -> (True, False)
  Nothing -> (True, True)

{-# NOINLINE sticky #-}

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

{-# NOINLINE xilinxElasticBuffer #-}

{- | Wrapper around `xpmCdcPulse` that returns an acknowledgement when the pulse is
guaranteed to be transferred to the destination domain. In essence it will
delay acknowledgement of sequential pulses to satisfy the minimum gap requirement
specified in the documentation: (2 * max srcPeriod dstPeriod)
-}
safeXpmCdcPulse ::
  forall src dst.
  ( KnownDomain src
  , KnownDomain dst
  , 1 <= DomainPeriod src
  ) =>
  Clock src ->
  Clock dst ->
  Signal src Bool ->
  (Signal dst Bool, Signal src Bool)
safeXpmCdcPulse clkSrc clkDst inputPulse0 = (outputPulse, inputPulse0 .&&. ack)
 where
  inputPulse1 = E.isRising clkSrc E.noReset enableGen False (inputPulse0 .&&. ack)
  outputPulse = xpmCdcPulse clkSrc clkDst inputPulse1
  cnt0 = 0 :: Index (1 + PeriodToCycles src (2 * Max (DomainPeriod src) (DomainPeriod dst)))
  cnt = E.regEn clkSrc E.noReset enableGen cnt0 runCounter $ satSucc SatWrap <$> cnt
  runCounter = inputPulse1 .||. cnt ./= 0
  ack = cnt .== minBound

{- | An elastic buffer backed by a Xilinx FIFO. It exposes all its control and
monitor signals in its read domain. It's important to note that it takes significantly
more time to remove frames than to add frames because of clock domain crossing.
-}
xilinxElasticBuffer ::
  forall n readDom writeDom a.
  ( HasCallStack
  , KnownDomain readDom
  , KnownDomain writeDom
  , NFDataX a
  , Show a
  , KnownNat n
  , 4 <= n
  , n <= 17
  , 1 <= DomainPeriod readDom
  ) =>
  Clock readDom ->
  Clock writeDom ->
  -- | Resetting resets the 'Underflow' and 'Overflow' signals, but not the 'RelDataCount'
  -- ones. Make sure to hold the reset at least 3 cycles in both clock domains.
  Reset readDom ->
  Signal readDom (Maybe EbControl) ->
  Signal writeDom a ->
  ( Signal readDom (RelDataCount n)
  , -- Indicates whether the FIFO under or overflowed. This signal is sticky: it
    -- will only deassert upon reset.
    Signal readDom Underflow
  , Signal writeDom Overflow
  , Signal readDom Bool
  , Signal readDom a
  )
xilinxElasticBuffer clkRead clkWrite rstRead ebControl wdata
  | periodDiff > 1.26 = -- Prevent rounding errors
    clashCompileError $ [i|Write domain is more than 50% faster than the read domain period(x#{periodDiff})|]
  | otherwise =
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
    , controlAck
    , fifoData
    )
 where

  writePeriod = natToNum @(DomainPeriod writeDom)
  readPeriod = natToNum @(DomainPeriod readDom)
  periodDiff = readPeriod / writePeriod :: Double

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

  (readEnable, writeEnable0) = unbundle (ebControlToReadWrite <$> ebControl)
  (writeWait, controlAck) = safeXpmCdcPulse clkRead clkWrite (fmap not writeEnable0)


  writeData = mux writeWait (pure Nothing) (Just <$> wdata)

{-# NOINLINE resettableXilinxElasticBuffer #-}

-- | Wrapper around `xilinxElasticBuffer` that contains a statemachine to fill the buffer
-- half-full before considering it "stable". It's important to note that it takes significantly
-- more time to remove frames than to add frames because of clock domain crossing.
resettableXilinxElasticBuffer ::
  forall n readDom writeDom a.
  ( KnownDomain writeDom
  , KnownDomain readDom
  , NFDataX a
  , KnownNat n
  , Show a
  , 4 <= n
  , n <= 17
  , 1 <= DomainPeriod readDom
  ) =>
  Clock readDom ->
  Clock writeDom ->
  -- | Resetting resets the 'Underflow' and 'Overflow' signals, but not the 'RelDataCount'
  -- ones. Make sure to hold the reset at least 3 cycles in both clock domains.
  Reset readDom ->
  Signal readDom (Maybe EbControl) ->
  Signal writeDom a ->
  ( Signal readDom (RelDataCount n)
  , Signal readDom Underflow
  , Signal readDom Overflow
  , Signal readDom Stable
  , Signal readDom Bool
  , Signal readDom a
  )
resettableXilinxElasticBuffer clkRead clkWrite rstRead ebControlExternal wdata =
  (dataCount, under, over1, stable, stable .&&. ebControlAck, readData)
 where
  (dataCount, under, over, ebControlAck, readData) =
    xilinxElasticBuffer @n clkRead clkWrite fifoReset ebControl wdata
  fifoReset = unsafeFromActiveHigh $ not <$> stable
  over1 = CE.safeDffSynchronizer clkWrite clkRead False over

  controllerReset = unsafeFromActiveHigh (unsafeToActiveHigh rstRead .||. under .||. over1)

  ebControl = mux stable ebControlExternal ebControlInternal
  ebControlInternal =
    withClockResetEnable clkRead controllerReset enableGen
      $ mealy goControl (Just RemoveFrame) dataCount

  stable = ebControlInternal .== Nothing
  goControl :: Maybe EbControl -> RelDataCount n -> (Maybe EbControl, Maybe EbControl)
  goControl state0 datacount = (state1, state0)
   where
    state1 =
      case state0 of
        Just RemoveFrame
          | datacount == minBound -> Just AddFrame
          | otherwise -> Just RemoveFrame
        Just AddFrame
          | datacount < targetDataCount -> Just AddFrame
          | otherwise -> Nothing
        Nothing -> state0
