-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Bittide.ElasticBuffer where

import Bittide.ClockControl (RelDataCount, targetDataCount)
import Bittide.Df
import Bittide.Extra.Maybe (orNothing)
import Bittide.SharedTypes (BitboneMm)
import Clash.Class.BitPackC (ByteOrder)
import Clash.Cores.Xilinx.DcFifo
import Clash.Cores.Xilinx.Xpm.Cdc.Extra (safeXpmCdcHandshake)
import Clash.Cores.Xilinx.Xpm.Cdc.Pulse (xpmCdcPulse)
import Clash.Prelude
import GHC.Stack
import Protocols (Ack (..), CSignal, Circuit (..), applyC)
import Protocols.Df.Extra (ackWhen, skid)
import Protocols.MemoryMap (Access (..))
import Protocols.MemoryMap.Registers.WishboneStandard (
  RegisterConfig (..),
  busActivityWrite,
  deviceWb,
  registerConfig,
  registerWbDfI,
  registerWbI,
 )

import qualified Clash.Explicit.Prelude as E

{- | Elastic buffer adjustment command. Negative values drain (remove frames), positive
values fill (add frames).
-}
type EbAdjustment = Signed 32

{- | Extract magnitude if the adjustment is a drain (negative).

Returns the absolute value of the adjustment as an unsigned integer when the
adjustment is negative, otherwise returns Nothing.

\$setup
>>> import Clash.Prelude

>>> toDrainMaybe (-5)
Just 5
>>> toDrainMaybe (-1)
Just 1
>>> toDrainMaybe 0
Nothing
>>> toDrainMaybe 5
Nothing
>>> toDrainMaybe minBound  -- Handle edge case: -2147483648
Just 2147483648
-}
toDrainMaybe :: EbAdjustment -> Maybe (Unsigned 32)
toDrainMaybe adj
  | adj < 0 = Just (bitCoerce (truncateB (negate (extend adj :: Signed 33))))
  | otherwise = Nothing

{- | Extract magnitude if the adjustment is a fill (positive).

Returns the adjustment value as an unsigned integer when the adjustment is
positive, otherwise returns Nothing.

>>> toFillMaybe 5
Just 5
>>> toFillMaybe 1
Just 1
>>> toFillMaybe 0
Nothing
>>> toFillMaybe (-5)
Nothing
>>> toFillMaybe maxBound  -- Maximum positive value: 2147483647
Just 2147483647
-}
toFillMaybe :: EbAdjustment -> Maybe (Unsigned 32)
toFillMaybe adj
  | adj > 0 = Just (bitCoerce adj)
  | otherwise = Nothing

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

data ElasticBufferData a
  = -- | No valid data present, because FIFO was empty
    Empty
  | -- | No valid data preset, because CPU requested a fill
    FillCycle
  | -- | FIFO is in passthrough mode and was not empty
    Data a
  deriving (Generic, NFDataX, Eq, Show)

fromData :: ElasticBufferData a -> a
fromData (Data dat) = dat
fromData FillCycle = errorX "ElasticBufferData.fromData: FillCycle has no data"
fromData Empty = errorX "ElasticBufferData.fromData: Empty has no data"

fromElasticBufferData :: a -> ElasticBufferData a -> a
fromElasticBufferData _ (Data dat) = dat
fromElasticBufferData dflt _ = dflt

toElasticBufferData :: Bool -> Bool -> a -> ElasticBufferData a
toElasticBufferData requestedReadInPreviousCycle underflow a
  | not requestedReadInPreviousCycle = FillCycle
  | underflow = Empty
  | otherwise = Data a

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
  -- | Operating mode of the elastic buffer. Must remain stable until an acknowledgement
  -- is received. Negative values drain, positive values fill, zero is a no-op.
  Signal readDom (Maybe EbAdjustment) ->
  -- | Data to write into the elastic buffer. Will be ignored for a single cycle
  -- when it gets a drain adjustment (negative value). Which cycle this is depends on
  -- clock domain crossing.
  Signal writeDom a ->
  ( Signal readDom (RelDataCount n)
  , Signal readDom Underflow
  , Signal writeDom Overflow
  , Signal readDom (ElasticBufferData a)
  , -- Acknowledgement for EbMode
    Signal readDom Ack
  )
xilinxElasticBuffer clkRead clkWrite adjustment wdata =
  ( -- Note that this is chosen to work for 'RelDataCount' either being
    -- set to 'Signed' with 'targetDataCount' equals 0 or set to
    -- 'Unsigned' with 'targetDataCount' equals 'shiftR maxBound 1 + 1'.
    -- This way, the representation can be easily switched without
    -- introducing major code changes.
    (+ targetDataCount)
      . bitCoerce
      . (+ (-1 - shiftR maxBound 1))
      <$> readCount
  , isUnderflow
  , isOverflow
  , fifoOut
  , adjustmentAck
  )
 where
  FifoOut{readCount, isUnderflow, isOverflow, fifoData} =
    dcFifo
      (defConfig @n){dcOverflow = True, dcUnderflow = True}
      clkWrite
      noResetWrite
      clkRead
      noResetRead
      writeData
      readEnable

  -- We don't reset the Xilinx FIFO: its reset documentation is self-contradictory
  -- and mentions situations where the FIFO can end up in an unrecoverable state.
  noResetWrite = unsafeFromActiveHigh (pure False)
  noResetRead = unsafeFromActiveHigh (pure False)

  -- Muxing between drain and fills:
  adjustmentAck = selectAck <$> adjustment <*> drainAck <*> fillAck
  fifoOut = toElasticBufferData <$> readEnableDelayed <*> isUnderflow <*> fifoData
  readEnableDelayed = E.register clkRead noResetRead enableGen False readEnable
  readEnable = not <$> fill

  selectAck :: Maybe EbAdjustment -> Ack -> Ack -> Ack
  selectAck adj dAck fAck = case adj of
    Just a
      | a < 0 -> dAck -- Drain
      | a > 0 -> fAck -- Fill
      | otherwise -> Ack True -- Zero: immediate ack for no-op
    Nothing -> errorX "xilinxElasticBuffer: No adjustment to acknowledge"

  -- Fill logic:
  (fillAck, fill) =
    E.mooreB
      clkRead
      noResetRead
      enableGen
      goActState
      goActOutput
      Nothing
      (maybe Nothing toFillMaybe <$> adjustment)

  goActState :: Maybe (Unsigned 32) -> Maybe (Unsigned 32) -> Maybe (Unsigned 32)
  goActState Nothing i = i
  goActState (Just 0) _ = Nothing
  goActState (Just n) _ = Just (n - 1)

  goActOutput :: Maybe (Unsigned 32) -> (Ack, Bool)
  goActOutput Nothing = (Ack False, False)
  goActOutput (Just 0) = (Ack True, False)
  goActOutput (Just _) = (Ack False, True)

  -- Drain logic (CDC based):
  writeData = mux drain (pure Nothing) (Just <$> wdata)
  (drainAckWrite, drain) =
    E.mooreB clkWrite noResetWrite enableGen goActState goActOutput Nothing maybeDrainCmd
  (drainAck, maybeDrainCmd) =
    safeXpmCdcHandshake
      clkRead
      E.noReset
      clkWrite
      E.noReset
      (maybe Nothing toDrainMaybe <$> adjustment)
      drainAckWrite

{-# OPAQUE xilinxElasticBufferWb #-}

{- | Wishbone wrapper around 'xilinxElasticBuffer' that exposes control and monitoring
via memory-mapped registers using the clash-protocols-memmap infrastructure.

This component allows software control of the elastic buffer's buffer occupancies and
provides monitoring capabilities. The primary use case is to enable a CPU to perform
buffer initialization during the system startup phase.

The registers provided are:
1. Command Register (Write-Only): Adding or removing singular frames from the buffer.
2. Data Count Register (Read-Only): Current fill level of the buffer.
3. Underflow Register (Read-Write): Sticky flag indicating if an underflow has occurred.
   Flag can be cleared by writing false to this register.
4. Overflow Register (Read-Write): Sticky flag indicating if an overflow has occurred.
   Flag can be cleared by writing false to this register.
5. Stable Register (Write-Only): Software can set this flag to indicate that the buffer
   is stable.
-}
xilinxElasticBufferWb ::
  forall n readDom writeDom addrW a.
  ( HasCallStack
  , KnownDomain readDom
  , KnownDomain writeDom
  , NFDataX a
  , KnownNat n
  , 4 <= n
  , n <= 17
  , KnownNat addrW
  , ?busByteOrder :: ByteOrder
  , ?regByteOrder :: ByteOrder
  ) =>
  Clock readDom ->
  Reset readDom ->
  SNat n ->
  Clock writeDom ->
  Signal writeDom a ->
  Circuit
    (BitboneMm readDom addrW)
    ( CSignal readDom (RelDataCount n)
    , CSignal readDom Underflow
    , CSignal readDom Overflow
    , CSignal readDom (ElasticBufferData a)
    )
xilinxElasticBufferWb clkRead rstRead SNat clkWrite wdata =
  withClockResetEnable clkRead rstRead enableGen $ circuit $ \wb -> do
    [ wbAdjustmentAsync
      , wbAdjustmentWait
      , wbDataCount
      , wbUnderflow
      , wbOverflow
      ] <-
      deviceWb "ElasticBuffer" -< wb

    (_ebAdjustmentAsync, ebAdjustmentAsyncDfActivity) <-
      registerWbDfI
        (registerConfig "adjustment_async")
          { access = WriteOnly
          , description =
              "Submit an adjustment. Will stall if an adjustment is still in progress."
          }
        (0 :: EbAdjustment)
        -< (wbAdjustmentAsync, Fwd (pure Nothing))

    (_ebAdjustmentWait, ebAdjustmentWaitDfActivity) <-
      registerWbDfI
        (registerConfig "adjustment_wait")
          { access = WriteOnly
          , description = "Wait until ready to (immediately) accept a new adjustment"
          }
        ()
        -< (wbAdjustmentWait, Fwd (pure Nothing))

    ebAdjustmentDf0 <- applyC (fmap busActivityWrite) id -< ebAdjustmentAsyncDfActivity

    -- [Note Skid Buffer]
    --
    -- By putting a skid buffer here, we ensure that we can immediately accept a new adjustment
    -- when writing to `adjustment_go`. We then use the 'ready' signal from the skid buffer to
    -- implement the 'adjustment_wait' register.
    (ebAdjustmentDf1, Fwd ebReady) <- skid -< ebAdjustmentDf0
    ackWhen ebReady -< ebAdjustmentWaitDfActivity

    let (dataCount, underflow, overflow0, readData, adjustmentAck) =
          xilinxElasticBuffer @n clkRead clkWrite ebAdjustmentSig wdata

    Fwd ebAdjustmentSig <- unsafeFromDf -< (ebAdjustmentDf1, Fwd adjustmentAck)

    -- Synchronize overflow pulse from write domain to read domain
    let overflow1 = xpmCdcPulse clkWrite clkRead overflow0

    (dataCountOut, _dataCountActivity) <-
      registerWbI
        (registerConfig "data_count"){access = ReadOnly}
        0
        -< (wbDataCount, Fwd (Just <$> dataCount))

    (underflowOut, _underflowActivity) <-
      registerWbI
        (registerConfig "underflow")
          { access = ReadWrite
          , description = "Sticky underflow flag; can be cleared by writing false"
          }
        False
        -< (wbUnderflow, Fwd (flip orNothing True <$> underflow))

    (overflowOut, _overflowActivity) <-
      registerWbI
        (registerConfig "overflow")
          { access = ReadWrite
          , description = "Sticky overflow flag; can be cleared by writing false"
          }
        False
        -< (wbOverflow, Fwd (flip orNothing True <$> overflow1))

    idC -< (dataCountOut, underflowOut, overflowOut, Fwd readData)
