-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
module Bittide.ElasticBuffer where

import Clash.Prelude
import Protocols

import Bittide.ClockControl (RelDataCount, targetDataCount)
import Bittide.Df (unsafeFromDf)
import Bittide.ElasticBuffer.AutoCenter (autoCenter)
import Bittide.Extra.Maybe (toMaybe)
import Bittide.SharedTypes (BitboneMm)
import Bittide.Shutter (shutter)
import Clash.Class.BitPackC (ByteOrder)
import Clash.Class.Cdc.Handshake (safeHandshake)
import Clash.Cores.Xilinx.DcFifo
import Data.Maybe (isJust)
import GHC.Stack (HasCallStack)
import Protocols.Df (CollectMode (..), roundrobinCollect)
import Protocols.Df.Extra (ackWhen, skid)
import Protocols.MemoryMap (Access (..))
import Protocols.MemoryMap.Registers.WishboneStandard (
  BusActivity (BusWrite),
  RegisterConfig (..),
  busActivityWrite,
  deviceConfig,
  deviceWbI,
  registerConfig,
  registerWb,
  registerWbDfI,
  registerWbI,
  registerWbI_,
  registerWb_,
 )

import qualified Clash.Class.Cdc as Cdc
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

{-# OPAQUE stickyE #-}

-- | Create a sticky version of a boolean signal.
stickyE ::
  (KnownDomain dom) =>
  Clock dom ->
  Reset dom ->
  Signal dom Bool ->
  Signal dom Bool
stickyE clk rst a = stickyA
 where
  stickyA = E.register clk rst enableGen False (stickyA .||. a)

sticky ::
  (HiddenClockResetEnable dom) =>
  Signal dom Bool ->
  Signal dom Bool
sticky s = stickyE hasClock hasReset s

data ElasticBufferData a
  = -- | No valid data present, because FIFO was empty
    Empty
  | -- | No valid data preset, because CPU requested a fill
    FillCycle
  | -- | FIFO is in passthrough mode and was not empty
    Data a
  deriving (Generic, NFDataX, Eq, Show, Functor)

fromData :: ElasticBufferData a -> a
fromData (Data dat) = dat
fromData FillCycle = errorX "ElasticBufferData.fromData: FillCycle has no data"
fromData Empty = errorX "ElasticBufferData.fromData: Empty has no data"

fromElasticBufferData :: a -> ElasticBufferData a -> a
fromElasticBufferData _ (Data dat) = dat
fromElasticBufferData dflt _ = dflt

data DcFifoInput (readDom :: Domain) (writeDom :: Domain) (a :: Type) = DcFifoInput
  { writeData :: Signal writeDom (Maybe a)
  , readEnable :: Signal readDom Bool
  }
  deriving (Generic, NFDataX)

data DcFifoOutput (n :: Nat) (readDom :: Domain) (writeDom :: Domain) (a :: Type) = DcFifoOutput
  { dataCount :: Signal readDom (DataCount n)
  , underflow :: Signal readDom Underflow
  , overflow :: Signal writeDom Overflow
  , fifoOut :: Signal readDom (ElasticBufferData a)
  }
  deriving (Generic, NFDataX)

instance Protocol (DcFifoOutput n readDom writeDom a) where
  type Fwd (DcFifoOutput n readDom writeDom a) = DcFifoOutput n readDom writeDom a
  type Bwd (DcFifoOutput _ _ _ _) = ()

data DcFifoIO (n :: Nat) (readDom :: Domain) (writeDom :: Domain) (a :: Type)

instance Protocol (DcFifoIO n readDom writeDom a) where
  type Fwd (DcFifoIO n readDom writeDom a) = DcFifoInput readDom writeDom a
  type Bwd (DcFifoIO n readDom writeDom a) = DcFifoOutput n readDom writeDom a

type DcFifoC
  (n :: Nat)
  (readDom :: Domain)
  (writeDom :: Domain)
  (a :: Type) =
  ( HasCallStack
  , KnownDomain readDom
  , KnownDomain writeDom
  , NFDataX a
  , KnownNat n
  ) =>
  Circuit (DcFifoIO n readDom writeDom a) ()

elasticBufferAutoCenter ::
  forall readDom writeDom vendor.
  ( HasCallStack
  , KnownDomain readDom
  , KnownDomain writeDom
  , Cdc.HiddenVendor vendor
  , Cdc.ValidHandshake vendor (Unsigned 32) readDom writeDom
  ) =>
  Clock readDom ->
  Reset readDom ->
  Clock writeDom ->
  Reset writeDom ->
  {- | Operating mode of the elastic buffer. Must remain stable until an acknowledgement
  is received. Negative values drain, positive values fill, zero is a no-op.
  -}
  Signal readDom (Maybe EbAdjustment) ->
  (Signal writeDom Bool, Signal readDom Bool, Signal readDom Ack)
elasticBufferAutoCenter clkRead rstRead clkWrite rstWrite adjustment =
  (drain, readEnable, adjustmentAck)
 where
  -- Muxing between drain and fills:
  adjustmentAck = selectAck <$> adjustment <*> drainAck <*> fillAck
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
      rstRead
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
  (drainAckWrite, drain) =
    E.mooreB clkWrite rstWrite enableGen goActState goActOutput Nothing maybeDrainCmd
  (drainAck, maybeDrainCmd) =
    safeHandshake @vendor
      clkRead
      E.noReset
      clkWrite
      E.noReset
      (maybe Nothing toDrainMaybe <$> adjustment)
      drainAckWrite

elasticBufferControl ::
  forall n readDom writeDom addrW a vendor.
  ( HasCallStack
  , HasSynchronousReset readDom
  , KnownDomain readDom
  , KnownDomain writeDom
  , NFDataX a
  , KnownNat n
  , n <= 32
  , KnownNat addrW
  , Cdc.HiddenVendor vendor
  , Cdc.ValidPulse vendor Bool writeDom readDom
  , Cdc.ValidHandshake vendor (Unsigned 32) readDom writeDom
  , ?byteOrder :: ByteOrder
  ) =>
  Clock readDom ->
  Reset readDom ->
  -- | Local counter
  Signal readDom (Unsigned 64) ->
  Clock writeDom ->
  Reset writeDom ->
  Signal writeDom a ->
  Circuit
    (BitboneMm readDom addrW)
    (DcFifoIO n readDom writeDom a, CSignal readDom (RelDataCount n))
elasticBufferControl clkRead rstRead localCounter clkWrite rstWrite wdata =
  Circuit go
 where
  go (wbFwd, (dcFifoOut, _)) =
    let
      fn = toSignals goC
      (wbBwd, dcFifoIn) = fn (wbFwd, dcFifoOut)
     in
      (wbBwd, (dcFifoIn, relDataCount))
   where
    relDataCount =
      -- Note that this is chosen to work for 'RelDataCount' either being
      -- set to 'Signed' with 'targetDataCount' equals 0 or set to
      -- 'Unsigned' with 'targetDataCount' equals 'shiftR maxBound 1 + 1'.
      -- This way, the representation can be easily switched without
      -- introducing major code changes.
      (+ targetDataCount)
        . bitCoerce
        . (+ (-1 - shiftR maxBound 1))
        <$> dcFifoOut.dataCount

    goC :: Circuit (BitboneMm readDom addrW) (DcFifoIO n readDom writeDom a)
    goC =
      withClockResetEnable clkRead rstRead enableGen $ circuit $ \wb -> do
        [ wbAdjustmentAsync
          , wbAdjustmentWait
          , wbDataCount
          , wbUnderflow
          , wbOverflow
          , wbLocalCounterUnderflow
          , wbLocalCounterOverflow
          , wbClearStatusRegisters
          , wbAutoCenterReset
          , wbAutoCenterEnable
          , wbAutoCenterMargin
          , wbAutoCenterIsIdle
          , wbAutoCenterTotalAdjustments
          , wbMinDataCountSeen
          , wbMaxDataCountSeen
          ] <-
          deviceWbI (deviceConfig "ElasticBuffer") -< wb

        (_ebAdjustmentAsync, ebAdjustmentAsyncDfActivity) <-
          registerWbDfI @_ @_ @4
            ( registerConfig
                "adjustment_async"
                "Submit an adjustment. Will stall if an adjustment is still in progress."
            )
              { access = WriteOnly
              }
            (0 :: EbAdjustment)
            -< (wbAdjustmentAsync, Fwd (pure Nothing))

        (_ebAdjustmentWait, ebAdjustmentWaitDfActivity) <-
          registerWbDfI
            ( registerConfig
                "adjustment_wait"
                "Wait until ready to (immediately) accept a new adjustment"
            )
              { access = WriteOnly
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

        -- Auto-centering state machine
        (_autoCenterReset, Fwd autoCenterResetActivity) <-
          registerWbI
            ( registerConfig
                "auto_center_reset_unchecked"
                "Clear total adjustments. You must disable the state machine and wait for it to be idle before resetting it. After resetting, you must also wait for the state machine to become 'idle' again to make sure the registers are cleared."
            )
              { access = WriteOnly
              }
            ()
            -< (wbAutoCenterReset, Fwd (pure Nothing))

        (Fwd autoCenterEnable, _autoCenterEnableActivity) <-
          registerWbI
            (registerConfig "auto_center_enable" "Enable auto-centering state machine")
              { access = ReadWrite
              }
            False
            -< (wbAutoCenterEnable, Fwd (pure Nothing))

        (Fwd autoCenterMargin, _autoCenterMarginActivity) <-
          registerWbI
            (registerConfig "auto_center_margin" "Margin for auto-centering")
              { access = ReadWrite
              }
            (2 :: Unsigned 16)
            -< (wbAutoCenterMargin, Fwd (pure Nothing))

        registerWbI_
          (registerConfig "auto_center_is_idle" "Whether the auto-centering state machine is idle")
            { access = ReadOnly
            }
          False
          -< (wbAutoCenterIsIdle, Fwd (Just <$> autoCenterIsIdle))

        registerWbI_
          ( registerConfig
              "auto_center_total_adjustments"
              "Total adjustments applied by the auto-centering state machine"
          )
            { access = ReadOnly
            }
          (0 :: Signed 32)
          -< (wbAutoCenterTotalAdjustments, Fwd (Just <$> autoCenterTotalAdjustments))

        let
          autoCenterReset =
            unsafeFromActiveHigh (isJust . busActivityWrite <$> autoCenterResetActivity)

        (autoCenterAdjustmentDf, Fwd autoCenterTotalAdjustments, Fwd autoCenterIsIdle) <-
          autoCenter
            (autoCenterReset `E.orReset` rstRead)
            (toEnable autoCenterEnable)
            autoCenterMargin
            relDataCount
            -< ()

        -- Multiplex manual and auto-center adjustments using round-robin collection
        ebAdjustmentDfMuxed <-
          roundrobinCollect @2 Parallel -< [ebAdjustmentDf1, autoCenterAdjustmentDf]

        let
          (writeEnable, readEnable, adjustmentAck) =
            elasticBufferAutoCenter @readDom @writeDom
              clkRead
              rstRead
              clkWrite
              rstWrite
              ebAdjustmentSig

          writeData = mux writeEnable (pure Nothing) (Just <$> wdata)

        Fwd ebAdjustmentSig <- unsafeFromDf -< (ebAdjustmentDfMuxed, Fwd adjustmentAck)

        -- Synchronize overflow pulse from write domain to read domain
        let overflow1 = Cdc.pulse @vendor clkWrite clkRead dcFifoOut.overflow

        let
          isFirstRising :: Signal readDom Bool -> Signal readDom Bool
          isFirstRising = E.isRising clkRead flagsReset enableGen False . stickyE clkRead flagsReset

          flagsReset :: Reset readDom
          flagsReset =
            E.orReset rstRead (unsafeFromActiveHigh (clearStatusRegisters .== Just (BusWrite True)))

        localCounterUnderflow <- shutter (isFirstRising dcFifoOut.underflow) -< Fwd localCounter
        localCounterOverflow <- shutter (isFirstRising overflow1) -< Fwd localCounter

        let
          minDataCountSeen1 :: Signal readDom (RelDataCount n)
          minDataCountSeen1 = min <$> minDataCountSeen0 <*> relDataCount

          maxDataCountSeen1 :: Signal readDom (RelDataCount n)
          maxDataCountSeen1 = max <$> maxDataCountSeen0 <*> relDataCount

        registerWbI_
          (registerConfig "data_count" ""){access = ReadOnly}
          0
          -< (wbDataCount, Fwd (Just <$> relDataCount))

        -- Status registers
        registerWb_
          clkRead
          flagsReset
          (registerConfig "underflow" "Sticky underflow flag; can be cleared by writing false")
            { access = ReadOnly
            }
          False
          -< (wbUnderflow, Fwd (flip toMaybe True <$> dcFifoOut.underflow))

        registerWb_
          clkRead
          flagsReset
          (registerConfig "underflow_timestamp" "Local counter value when first underflow occurred")
            { access = ReadOnly
            }
          0
          -< (wbLocalCounterUnderflow, localCounterUnderflow)

        registerWb_
          clkRead
          flagsReset
          (registerConfig "overflow" "Sticky overflow flag; can be cleared by writing false")
            { access = ReadOnly
            }
          False
          -< (wbOverflow, Fwd (flip toMaybe True <$> overflow1))

        registerWb_
          clkRead
          flagsReset
          (registerConfig "overflow_timestamp" "Local counter value when first overflow occurred")
            { access = ReadOnly
            }
          0
          -< (wbLocalCounterOverflow, localCounterOverflow)

        (Fwd minDataCountSeen0, _i0) <-
          registerWb
            clkRead
            flagsReset
            (registerConfig "min_data_count_seen" ""){access = ReadOnly}
            maxBound
            -< (wbMinDataCountSeen, Fwd (Just <$> minDataCountSeen1))

        (Fwd maxDataCountSeen0, _i1) <-
          registerWb
            clkRead
            flagsReset
            (registerConfig "max_data_count_seen" ""){access = ReadOnly}
            minBound
            -< (wbMaxDataCountSeen, Fwd (Just <$> maxDataCountSeen1))

        (_cf, Fwd clearStatusRegisters) <-
          registerWbI
            ( registerConfig
                "clear_status_registers"
                "Clear the underflow and overflow sticky flags, their respective timestamps and the min/max data count seen registers."
            )
              { access = WriteOnly
              }
            False
            -< (wbClearStatusRegisters, Fwd (pure Nothing))

        let
          dcFifoIn = DcFifoInput{writeData = writeData, readEnable = readEnable}

        applyC (const dcFifoIn) (const ()) -< ()

joinEbAndControl ::
  forall n readDom writeDom addrW a vendor.
  ( HasCallStack
  , HasSynchronousReset readDom
  , KnownDomain readDom
  , KnownDomain writeDom
  , KnownNat n
  , n <= 32
  , NFDataX a
  , KnownNat addrW
  , Cdc.HiddenVendor vendor
  , Cdc.ValidPulse vendor Bool writeDom readDom
  , Cdc.ValidHandshake vendor (Unsigned 32) readDom writeDom
  , ?byteOrder :: ByteOrder
  ) =>
  Circuit
    (BitboneMm readDom addrW)
    (DcFifoIO n readDom writeDom a, CSignal readDom (RelDataCount n)) ->
  DcFifoC n readDom writeDom a ->
  Circuit
    (BitboneMm readDom addrW)
    (DcFifoOutput n readDom writeDom a, CSignal readDom (RelDataCount n))
joinEbAndControl controlC fifoC = Circuit go
 where
  Circuit controlFn = controlC
  Circuit fifoFn = fifoC

  go (bitboneFwd, _) = (bitboneBwd, (dcFifoOut, relDataCount))
   where
    (bitboneBwd, (dcFifoIn, relDataCount)) = controlFn (bitboneFwd, (dcFifoOut, ()))
    (dcFifoOut, _) = fifoFn (dcFifoIn, ())
