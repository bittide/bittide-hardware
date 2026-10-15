-- SPDX-FileCopyrightText: 2026 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{- | I2C interface for the Si5345 clock generator on the FMC card. It provides
the same register based interface as "Bittide.ClockControl.Si539xSpi", so
software can use the chip in the same way.
-}
module Bittide.ClockControl.Si5345I2c (
  -- * Memory mapped interface
  si5345I2cWb,

  -- * Driver
  si5345I2cDriver,
  si5345I2cDriverDebug,
  DriverDebug (..),
  DriverState (..),
  Step (..),

  -- * Pins
  I2cIn,
  I2cOut,
  splitSdaPins,
) where

import Clash.Prelude hiding (PeriodToCycles)

import Data.Maybe (fromMaybe, isJust)
import GHC.Stack (HasCallStack)

import Clash.Class.BitPackC (ByteOrder)
import Clash.Cores.I2c (I2cIn, I2cOut, i2c)

import Protocols
import Protocols.MemoryMap (Access (..), Mm)
import Protocols.MemoryMap.Registers.WishboneStandard (
  RegisterConfig (access),
  deviceConfig,
  deviceWbI,
  registerConfig,
  registerWbI,
  registerWbI_,
 )

import Bittide.Arithmetic.Time
import Bittide.ClockControl.Si539xSpi (Address, Busy, Page, RegisterOperation (..))
import Bittide.Extra.Maybe
import Bittide.SharedTypes

-- | I2C byte transfer the driver is currently performing.
data Step
  = -- | Address the chip to write the page register
    PageTarget
  | -- | Select the page register
    PageRegister
  | -- | Write the page, followed by a STOP condition
    PageData
  | -- | Address the chip to write the register address
    RegTarget
  | -- | Write the register address
    RegAddress
  | -- | Write the data byte, followed by a STOP condition
    WriteData
  | -- | Address the chip for reading, with a repeated START condition
    ReadTarget
  | -- | Read a byte, send a NACK, followed by a STOP condition
    ReadData
  | {- | Generate a STOP condition after the chip did not acknowledge a byte.
    The operation is retried afterwards.
    -}
    Abort
  | -- | Issue no command for a cycle after losing arbitration, then retry
    Retry
  deriving (Show, Eq, Generic, NFDataX, BitPack)

-- | State of 'si5345I2cDriver'.
data DriverState = DriverState
  { cachedPage :: Maybe Page
  -- ^ Page currently selected on the chip, if known
  , operation :: Maybe RegisterOperation
  -- ^ Operation in progress
  , step :: Step
  -- ^ Byte transfer in progress, only relevant when 'operation' is 'Just'
  }
  deriving (Show, Generic, NFDataX)

{- | First 'Step' of an operation. The page register only needs to be written
when the page differs from the page currently selected on the chip.
-}
firstStep :: Maybe Page -> RegisterOperation -> Step
firstStep cachedPage op
  | cachedPage == Just op.page = RegTarget
  | otherwise = PageTarget

{- | Circuitry that performs register operations on a Si5345 using an I2C
controller. Each register operation is performed as:

  * if the page differs from the last page written: write the page register
    (@0x01@)
  * for a write: write the register address followed by the data byte
  * for a read: write the register address, then read a single byte after a
    repeated START condition

If the chip does not acknowledge a byte, the driver generates a STOP condition,
forgets the selected page, and retries the operation. If arbitration is lost,
the driver retries the operation as well.

The driver accepts a new operation when it is not 'Busy'. Once the operation
has finished, it outputs the byte read (or the byte written) for a single
cycle. It does not accept a new operation in that cycle.
-}
si5345I2cDriver ::
  forall dom minSclPeriodPs.
  (HiddenClockResetEnable dom) =>
  -- | Minimum period of SCL. The Si5345 supports up to 400 kHz.
  SNat minSclPeriodPs ->
  -- | 7-bit I2C address of the chip
  BitVector 7 ->
  -- | Read or write operation for the Si5345 registers
  Signal dom (Maybe RegisterOperation) ->
  -- | Sampled I2C bus
  Signal dom I2cIn ->
  {- |
  1. Byte returned by read / write operation, valid for a single cycle
  2. The driver is 'Busy' and does not accept new operations
  3. I2C bus drivers
  -}
  ( Signal dom (Maybe Byte)
  , Signal dom Busy
  , Signal dom I2cOut
  )
si5345I2cDriver minSclPeriod i2cAddress incomingOp i2cIn = (result, busy, i2cOut)
 where
  (result, busy, i2cOut, _debug) =
    si5345I2cDriverDebug minSclPeriod i2cAddress incomingOp i2cIn

-- | Debug information of 'si5345I2cDriverDebug'.
data DriverDebug = DriverDebug
  { nackCount :: Unsigned 16
  -- ^ Number of bytes the chip did not acknowledge, saturating
  , arbitrationLostCount :: Unsigned 16
  -- ^ Number of times arbitration was lost, saturating
  , busState :: BitVector 8
  {- ^ From MSB to LSB: sampled SCL, sampled SDA, whether the driver is 'Busy',
  a zero bit, and the current 'Step' (only relevant while 'Busy')
  -}
  }
  deriving (Show, Generic, NFDataX)

-- | Like 'si5345I2cDriver', but also outputs 'DriverDebug'.
si5345I2cDriverDebug ::
  forall dom minSclPeriodPs.
  (HiddenClockResetEnable dom) =>
  SNat minSclPeriodPs ->
  BitVector 7 ->
  Signal dom (Maybe RegisterOperation) ->
  Signal dom I2cIn ->
  ( Signal dom (Maybe Byte)
  , Signal dom Busy
  , Signal dom I2cOut
  , Signal dom DriverDebug
  )
si5345I2cDriverDebug SNat i2cAddress incomingOp i2cIn = (result, busy, i2cOut, debug)
 where
  -- The I2C controller takes five clock divider periods per SCL period, and
  -- needs a divider of at least 3.
  clkCnt :: Unsigned 16
  clkCnt = max 3 (natToNum @(DivRU (PeriodToCycles dom minSclPeriodPs) 5) - 1)

  (dout, hostAck, _busBusy, arbitrationLost, ackOut, i2cOut) =
    i2c (pure clkCnt) start stop rd wr ackIn din i2cIn

  state = register initState nextState
  initState = DriverState{cachedPage = Nothing, operation = Nothing, step = PageTarget}

  (start, stop, rd, wr, ackIn, din) = unbundle (command <$> state)
  busy = isJust . (.operation) <$> state

  (nextState, result, nackSeen) =
    unbundle (go <$> state <*> incomingOp <*> hostAck <*> arbitrationLost <*> ackOut <*> dout)

  debug = DriverDebug <$> nackCount <*> arbitrationLostCount <*> busState
  nackCount = register 0 (countIf <$> nackSeen <*> nackCount)
  arbitrationLostCount = register 0 (countIf <$> arbitrationLost <*> arbitrationLostCount)
  countIf event n = if event then satSucc SatBound n else n
  busState = toBusState <$> i2cIn <*> busy <*> state
  toBusState (scl, sda) isBusy s =
    pack scl ++# pack sda ++# pack isBusy ++# (0 :: BitVector 1) ++# pack s.step

  targetByte :: Bool -> Byte
  targetByte isRead = i2cAddress ++# boolToBV isRead

  -- Start, stop, read, write, ACK to send after reading, and byte to write
  command :: DriverState -> (Bool, Bool, Bool, Bool, Bool, Byte)
  command DriverState{operation = Nothing} = (False, False, False, False, False, 0)
  command DriverState{operation = Just op, step} = case step of
    PageTarget -> (True, False, False, True, False, targetByte False)
    PageRegister -> (False, False, False, True, False, pageRegister)
    PageData -> (False, True, False, True, False, op.page)
    RegTarget -> (True, False, False, True, False, targetByte False)
    RegAddress -> (False, False, False, True, False, op.address)
    WriteData -> (False, True, False, True, False, fromMaybe 0 op.write)
    ReadTarget -> (True, False, False, True, False, targetByte True)
    -- Send a NACK after the byte, as it is the last byte we read
    ReadData -> (False, True, True, False, True, 0)
    Abort -> (False, True, False, False, False, 0)
    Retry -> (False, False, False, False, False, 0)

  -- Next state, the result of the operation, and whether the chip did not
  -- acknowledge a byte
  go ::
    DriverState ->
    Maybe RegisterOperation ->
    Bool ->
    Bool ->
    Bool ->
    Byte ->
    (DriverState, Maybe Byte, Bool)
  go s@DriverState{operation = Nothing} newOp _ _ _ _ =
    (s{operation = newOp, step = maybe PageTarget (firstStep s.cachedPage) newOp}, Nothing, False)
  go s@DriverState{cachedPage, operation = Just op, step} _ ack lost nack readByte
    -- The controller released the bus, start over.
    | lost = (s{cachedPage = Nothing, step = Retry}, Nothing, False)
    | step == Retry = (restart, Nothing, False)
    | not ack = (s, Nothing, False)
    -- The chip did not acknowledge a byte we wrote.
    | nack && step /= ReadData && step /= Abort = (s{cachedPage = Nothing, step = Abort}, Nothing, True)
    | otherwise = case step of
        PageTarget -> (s{step = PageRegister}, Nothing, False)
        PageRegister -> (s{step = PageData}, Nothing, False)
        PageData -> (s{cachedPage = Just op.page, step = RegTarget}, Nothing, False)
        RegTarget -> (s{step = RegAddress}, Nothing, False)
        RegAddress -> (s{step = maybe ReadTarget (const WriteData) op.write}, Nothing, False)
        WriteData -> (done, op.write, False)
        ReadTarget -> (s{step = ReadData}, Nothing, False)
        ReadData -> (done, Just readByte, False)
        Abort -> (restart, Nothing, False)
        Retry -> (restart, Nothing, False)
   where
    restart = s{cachedPage = Nothing, step = PageTarget}
    done = DriverState{cachedPage, operation = Nothing, step = PageTarget}

  -- Register that selects the page, available on every page.
  pageRegister :: Address
  pageRegister = 0x01
{-# OPAQUE si5345I2cDriverDebug #-}

{- | A memory-mapped version of 'si5345I2cDriver'. It has the same registers as
'Bittide.ClockControl.Si539xSpi.si539xSpiWb', so software can treat both
devices the same.
-}
si5345I2cWb ::
  forall aw minSclPeriodPs dom.
  ( HiddenClockResetEnable dom
  , KnownNat aw
  , 4 <= aw
  , HasCallStack
  , ?byteOrder :: ByteOrder
  ) =>
  -- | Minimum period of SCL. The Si5345 supports up to 400 kHz.
  SNat minSclPeriodPs ->
  -- | 7-bit I2C address of the chip
  BitVector 7 ->
  Circuit
    ( (ToConstBwd Mm, Bitbone dom aw)
    , CSignal dom I2cIn
    )
    ( "done" ::: CSignal dom Bool
    , CSignal dom I2cOut
    )
si5345I2cWb minSclPeriod i2cAddress =
  circuit $ \((mm, wb), Fwd i2cIn) -> do
    [wb0, wb1, wb2, wb3, wb4, wb5, wb6, wb7] <- deviceWbI (deviceConfig "Si5345I2c") -< (mm, wb)

    Fwd (regOp, _regOpActivity) <- registerWbI regOpConfig defRegOp -< (wb0, Fwd noWrite)
    Fwd (commit, _) <-
      registerWbI commitConfig False -< (wb1, Fwd (fmap (const False) <$> readData))
    registerWbI_ readDataConfig 0 -< (wb2, Fwd readData)

    -- We have no way of knowing when a controlled clock starts running, so we
    -- use a status register to communicate from software to hardware that
    -- configuration is done and the clock is running.
    (done, _doneActivity) <- registerWbI doneConfig False -< (wb3, Fwd noWrite)

    -- Forgets the selected page. Kept to match the interface of 'si539xSpiWb'.
    Fwd (resetReg, _resetActivity) <- registerWbI resetConfig False -< (wb4, Fwd noWrite)

    -- Debug information, to find out why an operation does not finish
    registerWbI_ nackCountConfig 0 -< (wb5, Fwd (Just . (.nackCount) <$> debug))
    registerWbI_ arbitrationLostCountConfig 0
      -< (wb6, Fwd (Just . (.arbitrationLostCount) <$> debug))
    registerWbI_ busStateConfig 0 -< (wb7, Fwd (Just . (.busState) <$> debug))

    let
      maybeRegOp = toMaybe <$> commit <*> regOp
      reset = unsafeFromActiveHigh resetReg
      (readData, _busy, i2cOut, debug) =
        withReset reset $ si5345I2cDriverDebug minSclPeriod i2cAddress maybeRegOp i2cIn

    idC -< (done, Fwd i2cOut)
 where
  regOpConfig =
    (registerConfig "register_operation" "Used to read from or write to a register on the Si5345.")
      { access = WriteOnly
      }

  commitConfig =
    (registerConfig "commit" "Commit the register_operation and start an I2C transaction.")
      { access = ReadWrite
      }

  readDataConfig =
    (registerConfig "read_data" "Data returned by read / write operation.")
      { access = ReadOnly
      }

  doneConfig =
    (registerConfig "done" "Set after configuration is done.")
      { access = WriteOnly
      }

  resetConfig =
    (registerConfig "reset" "Reset the I2C driver, so it selects the page again.")
      { access = WriteOnly
      }

  nackCountConfig =
    (registerConfig "nack_count" "Number of bytes the chip did not acknowledge, saturating.")
      { access = ReadOnly
      }

  arbitrationLostCountConfig =
    (registerConfig "arbitration_lost_count" "Number of times arbitration was lost, saturating.")
      { access = ReadOnly
      }

  busStateConfig =
    ( registerConfig
        "bus_state"
        "From MSB to LSB: sampled SCL, sampled SDA, busy, zero, and the current driver step."
    )
      { access = ReadOnly
      }

  noWrite = pure Nothing

  defRegOp = RegisterOperation{page = 0, address = 0, write = Nothing}

{- | Connect 'I2cOut' to the pins used on the FMC card: SCL is a bidirectional
pin, SDA uses a separate output and input pin. The released SDA output is
driven high. While the controller drives SDA, it reads back its own value.

Returns the sampled bus, the bidirectional SCL pin, and the SDA output pin.
-}
splitSdaPins ::
  (HasCallStack) =>
  -- | Bidirectional SCL pin
  BiSignalIn 'PullUp dom 1 ->
  -- | SDA input pin
  Signal dom Bit ->
  -- | I2C bus drivers
  Signal dom I2cOut ->
  (Signal dom I2cIn, BiSignalOut 'PullUp dom 1, Signal dom Bit)
splitSdaPins sclIn sdaIn i2cOut = (i2cIn, sclOut, sdaOut)
 where
  sclDrive = (\(sclO, sclOen, _, _) -> toMaybe (not sclOen) sclO) <$> i2cOut
  sdaDrive = (\(_, _, sdaO, sdaOen) -> toMaybe (not sdaOen) sdaO) <$> i2cOut

  sclOut = writeToBiSignal sclIn sclDrive
  sdaOut = fromMaybe high <$> sdaDrive
  i2cIn = bundle (readFromBiSignal sclIn, fromMaybe <$> sdaIn <*> sdaDrive)
