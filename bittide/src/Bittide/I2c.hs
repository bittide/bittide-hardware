-- SPDX-FileCopyrightText: 2026 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE OverloadedStrings #-}

{- | A general purpose I2C controller, controlled by software one byte transfer
at a time. Software builds complete I2C transactions out of 'I2cCommand's, which
makes it suitable for any I2C target.
-}
module Bittide.I2c (
  -- * Commands
  I2cCommand (..),
  I2cTransfer (..),
  I2cAck (..),
  I2cStatus (..),

  -- * Controller
  i2cController,
  i2cControllerWb,

  -- * Pins
  I2cIn,
  I2cOut,
  openDrainPins,
) where

import Clash.Prelude hiding (PeriodToCycles)

import Control.Monad (mfilter)
import Data.Maybe (isJust)
import GHC.Stack (HasCallStack)

import Clash.Class.BitPackC (BitPackC, ByteOrder)
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
import Protocols.MemoryMap.TypeDescription.TH

import Bittide.Arithmetic.Time
import Bittide.Extra.Maybe (toMaybe)
import Bittide.SharedTypes (Bitbone, Byte)

-- | Acknowledge bit sent or received after a byte.
data I2cAck = Acknowledge | NotAcknowledge
  deriving (Show, Eq, Generic, NFDataX, BitPack, BitPackC)

deriveTypeDescription ''I2cAck

-- | Byte transfer to perform as part of an 'I2cCommand'.
data I2cTransfer
  = {- | Do not transfer a byte. Only useful to generate a STOP condition.
    'I2cCommand.start' has no effect without a byte transfer.
    -}
    NoTransfer
  | -- | Write a byte
    WriteByte Byte
  | {- | Read a byte, and send the given acknowledge bit afterwards. Send a
    'NotAcknowledge' after the last byte of a read.
    -}
    ReadByte I2cAck
  deriving (Show, Eq, Generic, NFDataX, BitPack, BitPackC)

deriveTypeDescription ''I2cTransfer

-- | A single step of an I2C transaction.
data I2cCommand = I2cCommand
  { start :: Bool
  -- ^ Generate a (repeated) START condition before the transfer
  , transfer :: I2cTransfer
  -- ^ Byte transfer to perform
  , stop :: Bool
  -- ^ Generate a STOP condition after the transfer
  }
  deriving (Show, Eq, Generic, NFDataX, BitPack, BitPackC)

deriveTypeDescription ''I2cCommand

-- | Status of 'i2cController'.
data I2cStatus = I2cStatus
  { busy :: Bool
  -- ^ A command is in progress. New commands are ignored.
  , finished :: Bool
  {- ^ Asserted for a single cycle when a command has finished, has been aborted
  because arbitration was lost, or has been ignored because it has no effect.
  -}
  , readData :: Byte
  -- ^ Byte read by the last 'ReadByte' command
  , ackReceived :: I2cAck
  -- ^ Acknowledge bit received after the last 'WriteByte' command
  , arbitrationLost :: Bool
  {- ^ The last command was aborted, because another controller took over the
  bus. Software should retry the transaction from the start.
  -}
  , busBusy :: Bool
  {- ^ A START condition was seen on the bus, but no STOP condition since. This
  includes transactions of other controllers.
  -}
  }
  deriving (Show, Generic, NFDataX)

{- | I2C controller that performs one 'I2cCommand' at a time. A command is
accepted when the controller is not busy, other commands are ignored. Commands
that neither transfer a byte nor generate a STOP condition are ignored as well,
as the I2C core would never finish them.

The controller does not accept a new command in the cycle it asserts
'finished', so a command can be held until then.
-}
i2cController ::
  forall dom minSclPeriodPs.
  (HiddenClockResetEnable dom) =>
  -- | Minimum period of SCL
  SNat minSclPeriodPs ->
  -- | New command
  Signal dom (Maybe I2cCommand) ->
  -- | Sampled I2C bus
  Signal dom I2cIn ->
  (Signal dom I2cStatus, Signal dom I2cOut)
i2cController SNat newCmd i2cIn = (status, i2cOut)
 where
  -- The I2C core takes five clock divider periods per SCL period, and needs a
  -- divider of at least 3.
  clkCnt :: Unsigned 16
  clkCnt = max 3 (natToNum @(DivRU (PeriodToCycles dom minSclPeriodPs) 5) - 1)

  (dout, hostAck, busBusy, lost, ackOut, i2cOut) =
    i2c (pure clkCnt) start stop rd wr ackIn din i2cIn

  currentCommand :: Signal dom (Maybe I2cCommand)
  currentCommand = register Nothing (nextCommand <$> currentCommand <*> newCmd <*> hostAck <*> lost)

  nextCommand Nothing new _ _ = mfilter isEffective new
  nextCommand cmd _ ack al
    | ack || al = Nothing
    | otherwise = cmd

  (start, stop, rd, wr, ackIn, din) = unbundle (toCoreInputs <$> currentCommand)

  toCoreInputs Nothing = (False, False, False, False, False, 0)
  toCoreInputs (Just cmd) = case cmd.transfer of
    -- Without a transfer, the I2C core would write a byte after the START
    -- condition, so START is only generated before a byte transfer.
    NoTransfer -> (False, cmd.stop, False, False, False, 0)
    WriteByte b -> (cmd.start, cmd.stop, False, True, False, b)
    ReadByte a -> (cmd.start, cmd.stop, True, False, a == NotAcknowledge, 0)

  -- Results of the last command, updated when it finishes
  readData = regEn 0 (hostAck .&&. isRead <$> currentCommand) dout
  ackReceived = regEn Acknowledge (hostAck .&&. isWrite <$> currentCommand) (toAck <$> ackOut)
  arbitrationLost = register False (nextLost <$> arbitrationLost <*> currentCommand <*> newCmd <*> lost)

  -- Cleared when a new command is accepted
  nextLost _ _ _ True = True
  nextLost _ Nothing (Just cmd) _ | isEffective cmd = False
  nextLost prev _ _ _ = prev

  -- Whether a command makes the I2C core do anything at all.
  isEffective cmd = cmd.transfer /= NoTransfer || cmd.stop

  isRead (Just I2cCommand{transfer = ReadByte _}) = True
  isRead _ = False

  isWrite (Just I2cCommand{transfer = WriteByte _}) = True
  isWrite _ = False

  isFinished Nothing new _ _ = maybe False (not . isEffective) new
  isFinished (Just _) _ ack al = ack || al

  toAck nack = if nack then NotAcknowledge else Acknowledge

  finished = isFinished <$> currentCommand <*> newCmd <*> hostAck <*> lost

  status =
    I2cStatus
      <$> (isJust <$> currentCommand)
      <*> finished
      <*> readData
      <*> ackReceived
      <*> arbitrationLost
      <*> busBusy

{- | Memory-mapped version of 'i2cController'. Software writes a command to
@command@, then sets @commit@, and polls @commit@ until the hardware has cleared
it. Software must not write a new command before that.

The command is only passed to the controller once @commit@ is set. A command
spans multiple bytes, which the CPU may write with multiple bus transactions
in any order, so the controller must not act on a partially written command.
-}
i2cControllerWb ::
  forall aw minSclPeriodPs dom.
  ( HiddenClockResetEnable dom
  , KnownNat aw
  , 4 <= aw
  , HasCallStack
  , ?byteOrder :: ByteOrder
  ) =>
  -- | Minimum period of SCL
  SNat minSclPeriodPs ->
  Circuit
    ((ToConstBwd Mm, Bitbone dom aw), CSignal dom I2cIn)
    (CSignal dom I2cOut)
i2cControllerWb minSclPeriod = circuit $ \((mm, wb), Fwd i2cIn) -> do
  [wbCommand, wbCommit, wbReadData, wbAck, wbLost, wbBusBusy] <-
    deviceWbI (deviceConfig "I2cController") -< (mm, wb)

  Fwd (command, _commandActivity) <-
    registerWbI commandConfig defCommand -< (wbCommand, Fwd (pure Nothing))
  -- Cleared by the hardware once the command has finished. Software polls this
  -- register instead of a separate status register, which avoids a race
  -- between the bus write and the controller picking up the command.
  Fwd (commit, _commitActivity) <-
    registerWbI commitConfig False -< (wbCommit, Fwd clearCommit)
  registerWbI_ readDataConfig 0 -< (wbReadData, Fwd (Just . (.readData) <$> status))
  registerWbI_ ackConfig Acknowledge -< (wbAck, Fwd (Just . (.ackReceived) <$> status))
  registerWbI_ lostConfig False -< (wbLost, Fwd (Just . (.arbitrationLost) <$> status))
  registerWbI_ busBusyConfig False -< (wbBusBusy, Fwd (Just . (.busBusy) <$> status))

  let
    -- Clear commit a cycle after the command finished, so the results are
    -- visible in their registers by the time software sees commit cleared. Hide
    -- the command from the controller in the meantime, so it is not performed
    -- again.
    justFinished = register False ((.finished) <$> status)
    pending = mux (commit .&&. fmap not justFinished) (Just <$> command) (pure Nothing)
    (status, i2cOut) = i2cController minSclPeriod pending i2cIn
    clearCommit = (\f -> toMaybe f False) <$> justFinished

    defCommand = I2cCommand{start = False, transfer = NoTransfer, stop = False}

  idC -< Fwd i2cOut
 where
  commandConfig =
    ( registerConfig
        "command"
        "Command to perform. Only performed once commit is set."
    )
      { access = ReadWrite
      }
  commitConfig =
    ( registerConfig
        "commit"
        "Set to perform the command. Cleared by the hardware once the command has finished."
    )
      { access = ReadWrite
      }
  readDataConfig =
    (registerConfig "read_data" "Byte read by the last ReadByte command.")
      { access = ReadOnly
      }
  ackConfig =
    (registerConfig "ack_received" "Acknowledge bit received after the last WriteByte command.")
      { access = ReadOnly
      }
  lostConfig =
    ( registerConfig
        "arbitration_lost"
        "The last command was aborted, because another controller took over the bus."
    )
      { access = ReadOnly
      }
  busBusyConfig =
    (registerConfig "bus_busy" "A START condition was seen on the bus, but no STOP condition since.")
      { access = ReadOnly
      }

{- | Connect 'I2cOut' to two bidirectional open-drain pins, for SCL and SDA
respectively. Returns the sampled bus and the pin drivers.
-}
openDrainPins ::
  (HasCallStack) =>
  -- | SCL pin
  BiSignalIn 'PullUp dom 1 ->
  -- | SDA pin
  BiSignalIn 'PullUp dom 1 ->
  -- | I2C bus drivers
  Signal dom I2cOut ->
  (Signal dom I2cIn, BiSignalOut 'PullUp dom 1, BiSignalOut 'PullUp dom 1)
openDrainPins sclIn sdaIn i2cOut = (i2cIn, sclOut, sdaOut)
 where
  sclOut = writeToBiSignal sclIn ((\(o, oen, _, _) -> toMaybe (not oen) o) <$> i2cOut)
  sdaOut = writeToBiSignal sdaIn ((\(_, _, o, oen) -> toMaybe (not oen) o) <$> i2cOut)
  i2cIn = bundle (readFromBiSignal sclIn, readFromBiSignal sdaIn)
