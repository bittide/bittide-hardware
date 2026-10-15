-- SPDX-FileCopyrightText: 2026 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

-- | I2C target models for testing I2C controllers.
module Tests.I2c.Target (
  Mode (..),
  TargetState (..),
  pagedRegisterTarget,
) where

import Clash.Prelude

import Bittide.SharedTypes (Byte)

data Mode
  = -- | Not addressed, wait for a (repeated) START condition
    Ignore
  | -- | Receive the address byte
    RxAddr
  | -- | Receive the register pointer
    RxPtr
  | -- | Receive data, written to the register file
    RxData
  | -- | Transmit data from the register file
    Tx
  deriving (Eq, Show, Generic, NFDataX)

-- | State of 'pagedRegisterTarget'
data TargetState = TargetState
  { prevScl :: Bit
  , prevSda :: Bit
  , mode :: Mode
  , nextMode :: Mode
  -- ^ Mode after the acknowledge bit of a received byte
  , bitCnt :: Index 10
  {- ^ Number of rising SCL edges in the current byte, including the
  acknowledge bit
  -}
  , shift :: Byte
  , controllerAck :: Bit
  , sdaOut :: Bit
  , page :: Byte
  , pointer :: Byte
  , nacks :: Int
  -- ^ Number of address bytes left to not acknowledge
  , pageWrites :: Int
  -- ^ Number of writes to the page register
  , regs :: Vec 4 (Vec 256 Byte)
  -- ^ Register file, only the first four pages are modeled
  }
  deriving (Show, Generic, NFDataX)

{- | Model of an I2C target with a paged register file, like the Si5345: a
register file with a page register at address @0x01@ on every page. Writes start with a register pointer, followed by
data bytes written to consecutive registers. Reads return data from consecutive
registers, starting at the register pointer. The first @nacks@ address bytes
are not acknowledged.

Based on the I2C target model in the tests of @clash-cores@.
-}
pagedRegisterTarget ::
  (HiddenClockResetEnable dom) =>
  -- | 7-bit I2C address of the target
  BitVector 7 ->
  -- | Number of address bytes to not acknowledge
  Int ->
  -- | Bus SCL and SDA
  Signal dom (Bit, Bit) ->
  Signal dom TargetState
pagedRegisterTarget address nacks0 bus = st
 where
  st = register initState (go <$> st <*> bus)
  initState =
    TargetState
      { prevScl = high
      , prevSda = high
      , mode = Ignore
      , nextMode = Ignore
      , bitCnt = 0
      , shift = 0
      , controllerAck = high
      , sdaOut = high
      , page = 0
      , pointer = 0
      , nacks = nacks0
      , pageWrites = 0
      , regs = repeat (repeat 0)
      }

  go s (scl, sda) = (protocol s){prevScl = scl, prevSda = sda}
   where
    sclHigh = s.prevScl == high && scl == high
    startCond = sclHigh && s.prevSda == high && sda == low
    stopCond = sclHigh && s.prevSda == low && sda == high
    rising = s.prevScl == low && scl == high
    falling = s.prevScl == high && scl == low

    protocol st0
      | startCond = st0{mode = RxAddr, bitCnt = 0, sdaOut = high}
      | stopCond = st0{mode = Ignore, bitCnt = 0, sdaOut = high}
      | rising = onRising st0
      | falling = onFalling st0
      | otherwise = st0

    onRising st0 = case st0.mode of
      Ignore -> st0
      Tx | st0.bitCnt == 8 -> st0{bitCnt = 9, controllerAck = sda}
      _
        | st0.bitCnt < 8 && st0.mode /= Tx ->
            st0{bitCnt = st0.bitCnt + 1, shift = truncateB (st0.shift ++# pack sda)}
        | otherwise -> st0{bitCnt = st0.bitCnt + 1}

    onFalling st0 = case (st0.mode, st0.bitCnt) of
      (Ignore, _) -> st0
      -- Release SDA so the controller can acknowledge
      (Tx, 8) -> st0{sdaOut = high}
      -- Byte received, acknowledge or not
      (_, 8) -> receive st0
      -- End of acknowledge bit
      (Tx, 9)
        | st0.controllerAck == low -> transmit st0
        | otherwise -> st0{mode = Ignore, sdaOut = high}
      (_, 9)
        | st0.nextMode == Tx -> transmit st0
        | otherwise -> st0{mode = st0.nextMode, bitCnt = 0, sdaOut = high}
      -- Next bit to transmit
      (Tx, n)
        | n > 0 -> st0{sdaOut = st0.shift ! (7 - fromEnum n)}
      _ -> st0

    receive st0 = case st0.mode of
      RxAddr
        | slice d7 d1 st0.shift /= address -> st0{nextMode = Ignore}
        | st0.nacks > 0 -> st0{nextMode = Ignore, nacks = st0.nacks - 1}
        | lsb st0.shift == high -> ack Tx st0
        | otherwise -> ack RxPtr st0
      RxPtr -> ack RxData st0{pointer = st0.shift}
      _
        | st0.pointer == 0x01 ->
            ack RxData st0{page = st0.shift, pageWrites = st0.pageWrites + 1, pointer = 0x02}
        | otherwise ->
            ack
              RxData
              st0
                { regs = replace pageIx (replace st0.pointer st0.shift (st0.regs !! pageIx)) st0.regs
                , pointer = st0.pointer + 1
                }
       where
        pageIx = truncateB st0.page :: BitVector 2

    ack next st0 = st0{sdaOut = low, nextMode = next}

    transmit st0 =
      st0
        { mode = Tx
        , bitCnt = 0
        , shift = byte
        , sdaOut = msb byte
        , pointer = st0.pointer + 1
        }
     where
      byte
        | st0.pointer == 0x01 = st0.page
        | otherwise = (st0.regs !! (truncateB st0.page :: BitVector 2)) !! st0.pointer
