-- SPDX-FileCopyrightText: 2023 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE CPP #-}
{-# LANGUAGE ViewPatterns #-}

module Bittide.Instances.Pnr.I2C where

import Clash.Explicit.Prelude hiding (read)

import Clash.Cores.Xilinx.Extra
import Clash.Cores.Xilinx.VIO
import Clash.Cores.Experimental.I2C

import Clash.Annotations.TH (makeTopEntity)

import Data.Maybe

import Bittide.Instances.Domains


data I2CControllerState = I2CIdle | I2CWriteAddress | I2CWriteData | I2CReadData
  deriving (Generic, NFDataX, Eq)

i2cController ::
  (KnownDomain dom) =>
  Clock dom ->
  Reset dom ->
  Enable dom ->
  Signal dom (RamOp 128 (BitVector 8)) ->
  Signal dom Bool ->
  Signal dom Bool ->
  ( Signal dom Bool
  , Signal dom (Maybe I2COperation)
  , Signal dom Bool)
i2cController clk rst ena ramOp' hostAck' arbLost' = mealyB clk rst ena go I2CIdle
  (ramOp', hostAck', arbLost')
 where
  deconstructRamOp RamNoOp = (0, 0)
  deconstructRamOp (RamRead a) = (pack (a, True), 0)
  deconstructRamOp (RamWrite a b) = (pack (a, False), b)

  go state (ramOp, hostAck, arbLost) = (nextState, coreInput)
   where
    coreInput = (claimBus, i2cOp, cmdAck)
    cmdAck = state /= I2CIdle && nextState == I2CIdle
    (ramOpAddr, ramOpData) = deconstructRamOp ramOp
    (claimBus, i2cOp) = case state of
      I2CIdle         -> (False, Nothing)
      I2CWriteAddress -> (True, Just $ WriteData ramOpAddr)
      I2CWriteData    -> (True, Just $ WriteData ramOpData)
      I2CReadData     -> (True, Just $ ReadData)

    nextState = case (state, ramOp, hostAck, arbLost) of
      (I2CIdle, RamNoOp, _, _)                     -> I2CIdle
      (I2CIdle, _, _, _)                           -> I2CWriteAddress
      (I2CWriteAddress, RamRead _, True, False)    -> I2CReadData
      (I2CWriteAddress, RamWrite _ _, True, False) -> I2CWriteData
      (_, _, False, False)                      -> state
      _                                         -> I2CIdle

orNothing :: Bool -> a -> Maybe a
orNothing True a = Just a
orNothing False _ = Nothing

i2cTest ::
  "sys_125" ::: DiffClock Basic125 ->
  "sdaIn" ::: Signal Basic125 Bit ->
  ( "sclOut" ::: Signal Basic125 Bit
  , "sdaOut" ::: Signal Basic125 Bit
  , "mux_select" ::: Signal Basic125 (BitVector 3))
i2cTest diffClk sdaIn =
  ( sclOut
  , sdaOut
  , mux_select
  )
 where
  clk = ibufds diffClk
  sdaOut = fmap (bitCoerce . isNothing) sdaMaybe
  sclOut = fmap (bitCoerce . isNothing) sclMaybe
  (sclMaybe, sdaMaybe) = unbundle i2cO
  i2cIn = bundle (fmap bitCoerce sclOut, fromMaybe <$> sdaIn <*> sdaMaybe)

  (dout,hostAck,busy,al,ackOut,i2cO) = i2c clk vioAsyncReset vioStatemachineReset vioEnaCore clkCnt claimBus i2cOp ackIn i2cIn
  (claimBus, i2cOp, cmdAck) = i2cController clk vioAsyncReset vioEnaController ramOp hostAck al

  ramOp = regEn clk vioAsyncReset enableGen RamNoOp (startOpEdge .||. cmdAck .||. al) $ mux startOpEdge vioRamOp (pure RamNoOp)
  vioRamOp = mux vioIsWriteOp (RamWrite <$> vioAddr <*> vioI2CWriteData) (RamRead <$> vioAddr)
  startOpEdge = isRising clk vioAsyncReset enableGen False vioStartOp

  capturedData = regEn clk vioAsyncReset enableGen 0 hostAck dout
  ( unsafeFromActiveHigh -> vioAsyncReset
   , vioStatemachineReset
   , vioEnaCore
   , toEnable -> vioEnaController
   , clkCnt
   , ackIn
   , vioStartOp
   , vioIsWriteOp
   , vioAddr
   , vioI2CWriteData
   , mux_select
   ) = unbundle $ setName @"i2cVio" $
   vioProbe
   ( "capturedData"
    :> "busy"
    :> "al"
    :> "ackOut"
    :> Nil)

   ( "vioAsyncReset"
    :> "vioStatemachineReset"
    :> "vioEnaCore"
    :> "vioEnaController"
    :> "clkCnt"
    :> "ackIn"
    :> "vioStartOp"
    :> "vioIsWriteOp"
    :> "vioAddr"
    :> "vioI2CWriteData"
    :> "mux_select"
    :> Nil)

   (True, True, False, False, 1 :: Unsigned 16, False, False, False, 0 :: Index 128, 0 :: BitVector 8, 0b100) clk
   capturedData
   busy
   al
   ackOut

makeTopEntity 'i2cTest
