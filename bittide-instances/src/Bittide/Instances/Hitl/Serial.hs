-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use head" #-}

module Bittide.Instances.Hitl.Serial where

import Bittide.Instances.Domains
import Clash.Annotations.TH (makeTopEntity)
import Clash.Cores.UART
import Clash.Cores.Xilinx.Extra (ibufds)
import Clash.Cores.Xilinx.Unisim.DnaPortE2
import Clash.Explicit.Reset
import Clash.Prelude
import qualified Data.List as L
import Data.Maybe

sendSerial ::
  "CLK_125MHZ" ::: DiffClock Ext125 ->
  "USB_UART_TXD" ::: Signal Ext125 Bit ->
  "USB_UART_RXD" ::: Signal Ext125 Bit
sendSerial diffClk serialIn = serialOut
 where
  clk = ibufds diffClk
  (_rxData, serialOut, ack) = withClockResetEnable clk noReset enableGen $ uart baud serialIn txData
  baud = SNat @9600
  dna = readDnaPortE2 clk noReset enableGen simDna2
  txData = withClockResetEnable clk noReset enableGen shiftRegister dna ack

simShiftRegister :: [Maybe (BitVector 8)]
simShiftRegister = sampleN 50 dut
 where
  dut =
    withClockResetEnable clockGen resetGen enableGen $ shiftRegister @System @6 @8 dataIn ack
  dataIn = fromList $ L.replicate 5 Nothing L.++ [Just 1243786952] L.++ L.repeat Nothing
  ack = fromList $ cycle [True, False, False, True, True]

shiftRegister ::
  forall dom n m.
  (HiddenClockResetEnable dom, KnownNat n, KnownNat m, 1 <= n) =>
  Signal dom (Maybe (BitVector (n * m))) ->
  Signal dom Bool ->
  Signal dom (Maybe (BitVector m))
shiftRegister dataInS ackS =
  mealy go (repeat 0 :: Vec n (BitVector m), 0 :: Index (n + 1)) $ bundle (dataInS, ackS)
 where
  go ::
    (Vec n (BitVector m), Index (n + 1)) ->
    (Maybe (BitVector (n * m)), Bool) ->
    ((Vec n (BitVector m), Index (n + 1)), Maybe (BitVector m))
  go (vecOld, nElementsOld) (dataIn, ack) = ((vecNew, nElementsNew), dataOut)
   where
    (vecNew, nElementsNew)
      | nElementsOld == 0 && isJust dataIn = (bitCoerce $ fromJust dataIn, maxBound)
      | ack = (vecOld <<+ 0, satPred SatZero nElementsOld)
      | otherwise = (vecOld, nElementsOld)

    dataOut
      | nElementsOld == 0 = Nothing
      | otherwise = Just $ vecOld !! (0 :: Int)

-- dfLinePackets ::
--   Circuit
--   (Df dom (BitVector 8)) (PacketStream dom 1 (BitVector 8))
-- dfLinePackets = Circuit (unbundle . fmap go . bundle)
--  where
--   go (dataToMaybe -> dfM2S, psS2M) = (Ack $ (._ready) psS2M, fmap toPs psM2S)
--    where
--     toPs b = PacketStreamM2S {_data = repeat 0, _last = orNothing (b == ord '\n') 0, _meta = (), _abort = false}

makeTopEntity 'sendSerial
