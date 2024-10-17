-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use head" #-}

module Bittide.Instances.Hitl.DnaOverSerial where

import Bittide.Hitl
import Bittide.Instances.Domains
import Bittide.Instances.Hitl.Setup
import Clash.Annotations.TH (makeTopEntity)
import Clash.Cores.UART
import Clash.Cores.Xilinx.Extra (ibufds)
import Clash.Cores.Xilinx.Unisim.DnaPortE2
import Clash.Explicit.Reset
import Clash.Functor.Extra
import Clash.Prelude
import Data.Char
import qualified Data.List as L
import Data.Maybe

dnaOverSerial ::
  "CLK_125MHZ" ::: DiffClock Ext125 ->
  "USB_UART_TXD" ::: Signal Ext125 Bit ->
  "USB_UART_RXD" ::: Signal Ext125 Bit
dnaOverSerial diffClk serialIn = serialOut
 where
  clk = ibufds diffClk
  (_rxData, serialOut, ack) = withClockResetEnable clk testRst enableGen $ uart baud serialIn txData
  baud = SNat @9600

  dna = readDnaPortE2 clk testRst enableGen simDna2
  txData = withClockResetEnable clk testRst enableGen shiftRegister (dnaToAscii <<$>> dna) ack
  testStart = hitlVioBool clk testDone testSuccess
  testDone = testStart
  testSuccess = testStart
  testRst = unsafeFromActiveLow testStart

-- | Convert the DNA to ASCII and prepend a newline character.
dnaToAscii :: BitVector 96 -> Vec 25 (BitVector 8)
dnaToAscii x = resize (bitCoerce $ ord '\n') :> fmap bv4ToHexChar (unpack x)

-- | Convert a 4-bit bitvector to a hex character.
bv4ToHexChar :: BitVector 4 -> BitVector 8
bv4ToHexChar x = resize x + offset
 where
  offset
    | x < 10 = resize $ bitCoerce (ord '0')
    | otherwise = resize (bitCoerce $ ord 'a') - 10

simShiftRegister :: [Maybe (BitVector 8)]
simShiftRegister = sampleN 50 dut
 where
  dut =
    withClockResetEnable clockGen resetGen enableGen $ shiftRegister dataIn ack
  dataIn :: Signal System (Maybe (BitVector 64))
  dataIn = fromList $ L.replicate 5 Nothing L.++ [Just 1243786952] L.++ L.repeat Nothing
  ack = fromList $ cycle [True, False, False, True, True]

shiftRegister ::
  forall dom n m a.
  (HiddenClockResetEnable dom, KnownNat n, KnownNat m, 1 <= n, BitPack a, BitSize a ~ n * m) =>
  Signal dom (Maybe a) ->
  Signal dom Bool ->
  Signal dom (Maybe (BitVector m))
shiftRegister dataInS ackS =
  mealy go (repeat 0 :: Vec n (BitVector m), 0 :: Index (n + 1)) $ bundle (dataInS, ackS)
 where
  go ::
    (Vec n (BitVector m), Index (n + 1)) ->
    (Maybe a, Bool) ->
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

makeTopEntity 'dnaOverSerial

tests :: HitlTestGroup
tests =
  HitlTestGroup
    { topEntity = 'dnaOverSerial
    , extraXdcFiles = []
    , externalHdl = []
    , testCases =
        [ HitlTestCase
            { name = "DnaOverSerial"
            , parameters = paramForHwTargets allHwTargets ()
            , postProcData = ()
            }
        ]
    , mPostProc = Just "post-dna-over-serial"
    }
