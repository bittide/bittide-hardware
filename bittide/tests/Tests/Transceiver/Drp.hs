-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{- | Tests for the GTH DRP (Dynamic Reconfiguration Port) master,
'Bittide.Transceiver.Drp.drpMaster'.

The real GTH core is a black box in simulation, so these tests exercise the
master against a small /mock DRP slave/ that models a single 16-bit register per
channel with a fixed @drprdy@ latency. This verifies the read/write protocol, the
read-modify-write building blocks, channel multiplexing, and the watchdog
timeout.
-}
module Tests.Transceiver.Drp where

import Clash.Prelude

import Data.Maybe (listToMaybe)

import qualified Bittide.Transceiver.Drp as Drp
import qualified Clash.Explicit.Prelude as E
import qualified Data.List as L

import Clash.Hedgehog.Sized.Unsigned (genUnsigned)
import Hedgehog (Property)
import Test.Tasty
import Test.Tasty.Hedgehog (testProperty)
import Test.Tasty.TH (testGroupGenerator)

import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

-- | Number of channels used in the tests (>= 1; 2 exercises channel muxing).
type N = 2

-- | @drprdy@ latency of the mock slave, in cycles after the @drpen@ pulse.
slaveLatency :: Index 8
slaveLatency = 3

{- | A mock DRP slave modelling a single 16-bit register (the address is
ignored). On a @drpen@ pulse it waits 'slaveLatency' cycles, then asserts
@drprdy@ for one cycle, presenting the (pre-write) register value on @drpdo@ and,
for writes, latching @drpdi@ into the register.
-}
mockSlave ::
  forall dom.
  (KnownDomain dom) =>
  Clock dom ->
  Reset dom ->
  Enable dom ->
  -- | Initial register value
  BitVector 16 ->
  -- | @drpaddr@ (ignored)
  Signal dom (BitVector 9) ->
  -- | @drpdi@
  Signal dom (BitVector 16) ->
  -- | @drpen@
  Signal dom (BitVector 1) ->
  -- | @drpwe@
  Signal dom (BitVector 1) ->
  -- | @(drpdo, drprdy)@
  (Signal dom (BitVector 16), Signal dom (BitVector 1))
mockSlave clk rst ena initReg _addr di en we =
  unbundle (E.mealy clk rst ena go (initReg, 0, False, 0) (bundle (di, en, we)))
 where
  go ::
    (BitVector 16, Index 8, Bool, BitVector 16) ->
    (BitVector 16, BitVector 1, BitVector 1) ->
    ((BitVector 16, Index 8, Bool, BitVector 16), (BitVector 16, BitVector 1))
  go (reg, cnt, weL, diL) (diIn, enIn, weIn)
    -- Idle: start a transaction on an enable pulse, latching write data.
    | cnt == 0 =
        if enIn == 1
          then ((reg, slaveLatency, weIn == 1, diIn), (0, 0))
          else ((reg, 0, False, 0), (0, 0))
    -- Final cycle: assert drprdy, present the read value, apply any write.
    | cnt == 1 =
        let reg' = if weL then diL else reg
         in ((reg', 0, False, 0), (reg, 1))
    -- Counting down towards drprdy.
    | otherwise = ((reg, cnt - 1, weL, diL), (0, 0))

-- | Run the master against live mock slaves and sample @(busy, response)@.
runDrp ::
  -- | Initial per-channel register values
  Vec N (BitVector 16) ->
  -- | Number of cycles to sample
  Int ->
  -- | Request stream (padded with 'Nothing')
  [Maybe Drp.DrpRequest] ->
  [(Bool, Maybe Drp.Response)]
runDrp initVals nCycles reqs =
  sampleN nCycles (bundle (drpBusy, drpResp))
 where
  clk = clockGen @System
  rst = resetGen @System
  ena = enableGen @System

  reqSig = fromList (reqs L.++ L.repeat Nothing)

  (addrs, dis, ens, wes, drpResp, drpBusy) =
    Drp.drpMaster clk rst ena reqSig drpDos drpRdys

  (drpDos, drpRdys) = unzip slaveOuts
  slaveOuts =
    zipWith
      (\iv (a, d, e, w) -> mockSlave clk rst ena iv a d e w)
      initVals
      (zip4 addrs dis ens wes)

-- | Run the master with slaves that never respond and sample @(busy, response)@.
runDrpDead :: Int -> [Maybe Drp.DrpRequest] -> [(Bool, Maybe Drp.Response)]
runDrpDead nCycles reqs =
  sampleN nCycles (bundle (drpBusy, drpResp))
 where
  clk = clockGen @System
  rst = resetGen @System
  ena = enableGen @System
  reqSig = fromList (reqs L.++ L.repeat Nothing)
  (_, _, _, _, drpResp, drpBusy) =
    Drp.drpMaster @N clk rst ena reqSig (repeat (pure 0)) (repeat (pure 0))

-- | The first completed response in a sample list, if any.
firstResponse :: [(Bool, Maybe Drp.Response)] -> Maybe Drp.Response
firstResponse samples = listToMaybe [r | (_, Just r) <- samples]

mkRead :: Unsigned 8 -> Drp.DrpRequest
mkRead ch = Drp.DrpRequest{channel = ch, address = 0, writeData = 0, isWrite = False}

mkWrite :: Unsigned 8 -> BitVector 16 -> Drp.DrpRequest
mkWrite ch v = Drp.DrpRequest{channel = ch, address = 0, writeData = v, isWrite = True}

genValue :: H.Gen (BitVector 16)
genValue = pack <$> genUnsigned @16 Range.linearBounded

genChannel :: H.Gen (Unsigned 8)
genChannel = Gen.element [0, 1]

-- | A read returns the addressed channel's register value.
prop_drpRead :: Property
prop_drpRead = H.property $ do
  v <- H.forAll genValue
  ch <- H.forAll genChannel
  let
    initVals = replace (fromIntegral ch :: Index N) v (repeat 0)
    samples = runDrp initVals 64 [Nothing, Nothing, Just (mkRead ch)]
  firstResponse samples H.=== Just (Drp.Response{readData = v, timedOut = False})

-- | Writing a register and then reading it back returns the written value.
prop_drpWriteThenRead :: Property
prop_drpWriteThenRead = H.property $ do
  v <- H.forAll genValue
  ch <- H.forAll genChannel
  let
    reqs =
      [Nothing, Nothing, Just (mkWrite ch v)]
        L.++ L.replicate 25 Nothing
        L.++ [Just (mkRead ch)]
    samples = runDrp (repeat 0) 96 reqs
    responses = [r | (_, Just r) <- samples]
  -- Exactly two transactions complete: the write, then the read.
  L.length responses H.=== 2
  -- The write reports the pre-write value (0); the read returns the new value.
  responses
    H.=== [Drp.Response{readData = 0, timedOut = False}, Drp.Response{readData = v, timedOut = False}]

-- | A channel that never asserts @drprdy@ results in a timeout response.
prop_drpTimeout :: Property
prop_drpTimeout = H.withTests 1 $ H.property $ do
  -- 'Drp.DrpTimeout' is 1024; sample past it so the watchdog fires.
  let samples = runDrpDead 1100 [Nothing, Nothing, Just (mkRead 0)]
  firstResponse samples H.=== Just (Drp.Response{readData = 0, timedOut = True})

tests :: TestTree
tests = $(testGroupGenerator)
