-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# OPTIONS_GHC -Wno-orphans #-}

module Tests.ElasticBuffer where

import Clash.Prelude

import Test.Tasty
import Test.Tasty.HUnit

import Bittide.ClockControl (RelDataCount, targetDataCount)
import Bittide.ElasticBuffer
import Clash.Cores.Xilinx (withXilinx)
import Clash.Cores.Xilinx.ElasticBuffer (XilinxEBConstraints, xilinxElasticBuffer)
import Protocols

import qualified Data.List as L

createDomain vXilinxSystem{vPeriod = hzToPeriod 200e6, vName = "Fast"}
createDomain vXilinxSystem{vPeriod = hzToPeriod 199e6, vName = "Slow"}

tests :: TestTree
tests =
  testGroup
    "Tests.ElasticBuffer"
    [ testGroup
        "xilinxElasticBuffer"
        [ testCase "case_xilinxElasticBufferMaxBound" case_xilinxElasticBufferMaxBound
        , testCase "case_xilinxElasticBufferMinBound" case_xilinxElasticBufferMinBound
        , testCase "case_xilinxElasticBufferEq" case_xilinxElasticBufferEq
        ]
    ]

xilinxElasticBufferDut ::
  forall n readDom writeDom.
  ( HasCallStack
  , HasSynchronousReset readDom
  , KnownDomain readDom
  , KnownDomain writeDom
  , KnownNat n
  , n <= 32
  , XilinxEBConstraints n
  ) =>
  Clock readDom ->
  Clock writeDom ->
  Signal readDom (Maybe EbAdjustment) ->
  Signal writeDom (Unsigned 8) ->
  ( Signal readDom (RelDataCount n)
  , Signal readDom Underflow
  , Signal writeDom Overflow
  , Signal readDom (ElasticBufferData (Unsigned 8))
  , Signal readDom Ack
  )
xilinxElasticBufferDut clkRead clkWrite adjust dataIn =
  ( -- Note that this is chosen to work for 'RelDataCount' either being
    -- set to 'Signed' with 'targetDataCount' equals 0 or set to
    -- 'Unsigned' with 'targetDataCount' equals 'shiftR maxBound 1 + 1'.
    -- This way, the representation can be easily switched without
    -- introducing major code changes.
    (+ targetDataCount)
      . bitCoerce
      . (+ (-1 - shiftR maxBound 1))
      <$> dcFifoOutput.dataCount
  , dcFifoOutput.underflow
  , dcFifoOutput.overflow
  , dcFifoOutput.fifoOut
  , ebAdjustAck
  )
 where
  Circuit fn = xilinxElasticBuffer @n clkRead clkWrite

  noReset :: forall dom. (KnownDomain dom) => Reset dom
  noReset = unsafeFromActiveHigh (pure False)

  (writeEnable, readEnable, ebAdjustAck) =
    withXilinx $ elasticBufferAutoCenter clkRead noReset clkWrite noReset adjust

  writeData = mux writeEnable (pure Nothing) (Just <$> dataIn)

  dcFifoInput = DcFifoInput{writeData = writeData, readEnable = readEnable}

  (dcFifoOutput, _) = fn (dcFifoInput, ())

{- | When the xilinxElasticBuffer is written to more quickly than it is being read from,
its data count should overflow.
-}
case_xilinxElasticBufferMaxBound :: Assertion
case_xilinxElasticBufferMaxBound = do
  let
    command = fromList $ L.replicate 60 (Just 1) <> L.repeat Nothing
    wData = pure (0 :: Unsigned 8)
    underflows =
      sampleN
        2048
        ( (\(_, under, _, _, _) -> under)
            (xilinxElasticBufferDut @6 (clockGen @Slow) (clockGen @Fast) command wData)
        )
    overflows =
      sampleN
        16192
        ( (\(_, _, over, _, _) -> over)
            (xilinxElasticBufferDut @6 (clockGen @Slow) (clockGen @Fast) command wData)
        )

    -- Ignore the first 32 samples to allow the buffer to fill up
    underflowsTail = L.drop 32 underflows
    overflowsTail = L.drop 32 overflows

  assertBool
    ("elastic buffer should not underflow: " <> show underflowsTail)
    (not $ or underflowsTail)
  assertBool ("elastic buffer should overflow: " <> show overflowsTail) (or overflowsTail)

{- | When the xilinxElasticBuffer is read from more quickly than it is being written to,
its data count should underflow.
-}
case_xilinxElasticBufferMinBound :: Assertion
case_xilinxElasticBufferMinBound = do
  let
    command = fromList $ L.replicate 8 (Just 1) <> L.repeat Nothing
    wData = pure (0 :: Unsigned 8)
    underflows =
      sampleN
        2048
        ( (\(_, under, _, _, _) -> under)
            (xilinxElasticBufferDut @6 (clockGen @Fast) (clockGen @Slow) command wData)
        )
    overflows =
      sampleN
        2048
        ( (\(_, _, over, _, _) -> over)
            (xilinxElasticBufferDut @6 (clockGen @Fast) (clockGen @Slow) command wData)
        )

    -- Ignore the first 32 samples to allow the buffer to fill up
    underflowsTail = L.drop 32 underflows
    overflowsTail = L.drop 32 overflows

  assertBool ("elastic buffer should underflow: " <> show underflowsTail) (or underflowsTail)
  assertBool ("elastic buffer should not overflow: " <> show overflowsTail) (not $ or overflowsTail)

{- | When the xilinxElasticBuffer is written to as quickly to as it is read from, it should
neither overflow nor underflow.
-}
case_xilinxElasticBufferEq :: Assertion
case_xilinxElasticBufferEq = do
  let
    command = fromList $ L.replicate 16 (Just 1) <> L.repeat Nothing
    wData = pure (0 :: Unsigned 8)
    underflows =
      sampleN
        256
        ( (\(_, under, _, _, _) -> under)
            (xilinxElasticBufferDut @5 (clockGen @Slow) (clockGen @Slow) command wData)
        )
    overflows =
      sampleN
        256
        ( (\(_, _, over, _, _) -> over)
            (xilinxElasticBufferDut @5 (clockGen @Slow) (clockGen @Slow) command wData)
        )

    -- Ignore the first 32 samples to allow the buffer to fill up
    underflowsTail = L.drop 32 underflows
    overflowsTail = L.drop 32 overflows

  assertBool
    ("elastic buffer should not underflow: " <> show underflowsTail)
    (not $ or underflowsTail)
  assertBool ("elastic buffer should not overflow: " <> show overflowsTail) (not $ or overflowsTail)
