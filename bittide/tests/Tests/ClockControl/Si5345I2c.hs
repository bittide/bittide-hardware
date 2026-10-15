-- SPDX-FileCopyrightText: 2026 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Tests.ClockControl.Si5345I2c where

import Clash.Prelude

import Bittide.ClockControl.Si5345I2c
import Bittide.ClockControl.Si539xSpi (Address, Page, RegisterOperation (..))
import Bittide.SharedTypes (Byte)
import Test.Tasty
import Test.Tasty.HUnit
import Tests.I2c.Target (TargetState (..), pagedRegisterTarget)

import qualified Data.List as L

tests :: TestTree
tests =
  testGroup
    "Tests.ClockControl.Si5345I2c"
    [ testCase "Write and read back registers" writeReadBack
    , testCase "Page register is only written when the page changes" pageCaching
    , testCase "Operations are retried when not acknowledged" retryOnNack
    ]

chipAddress :: BitVector 7
chipAddress = 0x69

{- | Issue register operations to 'si5345I2cDriver' connected to 'pagedRegisterTarget',
one after the other. Returns the results of all operations and the final state
of the target, or 'Nothing' if that takes too long.
-}
runOps :: Int -> [RegisterOperation] -> Maybe ([Byte], TargetState)
runOps nacks ops =
  case L.span (not . fst) samples of
    (_, (_, (results, finalTarget)) : _) -> Just (results, finalTarget)
    _ -> Nothing
 where
  samples = sampleN 200_000 (bundle (done, bundle (resultsSoFar, target)))

  (result, _busy, i2cOut, _debug) =
    withClockResetEnable clockGen resetGen enableGen
      $ si5345I2cDriver (SNat @200_000) chipAddress currentOp bus

  target =
    withClockResetEnable clockGen resetGen enableGen
      $ pagedRegisterTarget @System chipAddress nacks bus

  -- Open-drain bus: the target does not drive SCL
  bus = toBus <$> i2cOut <*> target
  toBus (sclO, sclOen, sdaO, sdaOen) t =
    (if sclOen then high else sclO, (if sdaOen then high else sdaO) .&. t.sdaOut)

  -- Host: hold the current operation until the driver returns a result
  (currentOp, done, resultsSoFar) =
    unbundle
      $ withClockResetEnable clockGen resetGen enableGen
      $ moore hostT hostOut (ops, [])
      $ result

  hostT (_ : os, rs) (Just r) = (os, rs <> [r])
  hostT s _ = s
  hostOut (o : _, rs) = (Just o, False, rs)
  hostOut ([], rs) = (Nothing, True, rs)

writeOp :: Page -> Address -> Byte -> RegisterOperation
writeOp page address byte = RegisterOperation{page, address, write = Just byte}

readOp :: Page -> Address -> RegisterOperation
readOp page address = RegisterOperation{page, address, write = Nothing}

writeReadBack :: Assertion
writeReadBack = do
  let
    ops =
      [ writeOp 0x00 0x1D 0x01
      , writeOp 0x02 0x6B 0x46
      , writeOp 0x03 0x10 0xA5
      , readOp 0x00 0x1D
      , readOp 0x02 0x6B
      , readOp 0x03 0x10
      ]
  case runOps 0 ops of
    Nothing -> assertFailure "Simulation did not finish"
    Just (results, target) -> do
      results @?= [0x01, 0x46, 0xA5, 0x01, 0x46, 0xA5]
      ((target.regs !! (2 :: Int)) !! (0x6B :: Int)) @?= 0x46

pageCaching :: Assertion
pageCaching = do
  let
    ops =
      [ writeOp 0x00 0x1D 0x01
      , writeOp 0x00 0x1D 0x02
      , readOp 0x00 0x1D
      , writeOp 0x02 0x6B 0x46
      , readOp 0x02 0x6B
      , writeOp 0x00 0x1D 0x01
      ]
  case runOps 0 ops of
    Nothing -> assertFailure "Simulation did not finish"
    Just (results, target) -> do
      results @?= [0x01, 0x02, 0x02, 0x46, 0x46, 0x01]
      -- Page 0, page 2, back to page 0
      target.pageWrites @?= 3

retryOnNack :: Assertion
retryOnNack = do
  let ops = [writeOp 0x01 0x20 0x5A, readOp 0x01 0x20]
  case runOps 3 ops of
    Nothing -> assertFailure "Simulation did not finish"
    Just (results, target) -> do
      results @?= [0x5A, 0x5A]
      target.nacks @?= 0
