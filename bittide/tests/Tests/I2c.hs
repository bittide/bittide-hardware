-- SPDX-FileCopyrightText: 2026 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Tests.I2c where

import Clash.Prelude

import Bittide.I2c
import Bittide.SharedTypes (Byte)
import Test.Tasty
import Test.Tasty.HUnit
import Tests.I2c.Target (Mode (..), TargetState (..), pagedRegisterTarget)

import qualified Data.List as L

tests :: TestTree
tests =
  testGroup
    "Tests.I2c"
    [ testCase "Write and read back a register" writeReadBack
    , testCase "Unknown targets do not acknowledge" unknownTarget
    , testCase "Commands without effect are ignored" ignoredCommand
    , testCase "START is only generated before a byte transfer" startWithoutTransfer
    ]

-- | State of the host in 'runCommands'
data Host
  = -- | Issue the next command
    Issue
  | -- | Wait a cycle for the controller to register the command
    Accepted
  | -- | Wait until the controller is no longer busy
    Poll
  deriving (Show, Generic, NFDataX)

{- | Issue commands to 'i2cController' connected to 'pagedRegisterTarget', one after
the other. Returns the read data and received acknowledge bit after every
command, and the final state of the target. Returns 'Nothing' if that takes too
long.
-}
runCommands :: [I2cCommand] -> Maybe ([(Byte, I2cAck)], TargetState)
runCommands cmds =
  case L.span (not . fst) samples of
    (_, (_, (results, finalTarget)) : _) -> Just (results, finalTarget)
    _ -> Nothing
 where
  samples = sampleN 200_000 (bundle (done, bundle (resultsSoFar, target)))

  (status, i2cOut) =
    withClockResetEnable clockGen resetGen enableGen
      $ i2cController (SNat @200_000) newCmd bus

  target =
    withClockResetEnable clockGen resetGen enableGen
      $ pagedRegisterTarget @System chipAddress 0 bus

  -- Open-drain bus: the target does not drive SCL
  bus = toBus <$> i2cOut <*> target
  toBus (sclO, sclOen, sdaO, sdaOen) t =
    (if sclOen then high else sclO, (if sdaOen then high else sdaO) .&. t.sdaOut)

  (newCmd, done, resultsSoFar) =
    unbundle
      $ withClockResetEnable clockGen resetGen enableGen
      $ mealy host (Issue, cmds, [])
      $ status

  host (Issue, c : cs, rs) _ = ((Accepted, c : cs, rs), (Just c, False, rs))
  host (Issue, [], rs) _ = ((Issue, [], rs), (Nothing, True, rs))
  host (Accepted, cs, rs) _ = ((Poll, cs, rs), (Nothing, False, rs))
  host (Poll, cs, rs) s
    | s.busy = ((Poll, cs, rs), (Nothing, False, rs))
    | otherwise = ((Issue, L.drop 1 cs, rs'), (Nothing, False, rs'))
   where
    rs' = rs <> [(s.readData, s.ackReceived)]

chipAddress :: BitVector 7
chipAddress = 0x69

addressByte :: BitVector 7 -> Bool -> Byte
addressByte addr isRead = addr ++# boolToBV isRead

writeReadBack :: Assertion
writeReadBack = do
  let
    cmds =
      [ I2cCommand True (WriteByte (addressByte chipAddress False)) False
      , I2cCommand False (WriteByte 0x30) False
      , I2cCommand False (WriteByte 0xAB) True
      , I2cCommand True (WriteByte (addressByte chipAddress False)) False
      , I2cCommand False (WriteByte 0x30) False
      , I2cCommand True (WriteByte (addressByte chipAddress True)) False
      , I2cCommand False (ReadByte NotAcknowledge) True
      ]
  case runCommands cmds of
    Nothing -> assertFailure "Simulation did not finish"
    Just (results, target) -> do
      snd <$> L.take 6 results @?= L.replicate 6 Acknowledge
      fst <$> L.drop 6 results @?= [0xAB]
      ((target.regs !! (0 :: Int)) !! (0x30 :: Int)) @?= 0xAB

unknownTarget :: Assertion
unknownTarget = do
  let
    cmds =
      [ I2cCommand True (WriteByte (addressByte 0x50 False)) False
      , I2cCommand False NoTransfer True
      , I2cCommand True (WriteByte (addressByte chipAddress False)) False
      , I2cCommand False NoTransfer True
      ]
  case runCommands cmds of
    Nothing -> assertFailure "Simulation did not finish"
    Just (results, _target) -> snd <$> results @?= [NotAcknowledge, NotAcknowledge, Acknowledge, Acknowledge]

ignoredCommand :: Assertion
ignoredCommand = do
  let
    cmds =
      [ I2cCommand True NoTransfer False
      , I2cCommand True (WriteByte (addressByte chipAddress False)) True
      ]
  case runCommands cmds of
    Nothing -> assertFailure "Simulation did not finish"
    Just (results, _target) -> snd <$> results @?= [Acknowledge, Acknowledge]

{- | Without a byte transfer, the I2C core would write a byte after a START
condition. The controller must not pass START on to the core in that case.
-}
startWithoutTransfer :: Assertion
startWithoutTransfer = do
  let
    cmds =
      [ I2cCommand True NoTransfer True
      , I2cCommand True (WriteByte (addressByte chipAddress False)) True
      ]
  case runCommands cmds of
    Nothing -> assertFailure "Simulation did not finish"
    Just (_results, target) -> do
      target.starts @?= 1
      target.mode @?= Ignore
