-- SPDX-FileCopyrightText: 2023 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=20 #-}
{-# OPTIONS_GHC -fplugin=Protocols.Plugin #-}

module Tests.ClockControlWb where

import Clash.Explicit.Prelude hiding (PeriodToCycles, many)

import Clash.Signal (withClockResetEnable)
import Data.Char (chr)
import Data.Either (isRight)
import qualified Data.List as L
import Data.Maybe (mapMaybe)
import Language.Haskell.TH
import Project.FilePath
import System.FilePath
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.TH
import Text.Parsec
import Text.Read (readEither)

import Bittide.Arithmetic.Time (PeriodToCycles)
import Bittide.ClockControl (SpeedChange)
import Bittide.ClockControl.DebugRegister (DebugRegisterCfg (..), debugRegisterWb)
import Bittide.ClockControl.Registers (ClockControlData, clockControlWb, clockMod)
import Bittide.DoubleBufferedRam
import Bittide.Instances.Hitl.HwCcTopologies (cSigMap, csDupe)
import Bittide.Instances.Hitl.Setup (LinkCount)
import Bittide.ProcessingElement
import Bittide.ProcessingElement.Util
import Bittide.SharedTypes
import Bittide.Wishbone

import Protocols
import qualified Protocols.Df as Df
import Protocols.Idle

case_clock_control_wb_self_test :: Assertion
case_clock_control_wb_self_test = assertBool msg assertion
 where
  msg = case readCcdResult of
    Left m -> "failure:\n" <> m
    Right m -> "pass:\n" <> m
  assertion = isRight readCcdResult
  -- readUartResult = resultFromUartOutput uartString
  readUartResult :: Either ParseError [Int]
  readUartResult = parse outputFieldsParser "" uartString
  readCcdResult :: Either String String
  readCcdResult = case readUartResult of
    Left err -> Left $ show err
    Right val -> resultFromCcdOutput val ccData

  uartString :: String
  uartString = chr . fromIntegral <$> mapMaybe Df.dataToMaybe uartStream
  (uartStream, ccData) = sampleC def dut

tests :: TestTree
tests = $(testGroupGenerator)

type Margin = SNat 2
margin :: Margin
margin = SNat

type Framesize = PeriodToCycles System (Seconds 1)
framesize :: SNat Framesize
framesize = SNat

linkCount :: Int
linkCount = snatToNum (SNat @LinkCount)
linkMask :: BitVector LinkCount
linkMask = 0b1011011
linkMaskInt :: Int
linkMaskInt = fromIntegral linkMask
linkMaskPopcnt :: Int
linkMaskPopcnt = fromIntegral $ popCount linkMask

dataCounts :: Vec LinkCount (Signed 27)
dataCounts = iterateI (satSucc SatWrap) 0

debugRegisterConfig :: DebugRegisterCfg
debugRegisterConfig =
  DebugRegisterCfg
    { reframingEnabled = False
    }

dut ::
  Circuit () (Df System (BitVector 8), CSignal System (ClockControlData LinkCount))
dut =
  withClockResetEnable
    clockGen
    resetGen
    enableGen
    $ circuit
    $ \_unit -> do
      (uartRx, jtag) <- idleSource -< ()
      [uartBus, ccWb, dbgWb] <- processingElement peConfig -< jtag
      (uartTx, _uartStatus) <- uartInterfaceWb d2 d2 uartSim -< (uartBus, uartRx)
      [ccd0, ccd1] <-
        csDupe
          <| clockControlWb
            margin
            framesize
            (pure linkMask)
            (pure <$> dataCounts)
          -< ccWb
      cm <- cSigMap clockMod -< ccd0
      _dbg <- debugRegisterWb (pure debugRegisterConfig) -< (dbgWb, cm)
      idC -< (uartTx, ccd1)
 where
  (iMem, dMem) =
    $( do
        root <- runIO $ findParentContaining "cabal.project"
        let
          elfDir = root </> firmwareBinariesDir "riscv32imc-unknown-none-elf" Release
          elfPath = elfDir </> "clock-control-wb"
          iSize = 8 * 1024 -- 16 KB
          dSize = 64 * 1024 -- 256 KB
        memBlobsFromElf BigEndian (Just iSize, Just dSize) elfPath Nothing
     )

  peConfig =
    PeConfig
      (0b100 :> 0b010 :> 0b001 :> 0b110 :> 0b111 :> Nil)
      (Reloadable $ Blob iMem)
      (Reloadable $ Blob dMem)

type UartSample = Maybe (BitVector 8)

resultFromCcdOutput :: [Int] -> [ClockControlData LinkCount] -> Either String String
resultFromCcdOutput speedChanges ccdSamples = output2
 where
  numSCs = L.length speedChanges

  scList = mapMaybe (.clockMod) ccdSamples
  scIntList = L.map getSCInt scList

  output1 = scCheck speedChanges scIntList
  output2 = output1 >>= finalCheck

  scCheck :: [Int] -> [Int] -> Either String [Int]
  scCheck l1 l2 = case (l1, l2) of
    (_, []) -> undefined
    ([], rest) -> Right rest
    (h1 : t1, h2 : t2) -> next
     where
      next =
        if h1 == h2
          then scCheck t1 t2
          else
            Left
              $ "UART sample says `"
              <> show h1
              <> "`, but CcWb says `"
              <> show h2
              <> "`"

  finalCheck :: [Int] -> Either String String
  finalCheck = finalCheck' linkMaskPopcnt
   where
    finalCheck' 0 _ =
      Right $ "clockMod: successfully read " <> show numSCs <> "speed changes"
    finalCheck' n [] =
      Left
        $ "reached end of stream while trying to read clock change padding. remaining: "
        <> show (n - 1)
    finalCheck' n (h : t) =
      if h == 0
        then finalCheck' (n - 1) t
        else
          Left
            $ "failed to read clock change padding. expected `0`, found `"
            <> show h
            <> "`"

getSCInt :: SpeedChange -> Int
getSCInt = fromIntegral . pack

outputFieldsParser :: Parsec String st [Int]
outputFieldsParser = do
  _ <- outputFieldParserEq "nLinks" linkCount
  _ <- outputFieldParserEq "linkMask" linkMaskInt
  _ <- outputFieldParserEq "linkMaskPopcnt" linkMaskPopcnt
  _ <- outputFieldParser "reframingEnabled" not "'expected `False`'"
  _ <- outputFieldParserLte "linksStable" linkCount
  _ <- outputFieldParserLte "linksSettled" linkCount
  _ <- outputDataCounts
  outputCMods

outputFieldParserEq ::
  (Eq a, Read a, Show a) =>
  String ->
  a ->
  Parsec String st ()
outputFieldParserEq name val =
  outputFieldParser
    name
    (== val)
    ("failed on `== " <> show val <> "`")

outputFieldParserLte ::
  (Ord a, Read a, Show a) =>
  String ->
  a ->
  Parsec String st ()
outputFieldParserLte name val =
  outputFieldParser
    name
    (<= val)
    ("failed on `<= " <> show val <> "`")

outputFieldParser ::
  (Read a, Show a) =>
  String ->
  (a -> Bool) ->
  String ->
  Parsec String st ()
outputFieldParser name cond errmsg = do
  _ <- nameParser name ": "
  _ <- valueParser cond '\n' errmsg
  return ()

nameParser :: String -> String -> Parsec String st ()
nameParser name term = do
  name' <- manyTill anyChar (try (string term))
  if name == name'
    then return ()
    else
      unexpected
        $ "names do not match. expected `"
        <> show name
        <> "`, found `"
        <> show name'
        <> "`"

valueParser :: (Read a, Show a) => (a -> Bool) -> Char -> String -> Parsec String st a
valueParser cond term errmsg = do
  value <- manyTill anyChar (try (char term))
  case readEither value of
    Right val ->
      if cond val
        then return val
        else
          unexpected
            $ "value `"
            <> show val
            <> "` did not meet condition. msg: "
            <> errmsg
    Left err -> unexpected err

outputDataCounts :: Parsec String st ()
outputDataCounts = output
 where
  output = do
    _ <- nameParser "dataCounts" ": "
    _ <- readDataCountsList
    _ <- char '\n'
    return ()
  readDataCountsList = between (char '[') (char ']') sepByCommas
  sepByCommas = sepBy readDataCount (string ", ")
  readDataCount = between (char '(') (char ')') countPair
  countPair = do
    countPair0 <- manyTill anyChar (try (string ", "))
    -- _ <- string ", "
    countPair1 <- many (noneOf ")")
    case (readEither countPair0 :: Either String Int) of
      Right _ -> case (readEither countPair1 :: Either String Int) of
        Right _ -> return ()
        Left err ->
          unexpected
            $ "failed to parse `"
            <> countPair1
            <> "` as a number. msg: "
            <> err
      Left err ->
        unexpected
          $ "failed to parse `"
          <> countPair0
          <> "` as a number. msg: "
          <> err

outputCMods :: Parsec String st [Int]
outputCMods = output
 where
  output = do
    _ <- nameParser "clockMod" "("
    numCMods <- valueParser (const True) ')' ""
    _ <- string ": "
    list <- readClockModList
    if numCMods == L.length list
      then return list
      else
        unexpected
          $ "specified list length "
          <> show numCMods
          <> " does not match actual length "
          <> show (L.length list)
  readClockModList = between (char '[') (char ']') sepByCommas
  sepByCommas = sepBy readClockMod (try (string ", "))
  readClockMod = do
    clockMod <- anyChar
    case clockMod of
      '0' -> return 0
      '1' -> return 1
      '2' -> return 2
      _ -> unexpected $ "unknown speed change `" <> show clockMod <> "`"
