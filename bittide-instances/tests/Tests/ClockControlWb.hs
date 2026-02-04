-- SPDX-FileCopyrightText: 2023 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE RecordWildCards #-}

module Tests.ClockControlWb where

-- preludes
import Clash.Explicit.Prelude hiding (PeriodToCycles, many)

-- external imports

import Data.Char (chr)
import Data.Maybe (catMaybes, mapMaybe)
import Data.String.Interpolate
import Protocols
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.TH
import Text.Parsec
import Text.Parsec.String

-- internal imports
import Bittide.Arithmetic.Time (PeriodToCycles)
import Bittide.ClockControl.Registers (ClockControlData (clockMod))
import Bittide.Instances.Hitl.Setup (LinkCount)
import Bittide.Instances.Tests.ClockControlWb

-- qualified imports
import qualified Data.List as L

-- | The expected output of the UART
data SerialResult = SerialResult
  { linkCount :: Int
  , linkMask :: Int
  , linksOk :: Int
  , linkMaskPopcnt :: Int
  , linksStable :: Int
  , linksSettled :: Int
  , dataCounts :: [(Int, Int)]
  , clockMod :: [Int]
  }
  deriving (Show, Eq)

-- | Simulate the serial output of the cpu
sim :: IO ()
sim =
  putStr
    $ fmap (chr . fromIntegral)
    $ catMaybes
    $ fst (sampleC def dutNoMm)

case_clock_control_wb_self_test :: Assertion
case_clock_control_wb_self_test = do
  case parse resultParser "" uartString of
    Left err -> do
      print err
      assertFailure "Could not parse output"
    Right actual -> do
      let
        expected =
          SerialResult
            { linkCount = linkCount
            , linkMask = fromIntegral linkMask
            , linksOk = fromIntegral linksOk
            , linkMaskPopcnt = linkMaskPopcnt
            , linksStable = 0
            , linksSettled = 0
            , dataCounts = expectedDataCounts
            , clockMod =
                L.take (L.length actual.clockMod)
                  $ fromIntegral
                  . pack
                  <$> mapMaybe (.clockMod) ccData
            }
      assertEqual "Expected and actual differ" expected actual
 where
  uartString = chr . fromIntegral <$> catMaybes uartStream
  (uartStream, ccData) = sampleC def dutNoMm

type Margin = SNat 2
type Framesize = PeriodToCycles System (Seconds 1)

margin :: Margin
margin = SNat

framesize :: SNat Framesize
framesize = SNat

linkCount :: Int
linkCount = snatToNum (SNat @LinkCount)

linkMaskPopcnt :: Int
linkMaskPopcnt = fromIntegral $ popCount linkMask

expectedDataCounts :: [(Int, Int)]
expectedDataCounts = L.zip [0 ..] $ toList $ applyMask linkMask dataCounts
 where
  applyMask m = zipWith go (bitCoerce m)
  go m v = if m then fromIntegral v else 0

-- | Parse the output of the UART
resultParser :: Parser SerialResult
resultParser = do
  linkCountResult <- expectField "nLinks"
  linkMaskResult <- expectField "linkMask"
  linksOkResult <- expectField "linksOk"
  linkMaskPopcntResult <- expectField "linkMaskPopcnt"
  linksStableResult <- expectField "linksStable"
  linksSettledResult <- expectField "linksSettled"
  dataCountsResult <- expectField "dataCounts"
  clockModResult <- expectField "clockMod"
  return
    SerialResult
      { linkCount = linkCountResult
      , linkMask = linkMaskResult
      , linksOk = linksOkResult
      , linkMaskPopcnt = linkMaskPopcntResult
      , linksStable = linksStableResult
      , linksSettled = linksSettledResult
      , dataCounts = dataCountsResult
      , clockMod = clockModResult
      }

{- | A parser that parses a string in the form "name: value\n"
The parsed name should be equal to the given fieldName
-}
expectField :: (Read a) => String -> Parser a
expectField fieldName = do
  (actualName, value) <- fieldParser
  if actualName == fieldName
    then pure value
    else do
      parserFail
        [__i|
        expectField failed to match the expected field name.
        Expected field: #{fieldName}
        Actual field:   #{actualName}|]

-- | Parse a field of the form "name: value\n"
fieldParser :: (Read a) => Parser (String, a)
fieldParser = do
  name <- manyTill anyChar (try (string ": "))
  value <- manyTill anyChar (try (string "\n"))
  return (name, read value)

{- | Return the beginning of a list until you detect a certain sublist
That substring is not included in the result.
-}
splitAtSublist :: (Eq a) => [a] -> [a] -> Maybe ([a], [a])
splitAtSublist subList = recurse []
 where
  recurse _ [] = Nothing
  recurse acc remaining@(h : t)
    | subList `L.isPrefixOf` remaining = Just (acc, L.drop (L.length subList) remaining)
    | otherwise = recurse (acc <> [h]) t

tests :: TestTree
tests = $(testGroupGenerator)
