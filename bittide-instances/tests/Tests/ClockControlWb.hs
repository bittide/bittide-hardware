-- SPDX-FileCopyrightText: 2023 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=20 #-}
{-# OPTIONS_GHC -fplugin=Protocols.Plugin #-}

module Tests.ClockControlWb where

-- preludes
import Clash.Explicit.Prelude hiding (PeriodToCycles, many)

-- external imports
import Clash.Signal (withClockResetEnable)
import Data.Char (chr)
import Data.Maybe (mapMaybe)
import Data.String.Interpolate
import Language.Haskell.TH
import Project.FilePath
import Protocols
import Protocols.Idle
import System.FilePath
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.TH
import Text.Parsec
import Text.Parsec.String
import VexRiscv (DumpVcd (NoDumpVcd))

-- internal imports
import Bittide.Arithmetic.Time (PeriodToCycles)
import Bittide.ClockControl.DebugRegister (DebugRegisterCfg (..), debugRegisterWb)
import Bittide.ClockControl.Registers (ClockControlData, clockControlWb, clockMod)
import Bittide.DoubleBufferedRam
import Bittide.Instances.Hitl.HwCcTopologies (cSigMap, csDupe)
import Bittide.Instances.Hitl.Setup (LinkCount)
import Bittide.ProcessingElement
import Bittide.ProcessingElement.Util
import Bittide.SharedTypes
import Bittide.Wishbone

-- qualified imports
import qualified Data.List as L
import qualified Protocols.Df as Df

-- | The expected output of the UART
data SerialResult = SerialResult
  { linkCount :: Int
  , linkMask :: Int
  , linkMaskPopcnt :: Int
  , reframingEnabled :: Bool
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
    . mapMaybe Df.dataToMaybe
    $ fst (sampleC def dut)

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
            , linkMaskPopcnt = linkMaskPopcnt
            , reframingEnabled = False
            , linksStable = 0
            , linksSettled = 0
            , dataCounts = expectedDataCounts
            , clockMod =
                L.take (L.length actual.clockMod) $ fromIntegral . pack <$> mapMaybe clockMod ccData
            }
      putStrLn ""
      putStrLn "Actual |"
      print actual
      print expected
      putStrLn "Expected ^"
      assertBool "Expected and actual differ" $ actual == expected
 where
  uartString = chr . fromIntegral <$> mapMaybe Df.dataToMaybe uartStream
  (uartStream, ccData) = sampleC def dut

type Margin = SNat 2
type Framesize = PeriodToCycles System (Seconds 1)

margin :: Margin
margin = SNat
framesize :: SNat Framesize
framesize = SNat
linkCount :: Int
linkCount = snatToNum (SNat @LinkCount)
linkMask :: BitVector LinkCount
linkMask = 0b1011011
linkMaskPopcnt :: Int
linkMaskPopcnt = fromIntegral $ popCount linkMask

dataCounts :: Vec LinkCount (Signed 27)
dataCounts = iterateI (satSucc SatWrap) 0

expectedDataCounts :: [(Int, Int)]
expectedDataCounts = L.zip [0 ..] $ toList $ applyMask linkMask dataCounts
 where
  applyMask m = zipWith go (bitCoerce m)
  go m v = if m then fromIntegral v else 0

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
      [uartBus, ccWb, dbgWb] <- processingElement NoDumpVcd peConfig -< jtag
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

-- | Parse the output of the UART
resultParser :: Parser SerialResult
resultParser = do
  linkCountResult <- expectField "nLinks"
  linkMaskResult <- expectField "linkMask"
  linkMaskPopcntResult <- expectField "linkMaskPopcnt"
  reframingEnabledResult <- expectField "reframingEnabled"
  linksStableResult <- expectField "linksStable"
  linksSettledResult <- expectField "linksSettled"
  dataCountsResult <- expectField "dataCounts"
  clockModResult <- expectField "clockMod"
  return
    SerialResult
      { linkCount = linkCountResult
      , linkMask = linkMaskResult
      , linkMaskPopcnt = linkMaskPopcntResult
      , reframingEnabled = reframingEnabledResult
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
