-- SPDX-FileCopyrightText: 2023 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}
module Main (main) where

import Clash.Prelude
  ( BitPack(..), SNat(..), BitSize, Nat, KnownNat, Vec
  , type (*), type (-), type (<=), (.&&.), (.|.)
  , natToNum, shift
  )
import Clash.Signal.Internal (Femtoseconds(..))
import qualified Clash.Sized.Vector as Vec
  ( unsafeFromList, toList, repeat, zip, zipWith
  )

import Conduit
  ( ConduitT, Void, (.|)
  , runConduit, sourceHandle, scanlC, dropC, mapC, sinkList, yield, await
  )
import Control.Exception (Exception, throw)
import Control.Monad (when, forM, foldM, filterM)
import Data.ByteString.Internal (w2c)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Char (isHexDigit, digitToInt)
import Data.Csv
  ( FromField(..), FromRecord(..), HasHeader(..), (.!)
  , defaultDecodeOptions
  )
import Data.Csv.Conduit
  ( CsvStreamRecordParseError, CsvStreamHaltParseError(..)
  , fromCsvStreamError
  )
import Data.List (uncons)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy(..))
import qualified Data.Text as Text (unpack)
import qualified Data.Vector as Vector (length)
import GHC.IO.Exception (IOException(..), IOErrorType(..))
import System.IO (IOMode(..), Handle, openFile, hClose)
import System.Directory (listDirectory, doesDirectoryExist, createDirectoryIfMissing)
import System.Environment (getArgs, getProgName)
import System.FilePath ((</>), takeExtensions, takeBaseName)

import Bittide.Plot
import Bittide.ClockControl
import Bittide.ClockControl.StabilityChecker
import Bittide.Instances.Tests.FullMeshHwCc
import Bittide.Instances.Tests.FullMeshHwCc.IlaPlot
import Bittide.Instances.Domains
import Bittide.Topology.Graph

-- A newtype wrapper for working with hex encoded types.
newtype Hex a = Hex { fromHex :: a }
  deriving newtype (BitPack)

instance BitPack a => FromField (Hex a) where
  parseField bs = (unpack <$>) $ foldM pHex 0
    $ reverse $ zip [0,1..] $ reverse (w2c <$> BS.unpack bs)
   where
    pHex !a (i, c)
      | not (isHexDigit c) =
          fail $ "Non-hexadecimal digit: " <> [c]
      | i * 4 + log2 (digitToNum c) > natToNum @(BitSize a) =
          fail $ "Value is out of range: " <> BSC.unpack bs
      | otherwise =
          return $ shift a 4 .|. digitToNum c

    -- 'digitToInt' produces only 16 different values. Hence, it can be
    --  extended to work for any 'Num' instance.
    digitToNum x = case digitToInt x of
      { 0 -> 0; 1 -> 1; 2 -> 2; 3 -> 3; 4 -> 4; 5 -> 5; 6 -> 6; 7 -> 7; 8 -> 8
      ; 9 -> 9; 10 -> 10; 11 -> 11; 12 -> 12; 13 -> 13; 14 -> 14; 15 -> 15
      ; y | y < 0     -> error "digitToInt returned some negative value"
          | otherwise -> error "digitToInt returned some value greater than 15"
      }

    log2 :: Int -> Int
    log2 =
      let log2' !a 0 = a
          log2' !a n = log2' (a + 1) $ n `div` 2
      in  log2' 0

-- | The captured data entries, as they are dumped by the ILA of
-- 'Bittide.Instances.Tests.FullMeshHwCc.callistoClockControlWithIla'.
data Capture (n :: Nat) (m :: Nat) =
  Capture
    { sampleInBuffer  :: Int
    , sampleInWindow  :: Int
    , trigger         :: Hex Bool
    , triggerSignal   :: Hex Bool
    , capture         :: Hex Bool
    , captureCond     :: Hex CaptureCondition
    , globalTimestamp :: Hex GlobalTimestamp
    , localTimestamp  :: Hex LocalTimestamp
    , plotData        :: Hex (PlotData n m)
    }

instance (KnownNat n, KnownNat m) => FromRecord (Capture n m) where
  parseRecord v =
    if Vector.length v /= 9
    then fail $ "Row with more than 9 fields" <> show v
    else Capture
      <$> v .! 0 <*> v .! 1 <*> v .! 2 <*> v .! 3 <*> v .! 4
      <*> v .! 5 <*> v .! 6 <*> v .! 7 <*> v .! 8

-- | A data point resulting from post processing the captured data.
data DataPoint (n :: Nat) (m :: Nat) =
  DataPoint
    { dpIndex       :: Int
      -- ^ the index of the corresponding sample of the dump
    , dpLocalStamp  :: LocalTimestamp
      -- ^ the local time stamp counting the cycles of the dynamic clock
    , dpGlobalTime  :: Femtoseconds
      -- ^ the absolute number of femtoseconds since the start of the
      -- dump according to the global synchronized clock
    , dpLocalTime   :: Femtoseconds
      -- ^ the absolute number of femtoseconds since the start of the
      -- dump according to the local dynamic clock
    , dpDrift       :: Double
      -- ^ the drift between the global an the local clocks in
      -- femtoseconds per cycle of the global clock
    , dpCCChanges   :: Int
      -- ^ the accumulated number FINC/FDECs integrated over time
    , dpDataCounts  :: Vec n (DataCount m)
      -- ^ the elastic buffer data counts
    , dpStability   :: Vec n StabilityIndication
      -- ^ the stability indicators for each of the elastic buffers
    , dpRfStage      :: ReframingStage
      -- ^ the reframing stage
    }

-- | Multiplies some 'Femtoseconds' with any numerical value. Note
-- that this operation can produce negative values, which is
-- intentional.
infix 9 ~*
(~*) :: Integral a => a -> Femtoseconds -> Femtoseconds
a ~* (Femtoseconds b) = Femtoseconds $ fromInteger (toInteger a) * b

deriving newtype instance Num Femtoseconds
deriving newtype instance Real Femtoseconds
deriving newtype instance Enum Femtoseconds
deriving newtype instance Integral Femtoseconds

newtype CsvParseError e = CsvParseError e deriving newtype (Show)
instance Exception (CsvParseError CsvStreamRecordParseError)

-- | Data post processor.
postProcess ::
  (KnownNat n, KnownNat k, Monad m) =>
  ConduitT (Capture n k) (DataPoint n k) m ()
postProcess = scanlC process initDummy
  -- finally drop the dummy, the trigger and the calibration entry
  .| (dropC 3 >> mapC id)
 where
  process prevDP Capture{..} =
    let PlotData{..}    = fromHex plotData
        captureType     = fromHex captureCond
        localStamp      = fromHex localTimestamp
        ccChanges       = dpCCChanges prevDP
                        + natToNum @AccWindowHeight * sign dSpeedChange
        globalTime      = globalTsToFs $ fromHex globalTimestamp
        globalTimeDelta = globalTime - dpGlobalTime prevDP
        localTime       = case captureType of
          UntilTrigger -> 0
          _            ->  (+) (dpLocalTime prevDP)
                        $ (~*) (localStamp - dpLocalStamp prevDP)
                        $  (+) (clockPeriodFs (Proxy @GthTx))
                        $ (~*) (dpCCChanges prevDP)
                               (cccStepSize clockControlConfig)
        localTimeDelta  = localTime - dpLocalTime prevDP
        driftPerCycle   = fromIntegral (globalTimeDelta - localTimeDelta)
                        / fromIntegral globalTimeDelta
    in DataPoint
         { dpIndex      = sampleInBuffer
         , dpLocalStamp = localStamp
         , dpGlobalTime = globalTime
         , dpLocalTime  = localTime
         , dpDrift      = driftPerCycle
         , dpCCChanges  = ccChanges
         , dpDataCounts = (\(x,_,_) -> x) <$> dEBData
         , dpStability  =
             let combine (_, st, se) StabilityIndication{..} =
                   StabilityIndication
                     { stable  = fromMaybe stable st
                     , settled = fromMaybe settled se
                     }
              in Vec.zipWith combine dEBData $ dpStability prevDP
         , dpRfStage = case dRfStageChange of
             Stable   -> dpRfStage prevDP
             ToDetect -> RSDetect
             ToWait   -> RSWait
             ToDone   -> RSDone
         }

  globalTsToFs :: GlobalTimestamp -> Femtoseconds
  globalTsToFs (pulses, cycles) =
    (pulses ~* syncPulsePeriodFs) +
    (cycles ~* clockPeriodFs (Proxy @Basic125))
   where
    syncPulsePeriodFs = Femtoseconds $ natToNum @(1000 * SyncPulsePeriod)

  initDummy = DataPoint
    { dpIndex      = -1
    , dpLocalStamp = 0
    , dpGlobalTime = 0
    , dpLocalTime  = 0
    , dpDrift      = 0
    , dpCCChanges  = 0
    , dpDataCounts = Vec.repeat 0
    , dpStability  = Vec.repeat $ StabilityIndication
        { stable  = False
        , settled = False
        }
    , dpRfStage    = RSDetect
    }

fromCsvDump ::
  (KnownNat n, KnownNat m, 1 <= n) =>
  (Handle, FilePath) ->
  ConduitT () Void IO [DataPoint (n - 1) m]
fromCsvDump  (csvHandle, csvFile) =
     -- turn the input file into a conduit source
     sourceHandle csvHandle
     -- plugin the CSV parser
  .| fromCsvStreamError defaultDecodeOptions NoHeader toIOE
     -- drop the first two header lines and ensure that the remaining
     -- data entries are valid
  .| (dropC 2 >> checkForErrors)
     -- post process the data
  .| postProcess
     -- return as a list
  .| sinkList
 where
  toIOE (HaltingCsvParseError _ msg) = IOError
    { ioe_handle      = Just csvHandle
    , ioe_type        = SystemError
    , ioe_location    = csvFile
    , ioe_description = Text.unpack msg
    , ioe_errno       = Nothing
    , ioe_filename    = Just csvFile
    }

  checkForErrors = await >>= \case
    Nothing        -> return ()
    Just (Left e)  -> throw $ CsvParseError e
    Just (Right x) -> yield x >> checkForErrors

main :: IO ()
main = getArgs >>= \case
  []          -> wrongNumberOfArguments
  ilaDir : xr -> do
    let (outDir, yr) = fromMaybe (".", []) $ uncons xr
    prefix <- case yr of
      []  -> return $ "Bittide_Instances_Tests_FullMeshHwCc_fullMeshHwCcTest_"
                   <> "callistoClockControlWithIla"
      [x] -> return x
      _   -> wrongNumberOfArguments
    postProcessData <- do
      dirs <- listDirectory ilaDir
        >>= filterM doesDirectoryExist
          . fmap (ilaDir </>)
      when (length dirs /= nodeCount) $
        error $ "There are "
             <> (if length dirs > nodeCount then "more" else "less")
             <> " than " <> show nodeCount <> " directories in "
             <> ilaDir <> ". Aborting"
      forM dirs $ \d -> listDirectory d
        >>= fmap fst
          . maybe
              (error $ d <> " does not contain a matching .csv file. Aborting.")
              return
          . uncons
          . filter (isIlaDumpFile prefix)
          . fmap (d </>)
        >>= \file -> do
          h <- openFile file ReadMode
          rs <- runConduit $ fromCsvDump @NodeCount @DataCountSize (h, file)
          hClose h
          return (toPlotData <$> rs)
    createDirectoryIfMissing True outDir
    plotTopology outDir (complete (SNat :: SNat NodeCount))
      $ Vec.unsafeFromList postProcessData
 where
  nodeCount = natToNum @NodeCount

  isIlaDumpFile prefix =
         (== ".csv") . takeExtensions
    .&&. and . zipWith (==) prefix . takeBaseName

  wrongNumberOfArguments = do
    name <- getProgName
    error $ "Wrong number of arguments. Aborting.\n\nUsage: " <> name
         <> " <ila plot directory> [<output directory>] [<csv file prefix>]"

  toPlotData DataPoint{..} =
    ( dpGlobalTime
    , dpDrift
    , dpRfStage
    , Vec.toList $ Vec.zip dpDataCounts dpStability
    )
