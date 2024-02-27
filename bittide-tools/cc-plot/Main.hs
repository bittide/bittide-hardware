-- SPDX-FileCopyrightText: 2023-2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Extra.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

{-# OPTIONS_GHC -fconstraint-solver-iterations=20 #-}
module Main (main) where

import Clash.Prelude
  (BitPack(..), SNat(..), BitSize, Vec, (.|.), natToNum, shift, extend)

import Clash.Signal.Internal (Femtoseconds(..))
import qualified Clash.Sized.Vector as Vec
  (unsafeFromList, toList, repeat, zip, zipWith)

import GHC.TypeLits
import Data.Type.Equality ((:~:)(..))
import GHC.TypeLits.Compare ((:<=?)(..))
import GHC.TypeLits.Witnesses ((%<=?))
import GHC.TypeLits.Witnesses qualified as TLW (SNat(..))

import Conduit
  ( ConduitT, Void, (.|)
  , runConduit, sourceHandle, scanlC, dropC, mapC, sinkList, yield, await
  )
import Control.Exception (SomeException(..), Exception(..), throw, handle)
import Control.Monad (forM, foldM, filterM, unless)
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
import Data.List (uncons, sort)
import Data.Maybe (fromMaybe, fromJust)
import Data.Proxy (Proxy(..))
import qualified Data.Text as Text (unpack)
import qualified Data.Vector as Vector (length)
import GHC.IO.Exception (IOException(..), IOErrorType(..))
import System.IO (IOMode(..), Handle, openFile, hClose)
import System.Directory
  (listDirectory, doesDirectoryExist, createDirectoryIfMissing)
import System.Environment (getArgs, getProgName)
import System.FilePath ((</>), takeExtensions, takeBaseName, takeFileName)

import Bittide.Plot
import Bittide.ClockControl
import Bittide.ClockControl.StabilityChecker
import Bittide.Instances.Hitl.IlaPlot
import Bittide.Instances.Domains
import Bittide.Topology

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
-- 'Bittide.Instances.Hitl.FullMeshHwCc.callistoClockControlWithIla'.
data Capture (n :: Nat) (m :: Nat) =
  Capture
    { sampleInBuffer  :: Int
    , sampleInWindow  :: Int
    , trigger         :: Hex Bool
    , triggerSignal   :: Hex Bool
    , capture         :: Hex Bool
    , captureCond     :: Hex CaptureCondition
    , globalTimestamp :: Hex (GlobalTimestamp Basic125)
    , localTimestamp  :: Hex (DiffResult (LocalTimestamp GthTx))
    , plotData        :: Hex (PlotData n m)
    }

instance (KnownNat n, KnownNat m) => FromRecord (Capture n m) where
  parseRecord v =
    let len = 9 in
    if Vector.length v /= len
    then fail $ "Row with more than " <> show len <> " fields"
    else Capture
      <$> v .! 0 <*> v .! 1 <*> v .! 2 <*> v .! 3 <*> v .! 4
      <*> v .! 5 <*> v .! 6 <*> v .! 7 <*> v .! 8

-- | A data point resulting from post processing the captured data.
data DataPoint (n :: Nat) (m :: Nat) =
  DataPoint
    { dpIndex :: Int
      -- ^ the index of the corresponding sample of the dump
    , dpGlobalTime :: Femtoseconds
      -- ^ the absolute number of femtoseconds since the start of the
      -- dump according to the global synchronized clock
    , dpLastScheduledGlobalTime :: Femtoseconds
      -- ^ the absolute number of femtoseconds since the start of the
      -- dump according to the global synchronized clock at the time
      -- of the last scheduled capture
    , dpLocalTime :: Femtoseconds
      -- ^ the absolute number of femtoseconds since the start of the
      -- dump according to the local clock
    , dpLastScheduledLocalTime :: Femtoseconds
      -- ^ the absolute number of femtoseconds since the start of the
      -- dump according to the local clock at the time
      -- of the last scheduled capture
    , dpDrift :: Femtoseconds
      -- ^ the drift between the global an the local clocks in
      -- femtoseconds per scheduled capture period
    , dpCCChanges :: Int
      -- ^ the accumulated number FINC/FDECs integrated over time
    , dpRfStage :: ReframingStage
      -- ^ the reframing stage
    , dpDataCounts :: Vec n (DataCount m)
      -- ^ the elastic buffer data counts
    , dpStability :: Vec n StabilityIndication
      -- ^ the stability indicators for each of the elastic buffers
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
  forall n k c m .
  (KnownNat n, KnownNat k, KnownNat c, Monad m, c <= k) =>
  ConduitT (Capture n c) (DataPoint n k) m ()
postProcess = scanlC process initDummy
  -- finally drop the dummy, the trigger and the calibration entries
  .| (dropC 4 >> mapC id)
 where
  process prevDP Capture{..} =
    let PlotData{..}    = fromHex plotData
        captureType     = fromHex captureCond

        localStamp      =
          let ref = "[" <> show sampleInBuffer <> "]"
          in case fromHex localTimestamp of
            NoReference  -> error $ "LT: no reference " <> ref
            TooLarge     -> error $ "LT: too large " <> ref
            Difference x -> x + 1

        globalTime      = globalTsToFs $ fromHex globalTimestamp
        localTime       = case captureType of
          UntilTrigger -> globalTime
          _            -> dpLocalTime prevDP
                        + localStamp ~* clockPeriodFs (Proxy @GthTx)

        globalTimeDelta = globalTime
                        - dpLastScheduledGlobalTime prevDP
        localTimeDelta  = localTime
                        - dpLastScheduledLocalTime prevDP

        driftPerCycle   = case captureType of
          DataChange -> dpDrift prevDP
          _          -> fromIntegral (globalTimeDelta - localTimeDelta)

        ccChanges       = dpCCChanges prevDP
                        + natToNum @AccWindowHeight * sign dSpeedChange

        dataCounts      = (\(x,_,_) y -> extend @_ @c @(k - c) x + y)
                            <$> dEBData
                            <*> dpDataCounts prevDP
     in DataPoint
          { dpIndex                   = sampleInBuffer
          , dpGlobalTime              = globalTime
          , dpLastScheduledGlobalTime =
              case captureType of
                DataChange -> dpLastScheduledGlobalTime prevDP
                _          -> globalTime
          , dpLocalTime               = localTime
          , dpLastScheduledLocalTime  =
              case captureType of
                DataChange -> dpLastScheduledLocalTime prevDP
                _          -> localTime
          , dpDrift                   = driftPerCycle
          , dpCCChanges               = ccChanges
          , dpRfStage                 =
              case dRfStageChange of
                Stable   -> dpRfStage prevDP
                ToDetect -> RSDetect
                ToWait   -> RSWait
                ToDone   -> RSDone
          , dpDataCounts              = dataCounts
          , dpStability               =
              let combine (_, st, se) StabilityIndication{..} =
                    StabilityIndication
                      { stable  = fromMaybe stable st
                      , settled = fromMaybe settled se
                      }
               in Vec.zipWith combine dEBData $ dpStability prevDP
          }

  initDummy = DataPoint
    { dpIndex                   = -1
    , dpGlobalTime              = 0
    , dpLastScheduledGlobalTime = 0
    , dpLocalTime               = 0
    , dpLastScheduledLocalTime  = 0
    , dpDrift                   = 0
    , dpCCChanges               = 0
    , dpRfStage                 = RSDetect
    , dpDataCounts              = Vec.repeat 0
    , dpStability               =
        Vec.repeat $ StabilityIndication
          { stable  = False
          , settled = False
          }
    }

  globalTsToFs :: GlobalTimestamp Basic125 -> Femtoseconds
  globalTsToFs (pulses, cycles) =
    (pulses ~* syncPulsePeriodFs) +
    (cycles ~* clockPeriodFs (Proxy @Basic125))
   where
    syncPulsePeriodFs = Femtoseconds $ natToNum @(1000 * SyncPulsePeriod)

fromCsvDump ::
  forall n m.
  (KnownNat n, KnownNat m, 1 <= n, CompressedBufferSize <= m) =>
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
  .| postProcess @(n - 1) @m @CompressedBufferSize
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
    unless (null yr) $ wrongNumberOfArguments
    dirs <- listDirectory ilaDir
      >>= filterM doesDirectoryExist . fmap (ilaDir </>)

    case fromJust $ someNatVal $ toInteger $ length dirs of
      SomeNat (_ :: p n) -> case TLW.SNat @1 %<=? TLW.SNat @n of
        LE Refl -> do
          postProcessData <- do
            forM (sort dirs) $ \d -> concat <$> do
              csvFiles <- checkCsvFilesExist d <$> listDirectory d
              forM csvFiles $ \file -> do
                h <- openFile file ReadMode
                rs <- handle (\SomeException{} -> return []) $ do
                  rs <- runConduit $ fromCsvDump @n @CccBufferSize (h, file)
                  putStrLn $ "Using " <> (takeBaseName d </> takeFileName file)
                  return rs
                hClose h
                return (toPlotData <$> rs)

          createDirectoryIfMissing True outDir
          plotTopology outDir (complete (SNat :: SNat n))
            $ Vec.unsafeFromList postProcessData

        _ -> error $ ilaDir <> " is expected to contain sub-directories."
 where
  checkCsvFilesExist d xs =
    let ys = filter ((== ".csv") . takeExtensions) $ fmap (d </>) xs
     in case ys of
       [] -> error $ d <> " does not contain any *.csv files. Aborting."
       _  -> ys

  wrongNumberOfArguments = do
    name <- getProgName
    error $ "Wrong number of arguments. Aborting.\n\nUsage: " <> name
         <> " <ila plot directory> [<output directory>]"

  toPlotData DataPoint{..} =
    ( dpGlobalTime
    , dpDrift
    , dpRfStage
    , Vec.toList $ Vec.zip dpDataCounts dpStability
    )
