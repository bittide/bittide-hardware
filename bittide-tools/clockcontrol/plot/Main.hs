-- SPDX-FileCopyrightText: 2023-2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Extra.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

{-# OPTIONS_GHC -fconstraint-solver-iterations=20 #-}
module Main (main) where

import Clash.Prelude
  (BitPack(..), SNat(..), Vec, Index, natToNum, extend)

import Clash.Signal.Internal (Femtoseconds(..))
import qualified Clash.Sized.Vector as Vec
  ((!!), unsafeFromList, toList, repeat, zip, zipWith, indicesI, length)

import GHC.TypeLits
import Data.Type.Equality ((:~:)(..))
import GHC.TypeLits.Compare ((:<=?)(..))
import GHC.TypeLits.Witnesses ((%<=?))
import GHC.TypeLits.Witnesses qualified as TLW (SNat(..))

import Conduit
  ( ConduitT, Void, (.|)
  , runConduit, sourceHandle, scanlC, dropC, mapC, sinkList, yield, await
  )
import Control.Exception (Exception(..), catch, throw)
import Control.Monad (forM, forM_, filterM, unless)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.UTF8 as UTF8
import Data.Char (isDigit)
import Data.Csv
  ( FromField(..), FromNamedRecord(..), ToNamedRecord(..), (.:)
  , defaultDecodeOptions, encodeByName
  )
import Data.Csv.Conduit
  ( CsvStreamRecordParseError(..), CsvStreamHaltParseError(..)
  , fromNamedCsvStreamError
  )
import Data.List (uncons, sort, isPrefixOf, isSuffixOf)
import Data.Maybe (fromMaybe, fromJust)
import Data.Proxy (Proxy(..))
import Data.String (fromString)
import qualified Data.Text as Text (unpack)
import qualified Data.Vector as Vector (fromList)
import qualified Data.HashMap.Strict as HashMap (fromList, size)
import GHC.IO.Exception (IOException(..), IOErrorType(..))
import System.Directory (listDirectory, doesDirectoryExist, createDirectoryIfMissing)
import System.Environment (getArgs, getProgName)
import System.FilePath ((</>), takeExtensions, takeBaseName, takeFileName)
import Numeric.Extra (parseHex)
import System.IO (IOMode(..), Handle, openFile, hClose)

import Bittide.Plot
import Bittide.ClockControl
import Bittide.ClockControl.StabilityChecker
import Bittide.Github.Artifacts
import Bittide.Instances.Hitl.IlaPlot
import Bittide.Instances.Domains
import Bittide.Topology

-- A newtype wrapper for working with hex encoded types.
newtype Hex a = Hex { fromHex :: a }
  deriving newtype (BitPack)

instance BitPack a => FromField (Hex a) where
  parseField = either fail pure . parseHex . UTF8.toString

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

instance (KnownNat n, KnownNat m) => FromNamedRecord (Capture n m) where
  parseNamedRecord v =
    if HashMap.size v /= 3 + Vec.length ilaProbeNames
    then fail "Row with more than 8 fields"
    else Capture
      <$> v .: "Sample in Buffer"
      <*> v .: "Sample in Window"
      <*> v .: "TRIGGER"
      <*> v .: portName 0
      <*> v .: portName 1
      <*> v .: portName 2
      <*> v .: portName 3
      <*> v .: portName 4
      <*> v .: portName 5
   where
    portName = portName# ilaProbeNames

    portName# :: KnownNat k => Vec k String -> Index k -> BS.ByteString
    portName# names = fromString . (names Vec.!!)

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

instance (KnownNat n, KnownNat m) => ToNamedRecord (DataPoint n m) where
  toNamedRecord DataPoint{..} = HashMap.fromList $
    fmap (\(x, y) -> (BSC.pack x, BSC.pack y)) $
      [ ("Index", show dpIndex)
      , ("Synchronized Time (fs)", show $ toInteger dpGlobalTime)
      , ("Local Clock time (fs)", show $ toInteger dpLocalTime)
      , ("Clock Period Drift (fs)", show $ toInteger dpDrift)
      , ("Integrated FINC/FDECs", show dpCCChanges)
      , ("Reframing State", rf2bs dpRfStage)
      ] <>
      [ ("EB " <> show i, show $ toInteger x)
      | (i, x) <- Vec.toList $ Vec.zip Vec.indicesI dpDataCounts
      ] <>
      [ (show i <> " is stable", b2bs $ stable x)
      | (i, x) <- Vec.toList $ Vec.zip Vec.indicesI dpStability
      ] <>
      [ (show i <> " is settled", b2bs $ settled x)
      | (i, x) <- Vec.toList $ Vec.zip Vec.indicesI dpStability
      ]
   where
    b2bs = \case
      False -> "0"
      True  -> "1"

    rf2bs = \case
      RSDetect -> "Detect"
      RSWait   -> "Wait"
      RSDone   -> "Done"

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

type CsvParseError = CsvParseError# CsvStreamRecordParseError
data CsvParseError# e = CsvParseError Int e deriving (Show)
instance Exception CsvParseError

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
  .| fromNamedCsvStreamError defaultDecodeOptions toIOE
     -- drop the first two header lines and ensure that the remaining
     -- data entries are valid
  .| (dropC 1 >> checkForErrors 0)
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

  checkForErrors n = await >>= \case
    Nothing        -> return ()
    Just (Left e)  -> throw $ CsvParseError n e
    Just (Right x) -> yield x >> checkForErrors (n + 1)

plotTest :: FilePath -> [FilePath] -> FilePath -> IO ()
plotTest testDir dirs globalOutDir =
  case fromJust $ someNatVal $ toInteger $ length dirs of
    SomeNat (_ :: p n) -> case TLW.SNat @1 %<=? TLW.SNat @n of
      LE Refl -> do
        putStrLn $ "Creating plots for test case: " <> testName
        postProcessData <- do
          forM (sort dirs) $ \d -> concat <$> do
            csvFiles <- checkCsvFilesExist d <$> listDirectory d
            forM csvFiles $ \f -> do
              h <- openFile f ReadMode
              rs <- catch
                (do rs <- runConduit $ fromCsvDump @n @CccBufferSize (h, f)
                    putStrLn ("Using " <> (takeBaseName d </> takeFileName f))
                    return rs
                ) $ \(err :: CsvParseError) -> case err of
                    -- Ignore additional CSV files that may have
                    -- been produced by other ILAs. They are
                    -- identified via an error while parsing the
                    -- header row.
                    CsvParseError 0 _ -> return []
                    CsvParseError n (CsvStreamRecordParseError msg) ->
                      error $ unlines
                        [ "Error while parsing"
                        , ""
                        , "  " <> f
                        , ""
                        , "Line " <> show (n + 3) <> ", " <> Text.unpack msg
                        ]
              hClose h

              let
                k = length dirs - 1
                header = Vector.fromList $ map BSC.pack $
                  [ "Index"
                  , "Synchronized Time (fs)"
                  , "Local Clock time (fs)"
                  , "Clock Period Drift (fs)"
                  , "Integrated FINC/FDECs"
                  , "Reframing State"
                  ]
                  <> (("EB " <>) . show <$> [0,1..k - 1])
                  <> ((<> " is stable") . show <$> [0,1..k - 1])
                  <> ((<> " is settled") . show <$> [0,1..k - 1])

              unless (null rs) $ do
                createDirectoryIfMissing True $ outDir
                BSL.writeFile (outDir </> (takeFileName d <> ".csv"))
                  $ encodeByName header rs

              return (toPlotData <$> rs)

        createDirectoryIfMissing True $ outDir
        plotTopology outDir (complete (SNat :: SNat n))
          $ Vec.unsafeFromList postProcessData

      _ -> error $ testDir <> " is expected to contain sub-directories."
 where
  testName = takeFileName testDir
  outDir = globalOutDir </> testName

  checkCsvFilesExist d xs =
    let ys = filter ((== ".csv") . takeExtensions) $ fmap (d </>) xs
     in case ys of
       [] -> error $ d <> " does not contain any *.csv files. Aborting."
       _  -> ys

  toPlotData DataPoint{..} =
    ( dpGlobalTime
    , dpDrift
    , dpRfStage
    , Vec.toList $ Vec.zip dpDataCounts dpStability
    )

main :: IO ()
main = getArgs >>= \case
  [] -> wrongNumberOfArguments
  ilaSource : xr -> do
    (ilaDir, outDir, mArtifactName) <- do
      isDir <- doesDirectoryExist ilaSource
      (ilaDir, yr, mA) <-
        if isDir
        then return (ilaSource, xr, Nothing)
        else case isRunArtifactReference ilaSource of
          Nothing -> error $ "ERROR - Invalid argument: " <> ilaSource
          Just (runId, artifactName) -> case xr of
            [] -> wrongNumberOfArguments
            dir : yr ->
              let fullArtifactName = "_build-" <> artifactName <> "-debug"
               in retrieveArtifact runId fullArtifactName dir >>= \case
                    Just err -> error $ unlines
                      [ "ERROR - Cannot retrieve artifact."
                      , show err
                      ]
                    Nothing -> return (dir, yr, Just artifactName)
      let (outDir, zr) = fromMaybe (".", []) $ uncons yr
      unless (null zr) wrongNumberOfArguments
      return (ilaDir, outDir, mA)

    testDirs <- do
      let epsfix = maybe (Left "Bittide.Instances.Hitl.") Right mArtifactName
      dir <- diveDownInto epsfix ilaDir
      listDirectory dir
        >>= filterM doesDirectoryExist . fmap (dir </>)

    forM_ testDirs $ \testDir -> do
      nodeDirs <- listDirectory testDir
        >>= filterM doesDirectoryExist . fmap (testDir </>)
      plotTest testDir nodeDirs outDir
 where
  diveDownInto epsfix dir =
    listDirectory dir
      >>= filterM doesDirectoryExist . fmap (dir </>)
      >>= \case
        [] -> error $ "ERROR - empty directory: " <> dir
        dirs -> let subDirs = takeFileName <$> dirs in if
          | "vivado" `elem` subDirs ->
              diveDownInto epsfix $ dir </> "vivado"
          | "ila-data" `elem` subDirs ->
              diveDownInto epsfix $ dir </> "ila-data"
          | otherwise ->
              case filter (either isPrefixOf isSuffixOf epsfix) subDirs of
                subDir : _ -> diveDownInto epsfix $ dir </> subDir
                _ -> return dir

  isRunArtifactReference arg = case span ((/=) ':') arg of
    (xs, ':':ys)
      | all isDigit xs && ':' `notElem` ys -> Just (xs, ys)
      | otherwise -> Nothing
    _ -> Nothing

  wrongNumberOfArguments = do
    name <- getProgName
    error $ "Wrong number of arguments. Aborting.\n\nUsage: " <> name
         <> " <ila plot directory> [<output directory>]"
