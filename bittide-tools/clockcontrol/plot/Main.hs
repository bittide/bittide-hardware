-- SPDX-FileCopyrightText: 2023-2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Extra.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

{-# OPTIONS_GHC -fconstraint-solver-iterations=20 #-}
module Main (main, knownTestsWithSimConf) where

import Clash.Prelude
  ( BitPack(..), SNat(..), Vec, Index
  , checkedTruncateB, clockPeriod, extend
  , natToInteger, natToNum, snatProxy, snatToInteger
  )

import Clash.Signal.Internal (Femtoseconds(..))
import qualified Clash.Sized.Vector as Vec
  ( (!!), imap, unsafeFromList, toList, repeat
  , zip, zipWith, indicesI, length, take
  )

import GHC.TypeLits
import Data.Type.Equality ((:~:)(..))
import GHC.TypeLits.Compare ((:<=?)(..))
import GHC.TypeLits.Witnesses ((%<=?))
import GHC.TypeLits.Witnesses qualified as TLW (SNat(..))

import Conduit
  ( ConduitT, Void, (.|)
  , runConduit, sourceHandle, scanlC, dropC, mapC, sinkList, yield, await
  )
import Control.Arrow (first)
import Control.Exception (Exception(..), catch, throw)
import Control.Monad (forM, forM_, filterM, when, unless)
import Control.Monad.Extra (unlessM)
import Data.Bool
import Data.Bifunctor (bimap)
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
import Data.Functor ((<&>))
import Data.List (uncons, isPrefixOf, isSuffixOf, find, sortOn)
import Data.Maybe (fromMaybe, fromJust, isNothing)
import Data.Proxy (Proxy(..))
import Data.String (fromString)
import qualified Data.Map as Map (toList)
import qualified Data.Text as Text (unpack)
import qualified Data.Vector as Vector (fromList)
import qualified Data.HashMap.Strict as HashMap (fromList, size)
import qualified Data.Set as Set
  (fromList, toList, isProperSubsetOf, difference, member)
import GHC.IO.Exception (IOException(..), IOErrorType(..))
import System.Directory
  (listDirectory, doesDirectoryExist, createDirectoryIfMissing)
import System.Environment (getArgs, getProgName)
import System.Exit (die)
import System.FilePath
 ((</>), takeExtensions, takeBaseName, takeFileName, isExtensionOf)
import "bittide-extra" Numeric.Extra (parseHex)
import System.IO ( BufferMode(..), IOMode(..), Handle, openFile, hClose
                 , withFile, hSetBuffering, hPutStr, hFlush
                 )

import Bittide.Plot
import Bittide.ClockControl
import Bittide.ClockControl.StabilityChecker
import Bittide.Github.Artifacts
import Bittide.Hitl
import Bittide.Instances.Hitl.IlaPlot
import Bittide.Instances.Hitl.Setup
import Bittide.Instances.Hitl.Tests
import Bittide.Instances.Domains
import Bittide.Report.ClockControl
import Bittide.Simulate.Config (SimConf, simTopologyFileName, saveSimConfig)
import Bittide.Topology

import qualified Bittide.Simulate.Config as SimConf (SimConf(..))

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
    , dpGlobalLast :: GlobalTimestamp Basic125
      -- ^ the global time stamp of the previous scheduled capture
    , dpCycleDiff  :: Femtoseconds
      -- ^ the number of clock cycles since the last cheduled capture
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
    fmap (bimap BSC.pack BSC.pack) $
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

        cycleDiff       = case captureType of
          DataChange -> dpCycleDiff prevDP
          _ -> let (toInteger -> pL, toInteger -> cL) = dpGlobalLast prevDP
                   (toInteger -> pN, toInteger -> cN) = fromHex globalTimestamp
                in Femtoseconds $ fromInteger
                     $ (pN - pL) * natToInteger @(SyncPulseCycles Basic125)
                     + (cN - cL)

        knownClockDifference = Femtoseconds $ fromIntegral $ (1000 *)
          $ snatToInteger (clockPeriod @Basic125)
          - snatToInteger (clockPeriod @External)


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
                        + localStamp ~* clockPeriodFs (Proxy @External)

        globalTimeDelta = globalTime
                        - dpLastScheduledGlobalTime prevDP
        localTimeDelta  = localTime
                        - dpLastScheduledLocalTime prevDP

        driftPerCycle   = case captureType of
          DataChange -> dpDrift prevDP
          _          -> (globalTimeDelta - localTimeDelta) `div` cycleDiff
                      - knownClockDifference

        ccChanges       = dpCCChanges prevDP
                        + natToNum @AccWindowHeight * sign dSpeedChange

        dataCounts      = (\(x,_,_) y -> extend @_ @c @(k - c) x + y)
                            <$> dEBData
                            <*> dpDataCounts prevDP
     in DataPoint
          { dpIndex                   = sampleInBuffer
          , dpGlobalLast              =
              case captureType of
                DataChange -> dpGlobalLast prevDP
                _          -> fromHex globalTimestamp
          , dpCycleDiff               = cycleDiff
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
    , dpGlobalLast              = (0,0)
    , dpCycleDiff               = 0
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

-- | The HITL tests, whose post proc data offers a simulation config
-- for plotting.
knownTestsWithSimConf :: [(String, [(String, Maybe SimConf)])]
knownTestsWithSimConf = hasSimConf <$> hitlTests
 where
  hasSimConf = \case
    LoadConfig name _ -> (name, [])
    KnownType name test ->
      (name, first Text.unpack <$> Map.toList (mGetPPD @_ @SimConf test))

plotTest :: FilePath -> Maybe SimConf -> FilePath -> FilePath -> IO ()
plotTest testDir mCfg dir globalOutDir = do
  unless (isNothing mCfg) $ checkDependencies >>= maybe (return ()) die
  putStrLn $ "Creating plots for test case: " <> testName

  let
    knownId = flip Set.member $ Set.fromList $ Vec.toList
      $ Vec.imap (\i a -> show i <> "_" <> fst a) fpgaSetup
    topFromDirs = listDirectory dir
      >>= filterM (doesDirectoryExist . (dir </>))
      >>= return . fromJust . someNatVal . toInteger . length . filter knownId
      >>= \case
        SomeNat p ->
          return $ STop $ complete $ snatProxy p

  STop (t :: Topology n) <- case mCfg of
    Nothing  -> topFromDirs
    Just cfg -> case SimConf.mTopologyType cfg of
      Nothing          -> topFromDirs
      Just (Random {}) -> topFromDirs
      Just (DotFile f) -> readFile f >>= either die return . fromDot
      Just tt          -> fromTopologyType tt >>= either die return

  case TLW.SNat @n %<=? TLW.SNat @FpgaCount of
    LE Refl -> case TLW.SNat @1 %<=? TLW.SNat @n of
      LE Refl -> do
        let fpgas = Vec.toList $ Vec.imap (,)
                  $ Vec.take @n @(FpgaCount - n) SNat fpgaSetup

        postProcessData <- do
          forM fpgas $ \(i, (fpgaId, links)) -> concat . filter (not . null) <$> do
            let d = dir </> (show i <> "_" <> fpgaId)
            unlessM (doesDirectoryExist d) $ die $ "No directory: " <> d
            csvFiles <- checkCsvFilesExist d <$> listDirectory d
            forM csvFiles $ \f -> do
              h <- openFile f ReadMode
              rs <- catch
                (do rs <- runConduit $ fromCsvDump @FpgaCount @CccBufferSize (h, f)
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
                k = natToNum @(n - 1) :: Integer
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
                createDirectoryIfMissing True outDir
                BSL.writeFile (outDir </> (takeFileName d <> ".csv"))
                  $ encodeByName header rs

              return (toPlotData t i links <$> rs)

        createDirectoryIfMissing True outDir
        plot outDir t $ Vec.unsafeFromList postProcessData

        let allStable = and $ (\(_,_,_,xs) -> all (stable . snd) xs) . last
              <$> postProcessData

        case mCfg of
          Nothing -> return ()
          Just cfg' -> do
            let cfg = cfg' { SimConf.outDir = outDir
                           , SimConf.stable = Just allStable
                           }
                ids = bimap toInteger fst <$> fpgas
            case SimConf.mTopologyType cfg of
              Nothing -> writeTop Nothing
              Just (Random{}) -> writeTop Nothing
              Just (DotFile f) -> readFile f >>= writeTop . Just
              Just tt -> fromTopologyType tt >>= either die (`saveSimConfig` cfg)
            checkIntermediateResults outDir
              >>= maybe (generateReport "HITLT Report" outDir ids cfg) die
      _ -> die "Empty topology"
    _ -> die "Topology is larger than expected"
 where
  testName = takeFileName testDir
  outDir = globalOutDir </> testName

  checkCsvFilesExist d xs =
    let ys = filter ((== ".csv") . takeExtensions) $ fmap (d </>) xs
     in case ys of
       [] -> error $ d <> " does not contain any *.csv files. Aborting."
       _  -> ys

  toPlotData ::
    forall n m.
    (KnownNat n, 1 <= n, n <= FpgaCount) =>
    Topology n ->
    Index n ->
    Vec (FpgaCount - 1) (Index FpgaCount) ->
    DataPoint (FpgaCount - 1) m ->
    ( Femtoseconds
    , Femtoseconds
    , ReframingStage
    , [(DataCount m, StabilityIndication)]
    )
  toPlotData t i links DataPoint{..} =
    ( dpGlobalTime
    , dpDrift
    , dpRfStage
    , fmap snd
        $ sortOn fst
        $ filter (hasEdge t i . fst)
        $ fmap (first $ checkedTruncateB @n @(FpgaCount - n))
        $ filter ((<= natToNum @(n-1)) . fst)
        $ Vec.toList
        $ Vec.zip links
        $ Vec.zip dpDataCounts dpStability
    )

  writeTop (fromMaybe "digraph{}" -> str) =
    withFile (outDir </> simTopologyFileName) WriteMode $ \h -> do
      hSetBuffering h NoBuffering
      hPutStr h str
      hFlush h
      hClose h

main :: IO ()
main = getArgs >>= \case
  [] -> wrongNumberOfArguments
  plotDataSource : xr -> do
    (plotDataDir, outDir, mArtifactName) <- do
      isDir <- doesDirectoryExist plotDataSource
      (plotDataDir, yr, mA) <-
        if isDir
        then return (plotDataSource, xr, Nothing)
        else case isRunArtifactReference plotDataSource of
          Nothing -> die $ "Invalid argument: " <> plotDataSource
          Just (runId, artifactName) -> case xr of
            [] -> wrongNumberOfArguments
            dir : yr ->
              let fullArtifactName = "_build-" <> artifactName <> "-debug"
               in retrieveArtifact runId fullArtifactName dir >>= \case
                    Just err -> die $ unlines
                      [ "Cannot retrieve artifact."
                      , show err
                      ]
                    Nothing -> return (dir, yr, Just artifactName)
      let (outDir, zr) = fromMaybe (".", []) $ uncons yr
      unless (null zr) wrongNumberOfArguments
      return (plotDataDir, outDir, mA)

    tests <- do
      dirs <- listDirectory plotDataDir
      let hitlDir = plotDataDir </> "hitl"
      files <- bool
        (die $ "No 'hitl' folder in " <> fromMaybe plotDataDir mArtifactName)
        (listDirectory hitlDir)
        ("hitl" `elem` dirs)
      case filter (".yml" `isExtensionOf`) files of
        []  -> die $ "No YAML files in " <> hitlDir
        [x] -> return $ getTestsWithSimConf $ takeBaseName x
        _   -> die $ "Too many YAML files in " <> hitlDir

    (testDirs, testsDir) <- do
      let epsfix = maybe (Left "Bittide.Instances.Hitl.") Right mArtifactName
      dir <- diveDownInto epsfix plotDataDir
      listDirectory dir >>= filterM (doesDirectoryExist . (dir </>))
        <&> (, dir)

    let sDirs  = Set.fromList testDirs
        sNames = Set.fromList $ fst <$> tests
    when (sDirs /= sNames) $ die $
      if sDirs `Set.isProperSubsetOf` sNames
      then "Missing tests "
        <> show (Set.toList (sNames `Set.difference` sDirs))
        <> " in " <> testsDir
      else "Unknown tests "
        <> show (Set.toList (sDirs `Set.difference` sNames))
        <> " in " <> testsDir

    forM_ tests $ \(test, cfg) ->
      plotTest test cfg (testsDir </> test) outDir
 where
  getTestsWithSimConf name =
    maybe [] snd $ find ((== name) . fst) knownTestsWithSimConf

  diveDownInto epsfix dir =
    listDirectory dir
      >>= filterM doesDirectoryExist . fmap (dir </>)
      >>= \case
        [] -> die $ "Empty directory: " <> dir
        dirs -> let subDirs = takeFileName <$> dirs in if
          | "vivado" `elem` subDirs ->
              diveDownInto epsfix $ dir </> "vivado"
          | "ila-data" `elem` subDirs ->
              diveDownInto epsfix $ dir </> "ila-data"
          | otherwise ->
              case filter (either isPrefixOf isSuffixOf epsfix) subDirs of
                subDir : _ -> diveDownInto epsfix $ dir </> subDir
                _ -> return dir

  isRunArtifactReference arg = case span (/= ':') arg of
    (xs, ':':ys)
      | all isDigit xs && ':' `notElem` ys -> Just (xs, ys)
      | otherwise -> Nothing
    _ -> Nothing

  wrongNumberOfArguments = do
    name <- getProgName
    die $ "Wrong number of arguments. Aborting.\n\n"
       <> "Usage: " <> name <> " <ila plot directory> [<output directory>]"
