-- SPDX-FileCopyrightText: 2023 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=20 #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Extra.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}

module Main (main, knownTestsWithCcConf) where

import Clash.Prelude (
  BitPack (..),
  Index,
  KnownDomain,
  SNat (..),
  Vec (..),
  checkedTruncateB,
  extend,
  natToNum,
  snatProxy,
 )

import Clash.Signal.Internal (Femtoseconds (..))
import Clash.Sized.Vector qualified as Vec

import Data.Type.Equality ((:~:) (..))
import GHC.TypeLits hiding (SNat)
import GHC.TypeLits.Compare ((:<=?) (..))
import GHC.TypeLits.Witnesses ((%<=?))
import GHC.TypeLits.Witnesses qualified as TLW (SNat (..))

import Clash.Prelude qualified as C
import Conduit (
  ConduitT,
  Void,
  await,
  dropC,
  mapC,
  runConduit,
  scanlC,
  sinkList,
  sourceHandle,
  yield,
  (.|),
 )
import Control.Arrow (first)
import Control.Exception (Exception (..), catch, throw)
import Control.Monad (filterM, forM, forM_, unless, when)
import Control.Monad.Extra (ifM, unlessM)
import Data.Bifunctor (bimap)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Data.ByteString.Lazy qualified as BSL
import Data.ByteString.UTF8 qualified as UTF8
import Data.Csv (
  FromField (..),
  FromNamedRecord (..),
  ToNamedRecord (..),
  defaultDecodeOptions,
  encodeByName,
  (.:),
 )
import Data.Csv.Conduit (
  CsvStreamHaltParseError (..),
  CsvStreamRecordParseError (..),
  fromNamedCsvStreamError,
 )
import Data.HashMap.Strict qualified as HashMap
import Data.List (isSuffixOf, unzip4)
import Data.Map qualified as Map
import Data.Maybe (catMaybes, fromJust, fromMaybe, mapMaybe)
import Data.Proxy (Proxy (..))
import Data.Set qualified as Set
import Data.String (fromString)
import Data.Text qualified as Text
import Data.Typeable (cast)
import Data.Vector qualified as Vector
import GHC.IO.Exception (IOErrorType (..), IOException (..))
import GHC.Stack (HasCallStack)
import System.Directory (
  canonicalizePath,
  createDirectoryIfMissing,
  doesDirectoryExist,
  listDirectory,
 )
import System.Environment (getArgs, getProgName)
import System.Exit (die)
import System.FilePath (
  takeBaseName,
  takeExtensions,
  takeFileName,
  (</>),
 )
import System.IO (
  BufferMode (..),
  Handle,
  IOMode (..),
  hClose,
  hFlush,
  hPutStr,
  hSetBuffering,
  openFile,
  stderr,
  stdout,
  withFile,
 )
import Text.Read (readMaybe)
import "bittide-extra" Numeric.Extra (parseHex)

import Bittide.Arithmetic.PartsPer (PartsPer (..), cyclesToPartsPerI, ppm)
import Bittide.ClockControl
import Bittide.ClockControl.StabilityChecker
import Bittide.Github.Artifacts
import Bittide.Hitl
import Bittide.Instances.Domains
import Bittide.Instances.Hitl.IlaPlot
import Bittide.Instances.Hitl.Setup
import Bittide.Plot
import Bittide.Report.ClockControl
import Bittide.Simulate.Config (CcConf, saveCcConfig, simTopologyFileName)
import Bittide.Topology

import Bittide.Instances.Hitl.Tests (hitlTests)
import Bittide.Simulate.Config qualified as CcConf

-- A newtype wrapper for working with hex encoded types.
newtype Hex a = Hex {fromHex :: a}
  deriving newtype (BitPack)

instance (BitPack a) => FromField (Hex a) where
  parseField = either fail pure . parseHex . UTF8.toString

{- | The captured data entries, as they are dumped by the ILA of
'Bittide.Instances.Hitl.IlaPlot.callistoClockControlWithIla'. Fields marked with
an underscore are not used in the post processing.
-}
data Capture (nodeCount :: Nat) (compressedElasticBufferBits :: Nat) = Capture
  { sampleInBuffer :: Int
  , _sampleInWindow :: Int
  , _trigger :: Hex Bool
  , _triggerSignal :: Hex Bool
  , _capture :: Hex Bool
  , captureCond :: Hex CaptureCondition
  , globalTimestamp :: Hex (GlobalTimestamp Basic125)
  , localTimestamp :: Hex (DiffResult (LocalTimestamp GthTx))
  , plotData :: Hex (PlotData (nodeCount - 1) compressedElasticBufferBits)
  }

instance
  ( KnownNat nodeCount
  , KnownNat compressedElasticBufferBits
  , 1 <= nodeCount
  ) =>
  FromNamedRecord (Capture nodeCount compressedElasticBufferBits)
  where
  parseNamedRecord v =
    if HashMap.size v /= 3 + Vec.length ilaProbeNames
      then fail "Row with more than 8 fields"
      else
        Capture
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

    portName# :: (KnownNat n) => Vec n String -> Index n -> BS.ByteString
    portName# names = fromString . (names Vec.!!)

-- | A data point resulting from post processing the captured data.
data DataPoint (nodeCount :: Nat) (decompressedElasticBufferBits :: Nat) = DataPoint
  { dpIndex :: Int
  -- ^ the index of the corresponding sample of the dump
  , dpGlobalLast :: GlobalTimestamp Basic125
  -- ^ the global time stamp of the previous scheduled capture
  , dpGlobalTime :: Femtoseconds
  -- ^ the absolute number of femtoseconds since the start of the
  -- dump according to the global synchronized clock
  , dpDrift :: PartsPer
  -- ^ clock frequency difference in 'PartsPer'
  , dpCCChanges :: Int
  -- ^ the accumulated number FINC/FDECs integrated over time
  , dpRfStage :: ReframingStage
  -- ^ the reframing stage
  , dpDataCounts ::
      Vec
        nodeCount
        (Maybe (RelDataCount decompressedElasticBufferBits))
  -- ^ the elastic buffer data counts of the available links
  , dpStability :: Vec nodeCount (Maybe StabilityIndication)
  -- ^ the stability indicators for each of the elastic buffers
  -- of the available links
  }

instance
  ( KnownNat nodeCount
  , KnownNat decompressedElasticBufferBits
  , 1 <= nodeCount
  ) =>
  ToNamedRecord (DataPoint nodeCount decompressedElasticBufferBits)
  where
  toNamedRecord DataPoint{..} =
    HashMap.fromList $
      fmap (bimap BSC.pack BSC.pack) $
        [ ("Index", show dpIndex)
        , ("Synchronized Time (fs)", show $ toInteger dpGlobalTime)
        , ("Clock Period Drift (ppt)", case dpDrift of Ppt ppt -> show ppt)
        , ("Integrated FINC/FDECs", show dpCCChanges)
        , ("Reframing State", rf2bs dpRfStage)
        ]
          <> [ ("EB " <> show i, show $ toInteger x)
             | (i, x) <- topologyView dpDataCounts
             ]
          <> [ (show i <> " is stable", b2bs x.stable)
             | (i, x) <- topologyView dpStability
             ]
          <> [ (show i <> " is settled", b2bs x.settled)
             | (i, x) <- topologyView dpStability
             ]
   where
    topologyView =
      catMaybes
        . Vec.toList
        . fmap (\(i, x) -> (i,) <$> x)
        . Vec.zip Vec.indicesI

    b2bs = \case
      False -> "0"
      True -> "1"

    rf2bs = \case
      RSDetect -> "Detect"
      RSWait -> "Wait"
      RSDone -> "Done"

{- | Multiplies some 'Femtoseconds' with any numerical value. Note
that this operation can produce negative values, which is
intentional.
-}
infix 9 ~*

(~*) :: (Integral a) => a -> Femtoseconds -> Femtoseconds
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
  forall
    topologySize
    -- \^ the size of the topology underlying the data to be processed
    decompressedElasticBufferBits
    -- \^ the bitsize of the elastic buffer entries after decompression
    utilizedFpgaCount
    -- \^ the number of hardware nodes used to generated the data
    compressedElasticBufferBits
    -- \^ the bitsize of the elastic buffer entries before decompression
    anyMonad.
  ( KnownNat topologySize
  , KnownNat decompressedElasticBufferBits
  , KnownNat utilizedFpgaCount
  , KnownNat compressedElasticBufferBits
  , Monad anyMonad
  , compressedElasticBufferBits <= decompressedElasticBufferBits
  , 1 <= topologySize
  , 1 <= utilizedFpgaCount
  , topologySize <= utilizedFpgaCount
  ) =>
  Topology topologySize ->
  Index topologySize ->
  Vec (utilizedFpgaCount - 1) (Index utilizedFpgaCount) ->
  ConduitT
    (Capture utilizedFpgaCount compressedElasticBufferBits)
    (DataPoint topologySize decompressedElasticBufferBits)
    anyMonad
    ()
postProcess t i links =
  scanlC process initDummy
    -- finally drop the dummy, the trigger and the calibration entries
    .| (dropC 4 >> mapC id)
 where
  topologyView ::
    Vec (utilizedFpgaCount - 1) a ->
    Vec topologySize (Maybe a)
  topologyView =
    foldr (\(j, x) -> Vec.replace j $ Just x) (Vec.repeat Nothing)
      . filter ((.hasEdge) t i . fst)
      . fmap
        ( first $
            checkedTruncateB @topologySize @(utilizedFpgaCount - topologySize)
        )
      . filter ((<= natToNum @(topologySize - 1)) . fst)
      . Vec.toList
      . Vec.zip links

  process
    prevDP
    Capture
      { globalTimestamp
      , captureCond = fromHex -> captureType
      , localTimestamp = fromHex -> localTimestamp
      , sampleInBuffer
      , plotData = fromHex -> PlotData{dSpeedChange, dEBData, dRfStageChange}
      } =
      let
        globalTime = globalTsToFs $ fromHex globalTimestamp

        driftPartsPer :: PartsPer
        driftPartsPer
          | isScheduledCaptureCondition captureType =
              cyclesToPartsPerI
                (Proxy @GthTx)
                (Proxy @ScheduledCapturePeriod)
                ( case localTimestamp of
                    NoReference -> error $ "process: NoReference:" <> show sampleInBuffer
                    TooLarge -> error $ "process: TooLarge:" <> show sampleInBuffer
                    Difference x -> x + 1
                )
          | otherwise =
              dpDrift prevDP

        ccChanges =
          dpCCChanges prevDP
            + natToNum @AccWindowHeight * sign dSpeedChange

        dataCounts =
          ( \a b ->
              ( \(x, _, _) y ->
                  extend
                    @_
                    @compressedElasticBufferBits
                    @( decompressedElasticBufferBits
                        - compressedElasticBufferBits
                     )
                    x
                    + y
              )
                <$> a
                <*> b
          )
            <$> topologyView dEBData
            <*> dpDataCounts prevDP
       in
        DataPoint
          { dpIndex = sampleInBuffer
          , dpGlobalLast =
              case captureType of
                DataChange -> dpGlobalLast prevDP
                _ -> fromHex globalTimestamp
          , dpGlobalTime = globalTime
          , dpDrift = driftPartsPer
          , dpCCChanges = ccChanges
          , dpRfStage =
              case dRfStageChange of
                Stable -> dpRfStage prevDP
                ToDetect -> RSDetect
                ToWait -> RSWait
                ToDone -> RSDone
          , dpDataCounts = dataCounts
          , dpStability =
              let combine (Just (_, st, se)) (Just (StabilityIndication{..})) =
                    Just
                      StabilityIndication
                        { stable = fromMaybe stable st
                        , settled = fromMaybe settled se
                        }
                  combine _ _ = Nothing
               in Vec.zipWith
                    combine
                    (topologyView dEBData)
                    (dpStability prevDP)
          }

  initDummy =
    DataPoint
      { dpIndex = -1
      , dpGlobalLast = (0, 0)
      , dpGlobalTime = 0
      , dpDrift = 0
      , dpCCChanges = 0
      , dpRfStage = RSDetect
      , dpDataCounts = topologyView $ Vec.repeat 0
      , dpStability =
          topologyView $
            Vec.repeat
              StabilityIndication
                { stable = False
                , settled = False
                }
      }

  globalTsToFs :: GlobalTimestamp Basic125 -> Femtoseconds
  globalTsToFs (pulses, cycles) =
    (pulses ~* syncPulsePeriodFs)
      + (cycles ~* clockPeriodFs (Proxy @Basic125))
   where
    syncPulsePeriodFs = Femtoseconds $ natToNum @(1000 * SyncPulsePeriod)

fromCsvDump ::
  forall
    decompressedElasticBufferBits
    -- \^ the bitsize of the elastic buffer entries after decompression
    topologySize
    -- \^ the size of the topology underlying the data to be processed
    utilizedFpgaCount.
  -- \^ the number of hardware nodes used to generated the data

  ( KnownNat decompressedElasticBufferBits
  , CompressedBufferSize <= decompressedElasticBufferBits
  , KnownNat topologySize
  , KnownNat utilizedFpgaCount
  , 1 <= topologySize
  , 1 <= utilizedFpgaCount
  , topologySize <= utilizedFpgaCount
  ) =>
  Topology topologySize ->
  Index topologySize ->
  Vec (utilizedFpgaCount - 1) (Index utilizedFpgaCount) ->
  (Handle, FilePath) ->
  ConduitT () Void IO [DataPoint topologySize decompressedElasticBufferBits]
fromCsvDump t i links (csvHandle, csvFile) =
  -- turn the input file into a conduit source
  sourceHandle csvHandle
    -- plugin the CSV parser
    .| fromNamedCsvStreamError defaultDecodeOptions toIOE
    -- drop the first two header lines and ensure that the remaining
    -- data entries are valid
    .| (dropC 1 >> checkForErrors 0)
    -- post process the data
    .| postProcess
      @topologySize
      @decompressedElasticBufferBits
      @utilizedFpgaCount
      @CompressedBufferSize
      t
      i
      links
    -- return as a list
    .| sinkList
 where
  toIOE (HaltingCsvParseError _ msg) =
    IOError
      { ioe_handle = Just csvHandle
      , ioe_type = SystemError
      , ioe_location = csvFile
      , ioe_description = Text.unpack msg
      , ioe_errno = Nothing
      , ioe_filename = Just csvFile
      }

  checkForErrors n =
    await >>= \case
      Nothing -> return ()
      Just (Left e) -> throw $ CsvParseError n e
      Just (Right x) -> yield x >> checkForErrors (n + 1)

{- | The HITL tests, whose post proc data offers a simulation config
for plotting.
-}
knownTestsWithCcConf :: (HasCallStack) => Map.Map String [(String, CcConf)]
knownTestsWithCcConf = Map.fromList (mapMaybe go hitlTests)
 where
  justOrDie _ (Just x) = Just x
  justOrDie k Nothing = error $ "No CcConf for " <> show k

  go HitlTestGroup{topEntity, testCases = iters :: [HitlTestCase HwTargetRef q r]} =
    case cast @[HitlTestCase HwTargetRef q r] @[HitlTestCase HwTargetRef q CcConf] iters of
      Just q ->
        Just
          ( show topEntity
          , Map.toList (Map.mapMaybeWithKey justOrDie (mGetPPD @CcConf @HwTargetRef q))
          )
      Nothing -> Nothing

{- | Calculate an offset such that the clocks start at their set offsets. That is
to say, we consider the reference clock to be at 0 fs by definition. The offsets
of the other clocks are then measured relative to this reference clock. We don't
particularly care about the reference clocks in our plots however, it is much more
useful to shift it in such a way that the other clocks start at their set offsets.
This concept is explained visually in: https://github.com/bittide/bittide-hardware/issues/607.

This function will return an error ('Left') if the clocks cannot be shifted in
such a way that they all start at their set offsets. If this happens, something
is seriously wrong.
-}
getOffsetCorrection ::
  (KnownNat nNodes) =>
  -- | Post processed data - one per FPGA
  --
  -- TODO: Nicer data structure
  Vec nNodes [(a, PartsPer {- relative offset -}, b, c)] ->
  -- | The desired offsets for the clocks
  Vec nNodes PartsPer ->
  -- | Correction, if a sensible one can be found
  IO (Either String PartsPer)
getOffsetCorrection postProcessData desiredOffsets = do
  -- Gather first sampled offset for each node
  measuredOffsets <- forM (C.zip C.indicesI postProcessData) $
    \(nodeNo, unzip4 -> (_, offsets, _, _)) -> do
      case offsets of
        [] -> die $ "No offsets for node " <> show nodeNo
        (offset : _) -> pure offset

  let zippedOffsets = C.zip measuredOffsets desiredOffsets

  case zippedOffsets of
    Nil -> die "No offsets, nNodes ~ 0?"
    (measuredOffset0, desiredOffset0) `Cons` _ -> pure $ do
      let correction = desiredOffset0 - measuredOffset0
      forM_ zippedOffsets $ \(measuredOffset, desiredOffset) -> do
        when (abs (measuredOffset + correction - desiredOffset) >= ppm 5) $
          Left $
            unlines
              [ "Clocks did not start at their set offsets."
              , ""
              , "Measured offsets:  " <> show measuredOffsets
              , "Desired offsets:   " <> show desiredOffsets
              , "Corrected offsets: " <> show (((+) correction) <$> measuredOffsets)
              , "Correction:        " <> show correction
              ]
        pure ()
      pure correction

plotTest ::
  (KnownDomain refDom) =>
  Proxy refDom ->
  FilePath ->
  CcConf ->
  FilePath ->
  FilePath ->
  IO ()
plotTest refDom testDir cfg dir globalOutDir = do
  checkDependencies >>= maybe (return ()) die
  putStrLn $ "Creating plots for test case: " <> testName

  let
    knownId =
      flip Set.member $
        Set.fromList $
          Vec.toList $
            Vec.imap (\i a -> show i <> "_" <> fst a) fpgaSetup
    topFromDirs =
      listDirectory dir
        >>= filterM (doesDirectoryExist . (dir </>))
        >>= return . fromJust . someNatVal . toInteger . length . filter knownId
        >>= \case
          SomeNat n -> return $ STop $ complete $ snatProxy n

  STop (t :: Topology topologySize) <-
    case cfg.ccTopologyType of
      Random{} -> topFromDirs
      DotFile f -> readFile f >>= either die return . fromDot
      tt -> froccTopologyType tt >>= either die return

  case TLW.SNat @topologySize %<=? TLW.SNat @FpgaCount of
    LE Refl -> case TLW.SNat @1 %<=? TLW.SNat @topologySize of
      LE Refl -> do
        let fpgas =
              Vec.toList $
                Vec.imap (,) $
                  Vec.take @topologySize @(FpgaCount - topologySize)
                    SNat
                    fpgaSetup

        postProcessData <- do
          forM fpgas $ \(i, (fpgaId, links)) ->
            concat . filter (not . null) <$> do
              let d = dir </> (show i <> "_" <> fpgaId)
              unlessM (doesDirectoryExist d) $ die $ "No directory: " <> d
              csvFiles <- checkCsvFilesExist d <$> listDirectory d
              forM csvFiles $ \f -> do
                h <- openFile f ReadMode
                rs <- catch
                  ( do
                      rs <- runConduit $ fromCsvDump @CccBufferSize t i links (h, f)
                      putStrLn ("Using " <> (takeBaseName d </> takeFileName f))
                      return rs
                  )
                  $ \(err :: CsvParseError) -> case err of
                    -- Ignore additional CSV files that may have
                    -- been produced by other ILAs. They are
                    -- identified via an error while parsing the
                    -- header row.
                    CsvParseError 0 _ -> return []
                    CsvParseError n (CsvStreamRecordParseError msg) ->
                      error $
                        unlines
                          [ "Error while parsing"
                          , ""
                          , "  " <> f
                          , ""
                          , "Line " <> show (n + 3) <> ", " <> Text.unpack msg
                          ]
                hClose h

                let
                  ls = show <$> filter ((.hasEdge) t i) (Vec.toList Vec.indicesI)
                  header =
                    Vector.fromList $
                      map BSC.pack $
                        [ "Index"
                        , "Synchronized Time (fs)"
                        , "Clock Period Drift (ppt)"
                        , "Integrated FINC/FDECs"
                        , "Reframing State"
                        ]
                          <> (("EB " <>) <$> ls)
                          <> ((<> " is stable") <$> ls)
                          <> ((<> " is settled") <$> ls)

                unless (null rs) $ do
                  createDirectoryIfMissing True outDir
                  BSL.writeFile (outDir </> (takeFileName d <> ".csv")) $
                    encodeByName header rs

                return (toPlotData <$> rs)

        let postProcessDataVec = Vec.unsafeFromList postProcessData

        -- Calculate offset correction for readability purposes. See:
        -- https://github.com/bittide/bittide-hardware/issues/607
        (maybeError, maybeOffsetCorrection) <-
          case cfg.clockOffsets of
            Nothing -> pure (Nothing, Nothing)
            Just (Vec.unsafeFromList -> offsets) ->
              getOffsetCorrection postProcessDataVec offsets >>= \case
                Left err -> pure (Just err, Nothing)
                Right correction -> pure (Nothing, Just correction)

        createDirectoryIfMissing True outDir
        plot maybeOffsetCorrection outDir t postProcessDataVec

        let
          allStable =
            all ((\(_, _, _, xs) -> all ((.stable) . snd) xs) . last) postProcessData
          cfg1 =
            cfg
              { CcConf.outDir = outDir
              , CcConf.stable = Just allStable
              }
          ids = bimap toInteger fst <$> fpgas

        case cfg.ccTopologyType of
          Random{} -> writeTop Nothing
          DotFile f -> readFile f >>= writeTop . Just
          tt -> froccTopologyType tt >>= either die (`saveCcConfig` cfg1)
        checkIntermediateResults outDir
          >>= maybe (generateReport refDom "HITLT Report" outDir ids cfg1) die

        -- Fail if clocks did not start at their set offsets. We purposely fail
        -- after generating the report, because the report generation is very
        -- useful for debugging.
        hFlush stdout
        hFlush stderr
        maybe (return ()) die maybeError
      _ -> die "Empty topology"
    _ -> die "Topology is larger than expected"
 where
  testName = takeFileName testDir
  outDir = globalOutDir </> testName

  checkCsvFilesExist d xs =
    let ys = filter ((== ".csv") . takeExtensions) $ fmap (d </>) xs
     in case ys of
          [] -> error $ d <> " does not contain any *.csv files. Aborting."
          _ -> ys

  toPlotData ::
    DataPoint n decompressedElasticBufferBits ->
    ( Femtoseconds
    , PartsPer
    , ReframingStage
    , [ ( RelDataCount decompressedElasticBufferBits
        , StabilityIndication
        )
      ]
    )
  toPlotData DataPoint{..} =
    ( dpGlobalTime
    , dpDrift
    , dpRfStage
    , mapMaybe (uncurry $ liftA2 (,)) $
        Vec.toList $
          Vec.zip dpDataCounts dpStability
    )

  writeTop (fromMaybe "digraph{}" -> str) =
    withFile (outDir </> simTopologyFileName) WriteMode $ \h -> do
      hSetBuffering h NoBuffering
      hPutStr h str
      hFlush h
      hClose h

{- | Try to parse a run artifact reference.

>>> parseArtifactRef "123:build-debug"
Just (123,"build-debug")
>>> parseArtifactRef "123_my_dir"
Nothing
-}
parseArtifactRef :: String -> Maybe (Int, String)
parseArtifactRef arg = case span (/= ':') arg of
  (readMaybe -> Just jobId, _ : jobName) -> Just (jobId, jobName)
  _ -> Nothing

{- | Given either a Github artifact reference (see 'parseArtifactRef') or a local
directory, return the fully qualified test name and the directory containing
a folder called \"ila-data\".
-}
getSourceData :: String -> IO (String, FilePath)
getSourceData artifactRef | Just (jobId, jobName) <- parseArtifactRef artifactRef = do
  -- Get artifact from Github
  let fullArtifactName = "_build-" <> jobName <> "-debug"
  artifactResult <- retrieveArtifact (show jobId) fullArtifactName ("_build" </> "plot")
  case artifactResult of
    Just err -> die (unlines ["Cannot retrieve artifact.", show err])
    Nothing -> do
      let vivadoDir = "_build" </> "plot" </> "vivado"
      dirs <- listDirectory vivadoDir
      case filter (('.' : jobName) `isSuffixOf`) dirs of
        [dir] -> getSourceData (vivadoDir </> dir)
        _ ->
          die $ "No or multiple directories with name containing " <> jobName <> " in " <> vivadoDir
getSourceData dir = do
  -- Get artifact from local directory
  let ilaDataDir = dir </> "ila-data"
  fullyQualifiedTestName <- takeFileName <$> canonicalizePath dir
  ifM
    (doesDirectoryExist ilaDataDir)
    (return (fullyQualifiedTestName, ilaDataDir))
    (die $ "No 'ila-data' directory in " <> dir)

main :: IO ()
main =
  getArgs >>= \case
    [plotDataSource, outputDir] -> do
      (fullyQualifiedTestName, plotDataDir) <- getSourceData plotDataSource
      ccConfs <- case Map.lookup fullyQualifiedTestName knownTestsWithCcConf of
        Nothing -> die $ "Could not find test config: " <> fullyQualifiedTestName
        Just ccConfs -> pure ccConfs

      forM_ ccConfs $ \(testName, ccConf) -> do
        plotTest (Proxy @Basic125) testName ccConf (plotDataDir </> testName) outputDir
    _ -> wrongNumberOfArguments
 where
  wrongNumberOfArguments = do
    name <- getProgName
    die $
      "Wrong number of arguments. Aborting.\n\n"
        <> "Usage: "
        <> name
        <> " <ila plot directory> <output directory>"
