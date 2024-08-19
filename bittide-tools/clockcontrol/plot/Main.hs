-- SPDX-FileCopyrightText: 2023 Google LLC
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
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=20 #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Extra.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}

module Main (main, knownTestsWithSimConf) where

import Clash.Prelude (
  BitPack (..),
  Index,
  KnownDomain,
  SNat (..),
  Vec,
  checkedTruncateB,
  clockPeriod,
  extend,
  natToInteger,
  natToNum,
  snatProxy,
  snatToInteger,
 )

import Clash.Signal.Internal (Femtoseconds (..))
import Clash.Sized.Vector qualified as Vec (
  imap,
  indicesI,
  length,
  repeat,
  replace,
  take,
  toList,
  unsafeFromList,
  zip,
  zipWith,
  (!!),
 )

import Data.Type.Equality ((:~:) (..))
import GHC.TypeLits
import GHC.TypeLits.Compare ((:<=?) (..))
import GHC.TypeLits.Witnesses ((%<=?))
import GHC.TypeLits.Witnesses qualified as TLW (SNat (..))

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
import Control.Applicative (liftA2)
import Control.Arrow (first)
import Control.Exception (Exception (..), catch, throw)
import Control.Monad (filterM, forM, forM_, unless, when)
import Control.Monad.Extra (unlessM)
import Data.Bifunctor (bimap)
import Data.Bool
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Data.ByteString.Lazy qualified as BSL
import Data.ByteString.UTF8 qualified as UTF8
import Data.Char (isDigit)
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
import Data.Functor ((<&>))
import Data.HashMap.Strict qualified as HashMap (fromList, size)
import Data.List (find, isPrefixOf, isSuffixOf, uncons)
import Data.Map qualified as Map
import Data.Maybe (catMaybes, fromJust, fromMaybe, mapMaybe)
import Data.Proxy (Proxy (..))
import Data.Set qualified as Set (
  difference,
  fromList,
  isProperSubsetOf,
  member,
  toList,
 )
import Data.String (fromString)
import Data.Text qualified as Text (unpack)
import Data.Vector qualified as Vector (fromList)
import GHC.IO.Exception (IOErrorType (..), IOException (..))
import GHC.Stack (HasCallStack)
import System.Directory (
  createDirectoryIfMissing,
  doesDirectoryExist,
  listDirectory,
 )
import System.Environment (getArgs, getProgName)
import System.Exit (die)
import System.FilePath (
  isExtensionOf,
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
  withFile,
 )
import "bittide-extra" Numeric.Extra (parseHex)

import Bittide.ClockControl
import Bittide.ClockControl.StabilityChecker
import Bittide.Github.Artifacts
import Bittide.Hitl
import Bittide.Instances.Domains
import Bittide.Instances.Hitl.IlaPlot
import Bittide.Instances.Hitl.Setup
import Bittide.Instances.Hitl.Tests
import Bittide.Plot
import Bittide.Report.ClockControl
import Bittide.Simulate.Config (SimConf, saveSimConfig, simTopologyFileName)
import Bittide.Topology

import Bittide.Simulate.Config qualified as SimConf (SimConf (..))

-- A newtype wrapper for working with hex encoded types.
newtype Hex a = Hex {fromHex :: a}
  deriving newtype (BitPack)

instance (BitPack a) => FromField (Hex a) where
  parseField = either fail pure . parseHex . UTF8.toString

{- | The captured data entries, as they are dumped by the ILA of
'Bittide.Instances.Hitl.IlaPlot.callistoClockControlWithIla'.
-}
data Capture (nodeCount :: Nat) (compressedElasticBufferBits :: Nat) = Capture
  { sampleInBuffer :: Int
  , sampleInWindow :: Int
  , trigger :: Hex Bool
  , triggerSignal :: Hex Bool
  , capture :: Hex Bool
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
  , dpCycleDiff :: Femtoseconds
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
        , ("Local Clock time (fs)", show $ toInteger dpLocalTime)
        , ("Clock Period Drift (fs)", show $ toInteger dpDrift)
        , ("Integrated FINC/FDECs", show dpCCChanges)
        , ("Reframing State", rf2bs dpRfStage)
        ]
          <> [ ("EB " <> show i, show $ toInteger x)
             | (i, x) <- topologyView dpDataCounts
             ]
          <> [ (show i <> " is stable", b2bs $ stable x)
             | (i, x) <- topologyView dpStability
             ]
          <> [ (show i <> " is settled", b2bs $ settled x)
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
      . filter (hasEdge t i . fst)
      . fmap
        ( first $
            checkedTruncateB @topologySize @(utilizedFpgaCount - topologySize)
        )
      . filter ((<= natToNum @(topologySize - 1)) . fst)
      . Vec.toList
      . Vec.zip links

  process prevDP Capture{..} =
    let PlotData{..} = fromHex plotData
        captureType = fromHex captureCond

        cycleDiff = case captureType of
          DataChange -> dpCycleDiff prevDP
          _ ->
            let (toInteger -> pL, toInteger -> cL) = dpGlobalLast prevDP
                (toInteger -> pN, toInteger -> cN) = fromHex globalTimestamp
             in Femtoseconds $
                  fromInteger $
                    (pN - pL) * natToInteger @(SyncPulseCycles Basic125)
                      + (cN - cL)

        knownClockDifference =
          Femtoseconds $
            fromIntegral $
              (1000 *) $
                snatToInteger (clockPeriod @Basic125)
                  - snatToInteger (clockPeriod @GthTx)

        localStamp =
          let ref = "[" <> show sampleInBuffer <> "]"
           in case fromHex localTimestamp of
                NoReference -> error $ "LT: no reference " <> ref
                TooLarge -> error $ "LT: too large " <> ref
                Difference x -> x + 1

        globalTime = globalTsToFs $ fromHex globalTimestamp
        localTime = case captureType of
          UntilTrigger -> globalTime
          _ ->
            dpLocalTime prevDP
              + localStamp ~* clockPeriodFs (Proxy @GthTx)

        globalTimeDelta =
          globalTime
            - dpLastScheduledGlobalTime prevDP
        localTimeDelta =
          localTime
            - dpLastScheduledLocalTime prevDP

        driftPerCycle = case captureType of
          DataChange -> dpDrift prevDP
          _ ->
            (globalTimeDelta - localTimeDelta) `div` cycleDiff
              - knownClockDifference

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
     in DataPoint
          { dpIndex = sampleInBuffer
          , dpGlobalLast =
              case captureType of
                DataChange -> dpGlobalLast prevDP
                _ -> fromHex globalTimestamp
          , dpCycleDiff = cycleDiff
          , dpGlobalTime = globalTime
          , dpLastScheduledGlobalTime =
              case captureType of
                DataChange -> dpLastScheduledGlobalTime prevDP
                _ -> globalTime
          , dpLocalTime = localTime
          , dpLastScheduledLocalTime =
              case captureType of
                DataChange -> dpLastScheduledLocalTime prevDP
                _ -> localTime
          , dpDrift = driftPerCycle
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
      , dpCycleDiff = 0
      , dpGlobalTime = 0
      , dpLastScheduledGlobalTime = 0
      , dpLocalTime = 0
      , dpLastScheduledLocalTime = 0
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
knownTestsWithSimConf :: (HasCallStack) => [(String, [(String, SimConf)])]
knownTestsWithSimConf = hasSimConf <$> hitlTests
 where
  hasSimConf = \case
    LoadConfig name _ -> (name, [])
    KnownType name test ->
      let !simConfMap = Map.mapMaybeWithKey justOrDie (mGetPPD @_ @SimConf test)
       in (name, first Text.unpack <$> Map.toList simConfMap)

  justOrDie _ (Just x) = Just x
  justOrDie k Nothing = error $ "No SimConf for " <> show k

plotTest ::
  (KnownDomain refDom) =>
  Proxy refDom ->
  FilePath ->
  SimConf ->
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
    case SimConf.mTopologyType cfg of
      Nothing -> topFromDirs
      Just (Random{}) -> topFromDirs
      Just (DotFile f) -> readFile f >>= either die return . fromDot
      Just tt -> fromTopologyType tt >>= either die return

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
                  ls = show <$> filter (hasEdge t i) (Vec.toList Vec.indicesI)
                  header =
                    Vector.fromList $
                      map BSC.pack $
                        [ "Index"
                        , "Synchronized Time (fs)"
                        , "Local Clock time (fs)"
                        , "Clock Period Drift (fs)"
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

        createDirectoryIfMissing True outDir
        plot refDom outDir t $ Vec.unsafeFromList postProcessData

        let
          allStable =
            all ((\(_, _, _, xs) -> all (stable . snd) xs) . last) postProcessData
          cfg1 =
            cfg
              { SimConf.outDir = outDir
              , SimConf.stable = Just allStable
              }
          ids = bimap toInteger fst <$> fpgas

        case SimConf.mTopologyType cfg of
          Nothing -> writeTop Nothing
          Just (Random{}) -> writeTop Nothing
          Just (DotFile f) -> readFile f >>= writeTop . Just
          Just tt -> fromTopologyType tt >>= either die (`saveSimConfig` cfg1)
        checkIntermediateResults outDir
          >>= maybe (generateReport (Proxy @Basic125) "HITLT Report" outDir ids cfg1) die
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

main :: IO ()
main =
  getArgs >>= \case
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
                        Just err ->
                          die $
                            unlines
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
        files <-
          bool
            (die $ "No 'hitl' folder in " <> fromMaybe plotDataDir mArtifactName)
            (listDirectory hitlDir)
            ("hitl" `elem` dirs)
        case filter (".yml" `isExtensionOf`) files of
          [] -> die $ "No YAML files in " <> hitlDir
          [x] -> return $ getTestsWithSimConf $ takeBaseName x
          _ -> die $ "Too many YAML files in " <> hitlDir

      (testDirs, testsDir) <- do
        let epsfix = maybe (Left "Bittide.Instances.Hitl.") Right mArtifactName
        dir <- diveDownInto epsfix plotDataDir
        listDirectory dir
          >>= filterM (doesDirectoryExist . (dir </>))
          <&> (,dir)

      let sDirs = Set.fromList testDirs
          sNames = Set.fromList $ fst <$> tests
      when (sDirs /= sNames) $
        die $
          if sDirs `Set.isProperSubsetOf` sNames
            then
              "Missing tests "
                <> show (Set.toList (sNames `Set.difference` sDirs))
                <> " in "
                <> testsDir
            else
              "Unknown tests "
                <> show (Set.toList (sDirs `Set.difference` sNames))
                <> " in "
                <> testsDir

      forM_ tests $ \(test, cfg) ->
        plotTest (Proxy @Basic125) test cfg (testsDir </> test) outDir
 where
  getTestsWithSimConf name =
    maybe [] snd $ find ((== name) . fst) knownTestsWithSimConf

  diveDownInto epsfix dir =
    listDirectory dir
      >>= filterM doesDirectoryExist . fmap (dir </>)
      >>= \case
        [] -> die $ "Empty directory: " <> dir
        dirs ->
          let subDirs = takeFileName <$> dirs
           in if
                | "vivado" `elem` subDirs ->
                    diveDownInto epsfix $ dir </> "vivado"
                | "ila-data" `elem` subDirs ->
                    diveDownInto epsfix $ dir </> "ila-data"
                | otherwise ->
                    case filter (either isPrefixOf isSuffixOf epsfix) subDirs of
                      subDir : _ -> diveDownInto epsfix $ dir </> subDir
                      _ -> return dir

  isRunArtifactReference arg = case span (/= ':') arg of
    (xs, ':' : ys)
      | all isDigit xs && ':' `notElem` ys -> Just (xs, ys)
      | otherwise -> Nothing
    _ -> Nothing

  wrongNumberOfArguments = do
    name <- getProgName
    die $
      "Wrong number of arguments. Aborting.\n\n"
        <> "Usage: "
        <> name
        <> " <ila plot directory> [<output directory>]"
