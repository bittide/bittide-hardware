{-# LANGUAGE RecordWildCards #-}
-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE ImplicitPrelude #-}

module Bittide.Simulate.Config (
  SimConf (..),
  simJsonConfigFileName,
  simTopologyFileName,
  simConfigCLIParser,
  saveSimConfig,
) where

import Bittide.Simulate (OutputMode (..))
import Bittide.Topology (STop (..), TopologyType (..), toDot, topTypeCLIParser)

import Data.Aeson (FromJSON (..), ToJSON (..), encode)
import Data.ByteString.Lazy qualified as BS (writeFile)
import Data.Default (Default (..))
import GHC.Generics (Generic)
import Language.Dot.Pretty (render)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))

import Options.Applicative

-- | Default name of the simulation JSON configuration file.
simJsonConfigFileName :: String
simJsonConfigFileName = "simulate.json"

-- | Default name of the simulation topology Graphviz file.
simTopologyFileName :: String
simTopologyFileName = "topology.gv"

-- | Collection of all simulation configuration parameters.
data SimConf = SimConf
  { mTopologyType :: Maybe (TopologyType IO Integer)
  -- ^ The topology type of the network to be simulated. Have a
  -- look at 'Bittide.Topology' for more insights on the supported
  -- topology types and their corresponding topologies.
  , outMode :: OutputMode
  -- ^ Some selector of how the data of the simulation result is
  -- returned for furhter processing.
  , duration :: Int
  -- ^ The number of clock cycles to simulate.
  , samples :: Int
  -- ^ The number of samples to be utilized for result
  -- generation. From the 'duration' many samples available, only
  -- every @duration `quot` samples@th sample is used.
  , stabilityMargin :: Int
  -- ^ Maximum number of elements a buffer occupancy is allowed to
  -- deviate to be considered stable.
  -- (cf. 'Bittide.ClockControl.StabilityChecker')
  , stabilityFrameSize :: Int
  -- ^ The minimum number of clock cycles a buffer occupancy must
  -- remain within to be considered stable.
  -- (cf. 'Bittide.ClockControl.StabilityChecker')
  , reframe :: Bool
  -- ^ Some flag for enabeling or disabling reframing.
  , rusty :: Bool
  -- ^ Some flag for enabeling or disabling the simulation of
  -- clock control via the Rust FFI.
  , waitTime :: Int
  -- ^ Number of clock cycles to wait until reframing takes place
  -- (after stability has been detected, for all elastic buffers).
  , stopWhenStable :: Bool
  -- ^ Stop simulation as soon as all buffers get stable.
  , stopAfterStable :: Maybe Int
  -- ^ Stop simulation after all buffers have been stable for
  -- at least the given number of clock cycles.
  , clockOffsets :: [Float]
  -- ^ The initital clock offsets in Femtoseconds
  -- (randomly generated if missing).
  , startupDelays :: [Int]
  -- ^ The Initital startup offsets, i.e, the number of clock
  -- cycles to wait before a node gets started (according to the
  -- node's individual clock, randomly generated if missing).
  , maxStartupDelay :: Int
  -- ^ Maximal number of clock cycles the startup of a node may be
  -- delayed (bounds the randomly generated offsets)".
  , createReport :: Bool
  -- ^ Some flag for enabling or disabling report generation.
  , outDir :: FilePath
  -- ^ The directory, in which the generated files are stored.
  , jsonArgs :: Maybe FilePath
  -- ^ Read arguments from a 'simulate.json' file, if given.
  , stable :: Maybe Bool
  -- ^ Stability result of the elastic buffers at the end of
  -- simulation, if available.
  }
  deriving (Show, Ord, Eq, Generic, ToJSON, FromJSON)

instance Default SimConf where
  def =
    SimConf
      { mTopologyType = Nothing
      , outMode = PDF
      , duration = 150000
      , samples = 100
      , stabilityMargin = 8
      , stabilityFrameSize = 1500000
      , reframe = True
      , rusty = False
      , waitTime = 100000
      , stopWhenStable = False
      , stopAfterStable = Nothing
      , clockOffsets = []
      , startupDelays = []
      , maxStartupDelay = 0
      , createReport = False
      , outDir = "_build"
      , jsonArgs = Nothing
      , stable = Nothing
      }

-- | Command line parser for reading a simulation configuration.
simConfigCLIParser :: Parser SimConf
simConfigCLIParser =
  SimConf
    <$> optional topTypeCLIParser
    <*> option
      auto
      ( long "output-mode"
          <> short 'm'
          <> metavar "MODE"
          <> value (outMode def)
          <> showDefault
          <> help "Available modes are: csv, pdf"
      )
    <*> option
      auto
      ( long "steps"
          <> short 's'
          <> metavar "NUM"
          <> value (duration def)
          <> showDefault
          <> help "Number of clock cycles to simulate"
      )
    <*> option
      auto
      ( long "samples"
          <> short 'a'
          <> metavar "NUM"
          <> value (samples def)
          <> showDefault
          <> help "Number of samples to keep & pass to matplotlib"
      )
    <*> option
      auto
      ( long "margin"
          <> short 'g'
          <> metavar "NUM"
          <> value (stabilityMargin def)
          <> showDefault
          <> help
            ( "Maximum number of elements a buffer occupancy is "
                <> "allowed to deviate to be considered stable"
            )
      )
    <*> option
      auto
      ( long "frame-size"
          <> short 'f'
          <> metavar "NUM"
          <> value (stabilityFrameSize def)
          <> showDefault
          <> help
            ( "Minimum number of clock cycles a buffer occupancy "
                <> "must remain within to be considered stable"
            )
      )
    <*> flag
      (reframe def)
      False
      ( long "disable-reframing"
          <> short 'e'
          <> help "Disables clock control reframing"
      )
    <*> flag
      (rusty def)
      (not $ rusty def)
      ( long "get-rusty"
          <> short 'y'
          <> help "Simulate clock control via the Rust FFI"
      )
    <*> option
      auto
      ( long "wait-time"
          <> short 'w'
          <> metavar "NUM"
          <> value (waitTime def)
          <> showDefault
          <> help
            ( "Number of clock cycles to wait until reframing takes place "
                <> "(after stability has been detected, for all elastic buffers)"
            )
      )
    <*> flag
      (stopWhenStable def)
      (not $ stopWhenStable def)
      ( long "stop-when-stable"
          <> short 'x'
          <> help "Stop simulation as soon as all buffers get stable"
      )
    <*> optional
      ( option
          auto
          ( long "stop-after-stable"
              <> short 'X'
              <> metavar "NUM"
              <> help
                ( "Stop simulation after all buffers have been stable for "
                    <> " at least the given number of simulation steps"
                )
          )
      )
    <*> option
      auto
      ( long "clock-offsets"
          <> short 't'
          <> metavar "NUM LIST"
          <> value (clockOffsets def)
          <> showDefault
          <> help "Initital clock offsets (randomly generated if missing)"
      )
    <*> option
      auto
      ( long "startup-delays"
          <> short 'T'
          <> metavar "NUM LIST"
          <> value (startupDelays def)
          <> showDefault
          <> help
            ( "Initital startup offsets, i.e, the number of clock cycles "
                <> "to wait before a node gets started (according to the "
                <> "node's individual clock, randomly generated if missing)"
            )
      )
    <*> option
      auto
      ( long "max-startup-delay"
          <> short 'u'
          <> metavar "NUM"
          <> value (maxStartupDelay def)
          <> showDefault
          <> help
            ( "Maximal number of clock cycles the startup of a node may be "
                <> "delayed (bounds the randomly generated offsets)"
            )
      )
    <*> flag
      (createReport def)
      (not $ createReport def)
      ( long "create-report"
          <> short 'r'
          <> help "Create a simulation report"
      )
    <*> strOption
      ( long "output-directory"
          <> short 'o'
          <> metavar "DIR"
          <> action "directory"
          <> value (outDir def)
          <> showDefault
          <> help "Directory, in which the generated files are stored"
      )
    <*> optional
      ( strOption
          ( long "json-args"
              <> short 'j'
              <> metavar "FILE"
              <> action "file"
              <> help
                ( "Read arguments from a 'simulate.json' file "
                    <> "(overwrites all arguments other than '-jz')"
                )
          )
      )
    <*> pure Nothing

{- | Saves a topology and a corresponding simulation configuration to
respective files in 'outDir'.
-}
saveSimConfig :: STop -> SimConf -> IO ()
saveSimConfig (STop t) cfg@SimConf{..} = do
  createDirectoryIfMissing True outDir
  let topologyFile = outDir </> simTopologyFileName
  writeFile topologyFile $ (<> "\n") $ render $ toDot t
  BS.writeFile (outDir </> simJsonConfigFileName) $
    encode
      cfg
        { jsonArgs = Nothing
        , mTopologyType = case mTopologyType of
            Just (Random{}) -> Just $ DotFile topologyFile
            _ -> mTopologyType
        }
