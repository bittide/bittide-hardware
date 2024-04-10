-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE ImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
module Bittide.Simulate.Config
  ( SimConf(..)
  , simJsonConfigFileName
  , simTopologyFileName
  , simConfigCLIParser
  , saveSimConfig
  ) where

import Bittide.Simulate (OutputMode(..))
import Bittide.Topology (TopologyType(..), STop(..), toDot, topTypeCLIParser)

import Data.Aeson (ToJSON(..), FromJSON(..), encode)
import Data.ByteString.Lazy qualified as BS (writeFile)
import Data.Default (Default(..))
import GHC.Generics (Generic)
import GHC.Int (Int64)
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

-- | Collection of all simulation configuration parameters. Have a
-- look at the implementation of 'simConfigCLIParser' for a
-- description of the individual fields.
data SimConf =
  SimConf
    { mTopologyType      :: Maybe (TopologyType IO Integer)
    , outMode            :: OutputMode
    , simulationSteps    :: Int
    , simulationSamples  :: Int
    , stabilityMargin    :: Int
    , stabilityFrameSize :: Int
    , disableReframing   :: Bool
    , rusty              :: Bool
    , waitTime           :: Int
    , stopWhenStable     :: Bool
    , stopAfterStable    :: Maybe Int
    , clockOffsets       :: [Int64]
    , startupOffsets     :: [Int]
    , maxStartupOffset   :: Int
    , createReport       :: Bool
    , outDir             :: FilePath
    , jsonArgs           :: Maybe FilePath
    , stable             :: Maybe Bool
    }
  deriving (Show, Ord, Eq, Generic, ToJSON, FromJSON)

instance Default SimConf where
  def = SimConf
    { mTopologyType      = Nothing
    , outMode            = PDF
    , simulationSteps    = 150000
    , simulationSamples  = 100
    , stabilityMargin    = 8
    , stabilityFrameSize = 1500000
    , disableReframing   = False
    , rusty              = False
    , waitTime           = 100000
    , stopWhenStable     = False
    , stopAfterStable    = Nothing
    , clockOffsets       = []
    , startupOffsets     = []
    , maxStartupOffset   = 0
    , createReport       = False
    , outDir             = "_build"
    , jsonArgs           = Nothing
    , stable             = Nothing
    }

-- | Command line parser for reading a simulation configuration.
simConfigCLIParser :: Parser SimConf
simConfigCLIParser =
  SimConf
    <$> optional topTypeCLIParser
    <*> option auto
          (  long "output-mode"
          <> short 'm'
          <> metavar "MODE"
          <> value (outMode def)
          <> showDefault
          <> help "Available modes are: csv, pdf"
          )
    <*> option auto
          (  long "steps"
          <> short 's'
          <> metavar "NUM"
          <> value (simulationSteps def)
          <> showDefault
          <> help "Number of clock cycles to simulate"
          )
    <*> option auto
          (  long "samples"
          <> short 'a'
          <> metavar "NUM"
          <> value (simulationSamples def)
          <> showDefault
          <> help "Number of samples to keep & pass to matplotlib"
          )
    <*> option auto
          (  long "margin"
          <> short 'g'
          <> metavar "NUM"
          <> value (stabilityMargin def)
          <> showDefault
          <> help
               (  "Maximum number of elements a buffer occupancy is "
               <> "allowed to deviate to be considered stable"
               )
          )
    <*> option auto
          (  long "frame-size"
          <> short 'f'
          <> metavar "NUM"
          <> value (stabilityFrameSize def)
          <> showDefault
          <> help
               (  "Minimum number of clock cycles a buffer occupancy "
               <> "must remain within to be considered stable"
               )
          )
    <*> flag (disableReframing def) (not $ disableReframing def)
          (  long "disable-reframing"
          <> short 'e'
          <> help "Disables clock control reframing"
          )
    <*> flag (rusty def) (not $ rusty def)
          (  long "get-rusty"
          <> short 'y'
          <> help "Simulate clock control via the Rust FFI"
          )
    <*> option auto
          (  long "wait-time"
          <> short 'w'
          <> metavar "NUM"
          <> value (waitTime def)
          <> showDefault
          <> help
               (  "Number of clock cycles to wait until reframing takes place "
               <> "(after stability has been detected, for all elastic buffers)"
               )
          )
    <*> flag (stopWhenStable def) (not $ stopWhenStable def)
          (  long "stop-when-stable"
          <> short 'x'
          <> help "Stop simulation as soon as all buffers get stable"
          )
    <*> optional
          ( option auto
              (  long "stop-after-stable"
              <> short 'X'
              <> metavar "NUM"
              <> help
                   (  "Stop simulation after all buffers have been stable for "
                   <> " at least the given number of simulation steps"
                   )
              )
          )
    <*> option auto
          (  long "clock-offsets"
          <> short 't'
          <> metavar "NUM LIST"
          <> value (clockOffsets def)
          <> showDefault
          <> help "Initital clock offsets (randomly generated if missing)"
          )
    <*> option auto
          (  long "startup-offsets"
          <> short 'T'
          <> metavar "NUM LIST"
          <> value (startupOffsets def)
          <> showDefault
          <> help (  "Initital startup offsets, i.e, the number of clock cycles "
                  <> "to wait before a node gets started (according to the "
                  <> "node's individual clock, randomly generated if missing)"
                  )

          )
    <*> option auto
          (  long "max-startup-offset"
          <> short 'u'
          <> metavar "NUM"
          <> value (maxStartupOffset def)
          <> showDefault
          <> help
               (  "Maximal number of clock cycles the startup of a node may be "
               <> "delayed (bounds the randomly generated offsets)"
               )
          )
    <*> flag (createReport def) (not $ createReport def)
          (  long "create-report"
          <> short 'r'
          <> help "Create a simulation report"
          )
    <*> strOption
          (  long "output-directory"
          <> short 'o'
          <> metavar "DIR"
          <> action "directory"
          <> value (outDir def)
          <> showDefault
          <> help "Directory, to which the generated files are written"
          )
    <*> optional
          ( strOption
              (  long "json-args"
              <> short 'j'
              <> metavar "FILE"
              <> action "file"
              <> help
                   (  "Read arguments from a 'simulate.json' file "
                   <> "(overwrites all arguments other than '-jz')"
                   )
              )
          )
    <*> pure Nothing

-- | Saves a topology and a corresponding simulation configuration to
-- respective files in 'outDir'.
saveSimConfig :: STop -> SimConf -> IO ()
saveSimConfig (STop t) cfg@SimConf{..} = do
  createDirectoryIfMissing True outDir
  let topologyFile = outDir </> simTopologyFileName
  writeFile topologyFile $ (<> "\n") $ render $ toDot t
  BS.writeFile (outDir </> simJsonConfigFileName) $ encode cfg
    { jsonArgs = Nothing
    , mTopologyType = case mTopologyType of
        Just (Random{}) -> Just $ DotFile topologyFile
        _               -> mTopologyType
    }
