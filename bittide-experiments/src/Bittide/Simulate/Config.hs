-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ImplicitPrelude #-}

module Bittide.Simulate.Config (
  CcConf (..),
  simJsonConfigFileName,
  simTopologyFileName,
  saveCcConfig,
) where

import Bittide.Arithmetic.PartsPer (PartsPer)
import Bittide.Topology (STop (..), TopologyType (..), toDot)

import Data.Aeson (FromJSON (..), ToJSON (..), encode)
import qualified Data.ByteString.Lazy as BS (writeFile)
import Data.Default (Default (..))
import GHC.Generics (Generic)
import Language.Dot.Pretty (render)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))

-- | Default name of the clock control JSON configuration file.
simJsonConfigFileName :: String
simJsonConfigFileName = "simulate.json"

-- | Default name of the clock control topology Graphviz file.
simTopologyFileName :: String
simTopologyFileName = "topology.gv"

-- | Collection of all clock control configuration parameters.
data CcConf = CcConf
  { ccTopologyType :: TopologyType IO Integer
  -- ^ The topology type of the network to be simulated. Have a
  -- look at 'Bittide.Topology' for more insights on the supported
  -- topology types and their corresponding topologies.
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
  , waitTime :: Int
  -- ^ Number of clock cycles to wait until reframing takes place
  -- (after stability has been detected, for all elastic buffers).
  , stopWhenStable :: Bool
  -- ^ Stop simulation as soon as all buffers get stable.
  , stopAfterStable :: Maybe Int
  -- ^ Stop simulation after all buffers have been stable for
  -- at least the given number of clock cycles.
  , clockOffsets :: Maybe [PartsPer]
  -- ^ The initital clock offsets in Femtoseconds
  -- (randomly generated if missing).
  , startupDelays :: [Int]
  -- ^ The Initital startup offsets, i.e, the number of clock
  -- cycles to wait before a node gets started (according to the
  -- node's individual clock, randomly generated if missing).
  , outDir :: FilePath
  -- ^ The directory, in which the generated files are stored.
  , stable :: Maybe Bool
  -- ^ Stability result of the elastic buffers at the end of
  -- simulation, if available.
  }
  deriving (Show, Ord, Eq, Generic, ToJSON, FromJSON)

instance Default CcConf where
  def =
    CcConf
      { ccTopologyType = Complete 8
      , duration = 150000
      , samples = 100
      , stabilityMargin = 8
      , stabilityFrameSize = 1500000
      , reframe = True
      , waitTime = 100000
      , stopWhenStable = False
      , stopAfterStable = Nothing
      , clockOffsets = Nothing
      , startupDelays = []
      , outDir = "_build"
      , stable = Nothing
      }

{- | Saves a topology and a corresponding simulation configuration to
respective files in 'outDir'.
-}
saveCcConfig :: STop -> CcConf -> IO ()
saveCcConfig (STop t) cfg@CcConf{..} = do
  createDirectoryIfMissing True outDir
  let topologyFile = outDir </> simTopologyFileName
  writeFile topologyFile $ (<> "\n") $ render $ toDot t
  BS.writeFile (outDir </> simJsonConfigFileName) $
    encode
      cfg
        { ccTopologyType = case ccTopologyType of
            Random{} -> DotFile topologyFile
            _ -> ccTopologyType
        }
