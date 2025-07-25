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
import Bittide.Topology (STopology (..), TopologyType (..), toDot)

import Data.Aeson (FromJSON (..), ToJSON (..), encode)
import Data.Default (Default (..))
import GHC.Generics (Generic)
import Language.Dot.Pretty (render)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))

import qualified Data.ByteString.Lazy as BS (writeFile)

-- | Default name of the clock control JSON configuration file.
simJsonConfigFileName :: String
simJsonConfigFileName = "simulate.json"

-- | Default name of the clock control topology Graphviz file.
simTopologyFileName :: String
simTopologyFileName = "topology.gv"

-- | Collection of all clock control configuration parameters.
data CcConf = CcConf
  { ccTopologyType :: TopologyType
  -- ^ The topology type of the network to be simulated. Have a
  -- look at 'Bittide.Topology' for more insights on the supported
  -- topology types and their corresponding topologies.
  , samples :: Int
  -- ^ The number of samples to be utilized for result
  -- generation. From the 'duration' many samples available, only
  -- every @duration `quot` samples@th sample is used.
  , reframe :: Bool
  -- ^ Some flag for enabeling or disabling reframing.
  , waitTime :: Int
  -- ^ Number of clock cycles to wait until reframing takes place
  -- (after stability has been detected, for all elastic buffers).
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
      , samples = 100
      , reframe = True
      , waitTime = 100000
      , clockOffsets = Nothing
      , startupDelays = []
      , outDir = "_build"
      , stable = Nothing
      }

{- | Saves a topology and a corresponding simulation configuration to
respective files in 'outDir'.
-}
saveCcConfig :: STopology -> CcConf -> IO ()
saveCcConfig (STopology t) cfg@CcConf{..} = do
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
