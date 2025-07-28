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
import Bittide.Topology (Topology (..), complete, toDot)

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Default (Default (..))
import GHC.Generics (Generic)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as ByteString

-- | Default name of the clock control JSON configuration file.
simJsonConfigFileName :: String
simJsonConfigFileName = "simulate.json"

-- | Default name of the clock control topology Graphviz file.
simTopologyFileName :: String
simTopologyFileName = "topology.gv"

-- | Collection of all clock control configuration parameters.
data CcConf = CcConf
  { topology :: Topology
  -- ^ The topology of the network to be simulated
  , samples :: Int
  -- ^ The number of samples to be utilized for result
  -- generation. From the 'duration' many samples available, only
  -- every @duration `quot` samples@th sample is used.
  , reframe :: Bool
  -- ^ Flag for enabling or disabling reframing.
  , waitTime :: Int
  -- ^ Number of clock cycles to wait until reframing takes place
  -- (after stability has been detected, for all elastic buffers).
  , clockOffsets :: Maybe [PartsPer]
  -- ^ The initial clock offsets in Femtoseconds
  -- (randomly generated if missing).
  , startupDelays :: [Int]
  -- ^ The initial startup offsets, i.e, the number of clock
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
      { topology = complete 8
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
saveCcConfig :: CcConf -> IO ()
saveCcConfig ccConf = do
  createDirectoryIfMissing True ccConf.outDir
  let simConfigFile = ccConf.outDir </> simJsonConfigFileName
  ByteString.writeFile simConfigFile (Aeson.encode ccConf)

  let dotFile = ccConf.outDir </> simTopologyFileName
  writeFile dotFile (toDot ccConf.topology)
