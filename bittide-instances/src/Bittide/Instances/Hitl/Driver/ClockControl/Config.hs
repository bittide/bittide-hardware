-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Bittide.Instances.Hitl.Driver.ClockControl.Config (
  CcConf (..),
  ccConfigFileName,
  saveCcConfig,
) where

import Prelude

import Bittide.ClockControl.Topology (Topology (..))
import Data.Aeson (FromJSON (..), ToJSON (..))
import GHC.Generics (Generic)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as ByteString

-- | Default name of the clock control JSON configuration file.
ccConfigFileName :: String
ccConfigFileName = "cc-config.json"

-- | Collection of all clock control configuration parameters.
data CcConf = CcConf
  { topology :: Topology
  -- ^ The topology of the network
  }
  deriving (Show, Ord, Eq, Generic, ToJSON, FromJSON)

{- | Saves a clock control configuration to a file called 'ccConfigFileName' in
the given directory.
-}
saveCcConfig :: FilePath -> CcConf -> IO ()
saveCcConfig dir ccConf = do
  createDirectoryIfMissing True dir
  ByteString.writeFile (dir </> ccConfigFileName) (Aeson.encode ccConf)
