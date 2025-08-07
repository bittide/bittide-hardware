-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Bittide.ClockControl.Config (
  CcConf (..),
  saveCcConfig,
  ccConfigFileName,
) where

import Clash.Prelude

import Bittide.ClockControl.Topology (Topology)
import Clash.Class.BitPackC (BitPackC)
import Data.Aeson (FromJSON, ToJSON)
import Protocols.MemoryMap.FieldType (ToFieldType)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as ByteString

{- | Collection of all clock control configuration parameters. This is typically
instantiated as either @CcConf Topology@ or @CcConf LinkMaskTopology@. The
former contains the full topology, including meta information, while the latter
contains only the link masks -- suitable for use in hardware.
-}
data CcConf a = CcConf
  { topology :: a
  -- ^ The topology of the network
  }
  deriving
    (Show, Ord, Eq, Functor, Generic, ToJSON, FromJSON, BitPack, BitPackC, ToFieldType)

-- | Default name of the clock control JSON configuration file.
ccConfigFileName :: String
ccConfigFileName = "cc-config.json"

{- | Saves a clock control configuration to a file called 'ccConfigFileName' in
the given directory.
-}
saveCcConfig :: FilePath -> CcConf Topology -> IO ()
saveCcConfig dir ccConf = do
  createDirectoryIfMissing True dir
  ByteString.writeFile (dir </> ccConfigFileName) (Aeson.encode ccConf)
