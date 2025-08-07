-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Bittide.Instances.Hitl.Driver.ClockControl.Config (
  CcConf (..),
  ccConfigFileName,
  saveCcConfig,
  toLinkMaskCcConf,
) where

import Prelude

import Bittide.ClockControl.Topology (Topology (..))
import Bittide.Instances.Hitl.Setup (FpgaCount, LinkCount)
import Clash.Class.BitPackC (BitPackC)
import Data.Aeson (FromJSON (..), ToJSON (..))
import GHC.Generics (Generic)
import Protocols.MemoryMap.FieldType (ToFieldType)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))

import qualified Bittide.Instances.Hitl.Setup as Setup
import qualified Clash.Prelude as C
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as ByteString

type LinkMaskTopology = C.Vec FpgaCount (C.BitVector LinkCount)

-- | Default name of the clock control JSON configuration file.
ccConfigFileName :: String
ccConfigFileName = "cc-config.json"

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
    (Show, Ord, Eq, Functor, Generic, ToJSON, FromJSON, C.BitPack, BitPackC, ToFieldType)

{- | Convert a clock control configuration for the full topology to one that
contains only the link masks. The latter has a @BitPackC@ instance, making it
suitable for use in hardware/registers.
-}
toLinkMaskCcConf :: CcConf Topology -> CcConf LinkMaskTopology
toLinkMaskCcConf = fmap Setup.linkMasks

{- | Saves a clock control configuration to a file called 'ccConfigFileName' in
the given directory.
-}
saveCcConfig :: FilePath -> CcConf Topology -> IO ()
saveCcConfig dir ccConf = do
  createDirectoryIfMissing True dir
  ByteString.writeFile (dir </> ccConfigFileName) (Aeson.encode ccConf)
