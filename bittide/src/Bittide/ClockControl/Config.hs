-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Bittide.ClockControl.Config (
  CcConf (..),
  defCcConf,
  saveCcConfig,
  ccConfigFileName,
) where

import Clash.Prelude

import Bittide.ClockControl.Topology (Topology)
import Clash.Class.BitPackC (BitPackC)
import Data.Aeson (FromJSON, ToJSON)
import Data.Word (Word32)
import Protocols.MemoryMap.TypeDescription
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))

import qualified Bittide.ClockControl.Topology as Topology
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as ByteString

data CallistoConfig = CallistoConfig
  { waitTime :: Maybe Word32
  -- ^ Number of cycles to wait until reframing takes place after stability has
  -- been detected. Reframing allows a system to resettle buffers around their
  -- midpoints, without dropping any frames. For more information, see
  -- [arXiv:2303.11467](https://arxiv.org/abs/2303.11467).
  , gain :: Float
  -- ^ See https://github.com/bittide/Callisto.jl/blob/e47139fca128995e2e64b2be935ad588f6d4f9fb/demo/pulsecontrol.jl#L24.
  }
  deriving (Eq, Ord, Show, Generic)
  deriving (BitPack, BitPackC, NFDataX)
  deriving (FromJSON, ToJSON)
deriveTypeDescription ''CallistoConfig

{- | Collection of all clock control configuration parameters. This is typically
instantiated as either @CcConf Topology@ or @CcConf LinkMaskTopology@. The
former contains the full topology, including meta information, while the latter
contains only the link masks -- suitable for use in hardware.
-}
data CcConf a = CcConf
  { topology :: a
  -- ^ The topology of the network
  , callisto :: CallistoConfig
  -- ^ Clock control configuration for the Callisto algorithm. See @callisto.rs@.
  }
  deriving (Eq, Ord, Show, Generic, Functor)
  deriving (BitPack, BitPackC, NFDataX)
  deriving (FromJSON, ToJSON)

deriveTypeDescription ''CcConf

defCcConf :: Int -> CcConf Topology
defCcConf nLinks =
  CcConf
    { topology = Topology.complete nLinks
    , callisto = CallistoConfig{waitTime = Nothing, gain = 2e-9}
    }

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
