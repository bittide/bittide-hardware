-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}

module Bittide.Instances.Hitl.Driver.ClockControl.Samples where

import Prelude

import Bittide.Instances.Domains (Bittide)
import Bittide.Instances.Hitl.Driver.ClockControl.Config (
  CcConf (topology),
  ccConfigFileName,
 )
import Bittide.Instances.Hitl.Setup (FpgaCount)
import Bittide.Sync (SyncOutGeneratorHalfPeriod)
import Control.Concurrent.Async (forConcurrently)
import Data.Binary.Get (Get)
import Data.ByteString.Lazy (ByteString)
import Data.Int (Int32)
import Data.Word (Word32, Word64, Word8)
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
import System.FilePath ((</>))

import qualified Bittide.ClockControl.Topology as Topology
import qualified Clash.Prelude as C
import qualified Data.Aeson as Aeson
import qualified Data.Binary.Get as Get
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Lazy as ByteStringLazy
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Csv as Csv
import qualified Data.String.Interpolate as I

type Link = (C.Index FpgaCount, C.Index FpgaCount)
type Picoseconds = Word64

data Buffer = Buffer
  { stable :: !Bool
  , settled :: !Bool
  , dataCount :: !Int32
  }
  deriving (Show, Eq, Generic)

data Sample = Sample
  { localClockCounter :: !Word64
  , numberOfSyncPulsesSeen :: !Word32
  , cyclesSinceSyncPulse :: !Word32
  , netSpeedChange :: !Int32
  , buffers :: !(C.Vec 7 Buffer)
  }
  deriving (Show, Eq, Generic)

-- | Parse a single sample in a 'Get' monad
parse1 :: Get Sample
parse1 = do
  localClockCounter <- Get.getWord64le
  numberOfSyncPulsesSeen <- Get.getWord32le
  cyclesSinceSyncPulse <- Get.getWord32le
  stables <- C.unpack . C.truncateB . C.pack <$> Get.getWord8
  settleds <- C.unpack . C.truncateB . C.pack <$> Get.getWord8
  _ <- Get.getByteString 2 -- padding
  netSpeedChange <- Get.getInt32le
  ebCounters <- sequence (C.repeat Get.getInt32le)

  return
    Sample
      { localClockCounter
      , numberOfSyncPulsesSeen
      , cyclesSinceSyncPulse
      , netSpeedChange
      , buffers = C.zipWith3 Buffer stables settleds ebCounters
      }

{- | Parse as many samples as possible from a 'ByteStringLazy.ByteString'. Any
remaining bytes that do not form a complete sample are ignored.
-}
parse :: ByteString -> [Sample]
parse bytes
  | ByteStringLazy.null bytes = []
  | Left (_, _, _) <- result = []
  | Right (rest, _, sample) <- result = sample : parse rest
 where
  result = Get.runGetOrFail parse1 bytes

{- | Parse a file containing samples. The file is expected to be in the same
format as produced by the `parse` function. Any remaining bytes that do not
form a complete sample are ignored.
-}
parseFile :: (HasCallStack) => FilePath -> IO [Sample]
parseFile filePath = parse <$> ByteStringLazy.readFile filePath

parseDirectory :: (HasCallStack) => FilePath -> IO (CcConf, C.Vec FpgaCount [Sample])
parseDirectory dir = do
  let path = dir </> ccConfigFileName
  ccConfBytes <- LazyByteString.readFile path
  ccConf <- case Aeson.eitherDecode @CcConf ccConfBytes of
    Left err -> error [I.i|Failed to parse CcConf from #{path}: #{err}|]
    Right conf -> pure conf

  samples <- forConcurrently C.indicesI $ \fpgaNr -> do
    if fromIntegral fpgaNr < Topology.size ccConf.topology
      then parseFile (dir </> [I.i|cc-samples-#{fpgaNr}.bin|])
      else pure []

  pure (ccConf, samples)

-- | Worker function for 'toNamedRecord' and 'headerOrder'
toNamedRecord# :: Sample -> [(Csv.Name, Csv.Field)]
toNamedRecord# sample =
  ("local_clock_counter", Csv.toField sample.localClockCounter)
    : ("number_of_sync_pulses_seen", Csv.toField sample.numberOfSyncPulsesSeen)
    : ("cycles_since_sync_pulse", Csv.toField sample.cyclesSinceSyncPulse)
    : ("net_speed_change", Csv.toField sample.netSpeedChange)
    : ("stables", Csv.toField stables)
    : ("settleds", Csv.toField settleds)
    : (zipWith go [0 ..] (C.toList (C.lazyV sample.buffers)))
 where
  go :: Int -> Buffer -> (Csv.Name, Csv.Field)
  go i buffer = ("eb_counter_" <> Char8.pack (show i), Csv.toField buffer.dataCount)

  stables, settleds :: Word8
  stables = C.unpack $ C.extend $ C.pack $ fmap (.stable) sample.buffers
  settleds = C.unpack $ C.extend $ C.pack $ fmap (.settled) sample.buffers

instance Csv.ToNamedRecord Sample where
  toNamedRecord = Csv.namedRecord . toNamedRecord#

instance Csv.DefaultOrdered Sample where
  headerOrder = Csv.header . fmap fst . toNamedRecord#

-- | Calculate the number of picoseconds from a sample
sampleToPicoseconds :: Sample -> Picoseconds
sampleToPicoseconds sample = seen1 * psPerSync + since1 * psPerCycle
 where
  psPerCycle = C.natToNum @(C.DomainPeriod Bittide)
  psPerSync = C.natToNum @SyncOutGeneratorHalfPeriod

  seen0 = C.numConvert sample.numberOfSyncPulsesSeen :: C.Unsigned 32
  seen1 = C.numConvert seen0 :: Word64

  since0 = C.numConvert sample.cyclesSinceSyncPulse :: C.Unsigned 32
  since1 = C.numConvert since0 :: Word64

psToS :: Picoseconds -> Float
psToS ps = fromIntegral ps * 1e-12
