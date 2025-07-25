-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE OverloadedStrings #-}

{- | Utilities to parse a binary file stored by @bittide_sys::sample_store@ (see)
Rust crates.
-}
module Bittide.ClockControl.SampleStore where

import Clash.Prelude hiding (sample)

import Data.Binary.Get
import Data.Csv ((.=))
import Data.Int (Int32)
import Data.Word (Word32, Word64)

import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as L
import qualified Data.Csv as Csv
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import qualified Prelude as P

data BufferStatus = BufferStatus
  { data_count :: Signed 32
  , stable :: Bool
  , settled :: Bool
  }
  deriving (Show, Generic)

-- | Represents a single data sample as stored by the sample store
data Sample n = Sample
  { local_clock_counter :: Unsigned 64
  , number_of_sync_pulses_seen :: Unsigned 32
  , cycles_since_sync_pulse :: Unsigned 32
  , net_speed_change :: Signed 32
  , buffers :: Vec n BufferStatus
  }
  deriving (Show, Generic)

instance (KnownNat n) => Csv.DefaultOrdered (Sample n) where
  headerOrder _ =
    V.fromList
      $ [ "local_clock_counter"
        , "number_of_sync_pulses_seen"
        , "cycles_since_sync_pulse"
        , "net_speed_change"
        ]
      <> count_names
      <> stable_names
      <> settled_names
   where
    (count_names, stable_names, settled_names) =
      P.unzip3 $ P.map eb_names [0 .. n - 1]

    eb_names i =
      ( "eb_data_count_" <> B8.pack (show i)
      , "eb_stable_" <> B8.pack (show i)
      , "eb_settled_" <> B8.pack (show i)
      )

    n = natToNum @n @Int

instance (KnownNat n) => Csv.ToNamedRecord (Sample n) where
  toNamedRecord sample = HM.fromList (fields0 <> fields1)
   where
    fields0 =
      [ "local_clock_counter" .= bitCoerce @_ @Word64 sample.local_clock_counter
      , "number_of_sync_pulses_seen" .= bitCoerce @_ @Word32 sample.number_of_sync_pulses_seen
      , "cycles_since_sync_pulse" .= bitCoerce @_ @Word32 sample.cycles_since_sync_pulse
      , "net_speed_change" .= bitCoerce @_ @Int32 sample.net_speed_change
      ]

    fields1 = P.concat $ P.zipWith goBuffer [(0 :: Int) ..] (toList sample.buffers)

    goBuffer idx status =
      [ ("eb_data_count" <> suffix) .= bitCoerce @_ @Int32 status.data_count
      , ("eb_stable" <> suffix) .= if status.stable then "true" else ("false" :: String)
      , ("eb_settled" <> suffix) .= if status.settled then "true" else ("false" :: String)
      ]
     where
      suffix = "_" <> B8.pack (show idx)

-- | A parser for a single 'Sample' record from a binary stream.
getSample :: (KnownNat n) => Get (Sample n)
getSample = do
  -- The fields are read in the exact order they appear in the binary file.
  p_local_clock_counter <- getWord64le
  p_num_sync_pulses <- getWord32le
  p_cycles_since_sync <- getWord32le
  p_stables <- unpack . resize . pack <$> getWord8
  p_settleds <- unpack . resize . pack <$> getWord8
  skip 2 -- 2 bytes of padding
  p_net_speed_change <- getInt32le

  data_counts <- fmap bitCoerce $ sequence $ repeat getInt32le

  return
    Sample
      { local_clock_counter = bitCoerce p_local_clock_counter
      , number_of_sync_pulses_seen = bitCoerce p_num_sync_pulses
      , cycles_since_sync_pulse = bitCoerce p_cycles_since_sync
      , net_speed_change = bitCoerce p_net_speed_change
      , buffers = BufferStatus <$> data_counts <*> p_stables <*> p_settleds
      }

{- | Lazily parses a lazy ByteString into a list of 'Sample' records.
This is the Haskell equivalent of the Python generator function.
-}
parseAll :: L.ByteString -> [Sample 7]
parseAll bs
  | L.null bs = []
  | otherwise = case runGetOrFail getSample bs of
      Right (remainder, _, sample) -> sample : parseAll remainder
      Left (_, _, errMsg) -> error $ "Parse error: " <> errMsg
