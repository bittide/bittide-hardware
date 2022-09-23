-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

-- | Println-debugging during simulation
module ContranomySim.Print ( hookPrint, getDataBytes ) where

import           Clash.Prelude

import qualified Data.ByteString as BS
import           Data.Foldable   (traverse_)
import qualified Data.List       as L
import           Data.Maybe      (catMaybes)
import           Data.Word       (Word8)


getDataBytes
  :: Int -- ^ How many bytes to sample
  -> Unsigned 32 -- ^ Address
  -> [Maybe (Unsigned 32, Signed 32)] -- ^ Data
  -> BS.ByteString
getDataBytes n addr =
     BS.pack
   . L.take n
   . fmap (addrByte.snd)
   . filter (\(addr', _) -> addr' == addr)
   . catMaybes

hookPrint
  :: Unsigned 32 -- ^ Address
  -> [Maybe (Unsigned 32, Signed 32)] -- ^ Data
  -> IO ()
hookPrint addr =
    traverse_ (pChar . snd)
  . filter (\(addr', _) -> addr' == addr)
  . catMaybes

pChar :: Signed 32 -> IO ()
pChar = BS.putStr . BS.singleton . addrByte

-- take one byte
addrByte :: Signed 32 -> Word8
addrByte = bitCoerce . slice d7 d0
