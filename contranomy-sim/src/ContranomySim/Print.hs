-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

-- | Println-debugging during simulation
module ContranomySim.Print ( hookPrint, getDataBytes, characterDevice ) where

import           Clash.Prelude

import Clash.Signal.Internal

import qualified Data.ByteString as BS
import           Data.Foldable   (traverse_)
import qualified Data.List       as L
import           Data.Maybe      (catMaybes)
import           Data.Word       (Word8)

import Bittide.Extra.Wishbone
import GHC.IO (unsafePerformIO)

{-# NOINLINE characterDevice #-}

characterDevice
  :: Signal dom (WishboneM2S 4 32)
  -> Signal dom (WishboneS2M 4)
characterDevice inSignal = unsafePerformIO (go inSignal)
 where
  go :: Signal dom (WishboneM2S 4 32) -> IO (Signal dom (WishboneS2M 4))
  go (m2s :- m2ss)
    | not (busCycle m2s && strobe m2s) = (wishboneS2M :-) <$> go m2ss
    | writeEnable m2s = do
        let char = bitCoerce $ writeData m2s
        pChar char
        (wishboneS2M { acknowledge = True } :-) <$> go m2ss
    | otherwise =
      ((wishboneS2M @4) { acknowledge = True, readData = 0 } :-) <$> go m2ss

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
