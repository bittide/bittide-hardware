-- | Println-debugging during simulation
module Println ( hookPrint ) where

import Clash.Prelude

import qualified Data.ByteString as BS
import Data.Foldable (traverse_)
import Data.Maybe (catMaybes)
import Data.Word (Word8)

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
