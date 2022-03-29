module Contranomy.Println ( hookPrint, contranomyPrintln ) where

import Clash.Prelude

import qualified Data.ByteString as BS
import Data.Foldable (traverse_)
import Data.Maybe (catMaybes)
import Data.IntMap (IntMap)

import Contranomy (Core, contranomy')

hookPrint
  :: KnownDomain dom
  => Unsigned 32
  -> Signal dom (Maybe (Unsigned 32, Signed 32))
  -> IO ()
hookPrint addr signal = hookPrint' addr (sample signal)

hookPrint'
  :: Unsigned 32 -- ^ Address
  -> [Maybe (Unsigned 32, Signed 32)] -- ^ Data
  -> IO ()
hookPrint' addr =
    pChars
  . fmap snd
  . filter (\(addr', _) -> addr' == addr)
  . catMaybes

pChars :: Foldable t => t (Signed 32) -> IO ()
pChars = traverse_ pChar

pChar :: Signed 32 -> IO ()
pChar = BS.putStr . BS.pack . intToBytes

intToBytes :: (BitPack a, BitSize a ~ 8) => Signed 32 -> [a]
intToBytes = fmap bitCoerce . sequence
    [ slice d31 d24
    , slice d23 d16
    , slice d15 d8
    , slice d7  d0
    ]

contranomyPrintln
  :: Clock Core
  -> Reset Core
  -> BitVector 32
  -> IntMap (BitVector 8)
  -> IntMap (BitVector 8)
  -> Signal Core (Bool, Bool, BitVector 32)
  -> IO ()
contranomyPrintln clk rst entry iMem dMem bundled =
  hookPrint 0x0 . fmap snd -- to be replaced with the actual address
    $ contranomy' clk rst entry iMem dMem bundled
