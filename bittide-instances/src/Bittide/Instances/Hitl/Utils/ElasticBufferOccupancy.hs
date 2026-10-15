-- SPDX-FileCopyrightText: 2026 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE PackageImports #-}

{- | Record elastic buffer occupancies reported by a management unit over long
runs. The management unit periodically reads (and clears) the minimum and
maximum occupancy each elastic buffer has seen, and prints them over UART as:

@
[MU] EBW <window> <time_us> min=<eb0>,..,<eb6> max=<eb0>,..,<eb6> uf=<mask> of=<mask>
@

where @uf@ and @of@ are bitmasks of the elastic buffers that underflowed or
overflowed during the window.
-}
module Bittide.Instances.Hitl.Utils.ElasticBufferOccupancy where

import Prelude

import Bittide.Instances.Hitl.Setup (LinkCount)
import Bittide.Instances.Hitl.Utils.Ugn (cpuPrefix, parseInteger, parseNatural)
import Control.Concurrent.Chan (Chan, readChan)
import Control.Monad (forM_, unless, when)
import Data.Bits (testBit)
import Data.ByteString (ByteString)
import Data.Int (Int8)
import Data.String.Interpolate (i)
import Data.Word (Word64, Word8)
import System.IO (Handle, IOMode (WriteMode), hFlush, hPutStrLn, withFile)
import Text.Parsec
import Text.Parsec.String (Parser)
import "extra" Data.List.Extra (trimEnd)

import qualified Clash.Prelude as C
import qualified Data.ByteString.Char8 as Char8
import qualified Data.List as L

-- | Minimum and maximum occupancy seen by all elastic buffers of a node in a window
data EbWindow = EbWindow
  { window :: Word64
  , timeUs :: Word64
  , mins :: [Int8]
  , maxs :: [Int8]
  , underflows :: Word8
  , overflows :: Word8
  }
  deriving (Show, Eq)

-- | Line printed by the management unit when it is done measuring
measurementDoneLine :: String
measurementDoneLine = "[MU] EB measurement done"

-- | Parse an 'EbWindow', see module documentation for the expected format.
parseEbWindow :: Parser EbWindow
parseEbWindow = do
  cpuPrefix "MU"
  _ <- string " EBW "
  window <- fromIntegral <$> parseNatural
  _ <- char ' '
  timeUs <- fromIntegral <$> parseNatural
  _ <- string " min="
  mins <- parseOccupancies
  _ <- string " max="
  maxs <- parseOccupancies
  _ <- string " uf="
  underflows <- fromIntegral <$> parseNatural
  _ <- string " of="
  overflows <- fromIntegral <$> parseNatural
  eof
  pure EbWindow{window, timeUs, mins, maxs, underflows, overflows}
 where
  linkCount = C.natToNum @LinkCount :: Int
  parseOccupancies = do
    occupancies <- (fromIntegral <$> parseInteger) `sepBy1` char ','
    unless (L.length occupancies == linkCount) $
      fail [i|Expected #{linkCount} occupancies, got #{L.length occupancies}|]
    pure occupancies

csvHeader :: String
csvHeader =
  L.intercalate "," $
    ["window", "time_us"]
      <> L.concat [[[i|eb#{n}_min|], [i|eb#{n}_max|]] | n <- [0 .. linkCount - 1]]
      <> ["underflows", "overflows"]
 where
  linkCount = C.natToNum @LinkCount :: Int

toCsvRow :: EbWindow -> String
toCsvRow w =
  L.intercalate "," $
    [show w.window, show w.timeUs]
      <> L.concat (L.zipWith (\mn mx -> [show mn, show mx]) w.mins w.maxs)
      <> [show w.underflows, show w.overflows]

{- | Read 'EbWindow's from a node's UART until the management unit reports it is
done measuring. Every window is appended to the given CSV file as soon as it
arrives, so data is retained even if the measurement is cut short. Fails if
clock control reports a problem.
-}
recordEbWindows :: FilePath -> Int -> Chan ByteString -> IO [EbWindow]
recordEbWindows csvPath nodeIndex chan =
  withFile csvPath WriteMode $ \h -> do
    hPutStrLn h csvHeader
    go h []
 where
  go :: Handle -> [EbWindow] -> IO [EbWindow]
  go h acc = do
    line <- trimEnd . Char8.unpack <$> readChan chan
    handleLine h acc line

  handleLine :: Handle -> [EbWindow] -> String -> IO [EbWindow]
  handleLine h acc line
    | line == measurementDoneLine = pure (L.reverse acc)
    | "[CC]" `L.isPrefixOf` line
    , any (`L.isInfixOf` line) ["Links no longer stable", "Deadline missed"] =
        fail [i|Node #{nodeIndex}: clock control reported: #{line}|]
    | "[MU] EBW " `L.isPrefixOf` line =
        case parse parseEbWindow "elastic buffer window" line of
          Left err -> do
            putStrLn [i|Node #{nodeIndex}: ignoring malformed line #{show line}: #{err}|]
            go h acc
          Right w -> do
            hPutStrLn h (toCsvRow w)
            -- Flush every minute's worth of windows, so progress can be
            -- inspected while the test is running.
            when (w.window `mod` 60 == 0) $ do
              hFlush h
              let
                windowNr = w.window
                (lo, hi) = envelope (w : acc)
              putStrLn
                [i|Node #{nodeIndex}: window #{windowNr}, occupancy range so far: [#{lo}, #{hi}]|]
            go h (w : acc)
    | otherwise = do
        unless (L.null line) $ putStrLn [i|Node #{nodeIndex}: #{line}|]
        go h acc

-- | Lowest minimum and highest maximum over all elastic buffers and windows
envelope :: [EbWindow] -> (Int8, Int8)
envelope ws = (minimumOr 0 (L.concatMap (.mins) ws), maximumOr 0 (L.concatMap (.maxs) ws))

minimumOr :: (Ord a) => a -> [a] -> a
minimumOr dflt [] = dflt
minimumOr _ xs = L.minimum xs

maximumOr :: (Ord a) => a -> [a] -> a
maximumOr dflt [] = dflt
maximumOr _ xs = L.maximum xs

{- | Print per node, per elastic buffer, the lowest and highest occupancy seen
over all windows. Fails if any elastic buffer under- or overflowed.
-}
summarizeEbWindows :: [[EbWindow]] -> IO ()
summarizeEbWindows windowsPerNode = do
  putStrLn "\n=== Elastic buffer occupancy ranges (min, max) ==="
  forM_ (L.zip [0 :: Int ..] windowsPerNode) $ \(nodeIndex, ws) -> do
    let
      perEb = L.zip (L.transpose ((.mins) <$> ws)) (L.transpose ((.maxs) <$> ws))
      showRange (mns, mxs) = [i|(#{minimumOr 0 mns}, #{maximumOr 0 mxs})|] :: String
    putStrLn
      [i|Node #{nodeIndex} (#{L.length ws} windows): #{L.unwords (showRange <$> perEb)}|]

  let
    flowProblems =
      [ [i|Node #{nodeIndex}, window #{windowNr}: #{kind} on elastic buffer(s) #{ebs}|]
      | (nodeIndex, ws) <- L.zip [0 :: Int ..] windowsPerNode
      , w <- ws
      , let windowNr = w.window
      , (kind, mask) <- [("underflow" :: String, w.underflows), ("overflow", w.overflows)]
      , let ebs = L.filter (testBit mask) [0 .. 7 :: Int]
      , not (L.null ebs)
      ]

  unless (L.null flowProblems) $ do
    putStrLn "\n=== Elastic buffer under-/overflows ==="
    mapM_ putStrLn flowProblems
    fail "Elastic buffer(s) under- or overflowed"
