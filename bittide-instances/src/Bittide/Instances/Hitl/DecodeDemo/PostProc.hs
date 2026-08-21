-- SPDX-FileCopyrightText: 2026 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{- | Post-processing for the decode demo: turns the per-node latency dumps
into one pooled CSV per variant plus a human-readable summary.

Inputs, all in @_build/hitl/Decode_Demo_DUT/@ (written by the driver):

* @results-{c,aprime}-<n>.data@ — GDB memory dumps of the management-unit
  firmware's @DecodeResults@ struct (layout mirrored from
  @firmware-support/bittide-sys/src/decode_demo.rs@).
* @pe-<variant>-<n>.txt@ — key/value dumps of the processing-element and
  credit-link registers, including the hardware histogram. Variants: the
  quiet runs (@a@, @b@, @bsf@) and the contention cells
  (@{ac,bc}{25,50,75}@).
* @tg-<cell>-<n>.txt@ — the traffic generator's counters and queue-delay
  histogram for each contention cell.

Outputs: @decode-demo-hist.csv@ (variant, node, latency-in-cycles,
bin-width, count) and @decode-demo-summary.txt@.
-}
module Bittide.Instances.Hitl.DecodeDemo.PostProc where

import Prelude

import Bittide.Hitl (TestStepResult (..))
import Control.Exception (SomeException, try)
import Control.Monad (forM)
import Data.Bits (shiftL)
import Data.List (intercalate)
import Data.Maybe (catMaybes)
import Data.Word (Word32)
import Project.FilePath (findParentContaining)
import System.Directory (doesFileExist)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))

import qualified Data.ByteString as BS

fpgaCount :: Int
fpgaCount = 8

-- | One histogram: (base latency, bin width in cycles, counts per bin).
data Histogram = Histogram
  { histBase :: Word32
  , binWidth :: Word32
  , bins :: [Word32]
  }

data VariantDump = VariantDump
  { variantName :: String
  , node :: Int
  , tokensDone :: Word32
  , failures :: Word32
  -- ^ Checksum failures plus lost frames / missed deadlines
  , minLatency :: Word32
  , maxLatency :: Word32
  , histogram :: Histogram
  }

word32At :: BS.ByteString -> Int -> Word32
word32At bs off =
  sum
    [ fromIntegral (BS.index bs (off + k)) `shiftL` (8 * k)
    | k <- [0 .. 3]
    ]

-- Layout of the firmware's #[repr(C)] DecodeResults. Keep in sync!
parseMcResults :: String -> Int -> BS.ByteString -> VariantDump
parseMcResults name n bs =
  VariantDump
    { variantName = name
    , node = n
    , tokensDone = word32At bs 8
    , failures = word32At bs 12 + word32At bs 16 + word32At bs 20
    , minLatency = word32At bs 32
    , maxLatency = word32At bs 36
    , histogram =
        Histogram
          { histBase = word32At bs 24
          , binWidth = 1 `shiftL` fromIntegral (word32At bs 28)
          , bins = [word32At bs (44 + 4 * b) | b <- [0 .. 255]]
          }
    }

parsePeDump :: String -> Int -> String -> Maybe VariantDump
parsePeDump name n contents = do
  let
    kvs = [(k, vs) | k : vs <- words <$> lines contents]
    field k = case lookup k kvs of
      Just (v : _) -> Just (read v)
      _ -> Nothing
  tokensDone <- field "tokens_done"
  failures <- field "checksum_fail_count"
  minLatency <- field "min_latency"
  maxLatency <- field "max_latency"
  histBase <- field "hist_base"
  binsRaw <- lookup "hist" kvs
  let histShift = maybe 0 fromIntegral (field "hist_shift" :: Maybe Word32)
  pure
    VariantDump
      { variantName = name
      , node = n
      , tokensDone
      , failures
      , minLatency
      , maxLatency
      , histogram =
          Histogram{histBase, binWidth = 1 `shiftL` histShift, bins = map read binsRaw}
      }

{- | The traffic generator's side of a contention cell, as a variant dump:
"latency" is the burst queueing delay (slot time to port grant).
-}
parseTgDump :: String -> Int -> String -> Maybe VariantDump
parseTgDump name n contents = do
  let
    kvs = [(k, vs) | k : vs <- words <$> lines contents]
    field k = case lookup k kvs of
      Just (v : _) -> Just (read v)
      _ -> Nothing
  received <- field "tg_received"
  errors <- field "tg_pattern_errors"
  collisions <- field "tg_collisions"
  minQ <- field "tg_min_queue"
  maxQ <- field "tg_max_queue"
  binsRaw <- lookup "tg_hist" kvs
  let histShift = maybe 0 fromIntegral (field "tg_hist_shift" :: Maybe Word32)
  pure
    VariantDump
      { variantName = name
      , node = n
      , tokensDone = received
      , failures = errors + collisions
      , minLatency = minQ
      , maxLatency = maxQ
      , histogram =
          Histogram{histBase = 0, binWidth = 1 `shiftL` histShift, bins = map read binsRaw}
      }

loadDumps :: FilePath -> IO [VariantDump]
loadDumps hitlDir = do
  mcDumps <- forM [(v, n) | v <- ["c", "aprime"], n <- [0 .. fpgaCount - 1]] $ \(v, n) -> do
    let path = hitlDir </> "results-" <> v <> "-" <> show n <> ".data"
    exists <- doesFileExist path
    if exists
      then Just . parseMcResults v n <$> BS.readFile path
      else pure Nothing
  let cells = [d <> duty | d <- ["ac", "bc"], duty <- ["25", "50", "75"]]
  peDumps <- forM [(v, n) | v <- ["a", "b", "bsf"] <> cells, n <- [0 .. fpgaCount - 1]] $ \(v, n) -> do
    let path = hitlDir </> "pe-" <> v <> "-" <> show n <> ".txt"
    exists <- doesFileExist path
    if exists
      then parsePeDump v n <$> readFile path
      else pure Nothing
  tgDumps <- forM [(v, n) | v <- cells, n <- [0 .. fpgaCount - 1]] $ \(v, n) -> do
    let path = hitlDir </> "tg-" <> v <> "-" <> show n <> ".txt"
    exists <- doesFileExist path
    if exists
      then parseTgDump ("tg-" <> v) n <$> readFile path
      else pure Nothing
  pure $ catMaybes (mcDumps <> peDumps <> tgDumps)

histCsv :: [VariantDump] -> String
histCsv dumps =
  unlines $
    "variant,node,latency_cycles,bin_width,count"
      : [ intercalate
            ","
            [ d.variantName
            , show d.node
            , show (d.histogram.histBase + fromIntegral b * d.histogram.binWidth)
            , show d.histogram.binWidth
            , show count
            ]
        | d <- dumps
        , (b, count) <- zip [(0 :: Int) ..] d.histogram.bins
        , count /= 0
        ]

summary :: [VariantDump] -> String
summary dumps =
  unlines $
    "variant node tokens_done failures min_latency max_latency"
      : [ unwords
            [ d.variantName
            , show d.node
            , show d.tokensDone
            , show d.failures
            , show d.minLatency
            , show d.maxLatency
            ]
        | d <- dumps
        ]

{- | The @mPostProc@ hook. The 'FilePath' argument is the ILA data directory,
which this demo does not use; the latency dumps live in the HITL directory.
-}
postProc :: FilePath -> ExitCode -> IO (TestStepResult ())
postProc _ilaDir exitCode = do
  result <- try go
  case (result, exitCode) of
    (Left (e :: SomeException), _) -> pure (TestStepFailure (show e))
    (Right (), ExitSuccess) -> pure (TestStepSuccess ())
    (Right (), _) -> pure (TestStepFailure "test exited non-zero")
 where
  go = do
    projectDir <- findParentContaining "cabal.project"
    let hitlDir = projectDir </> "_build/hitl" </> "Decode_Demo_DUT"
    dumps <- loadDumps hitlDir
    writeFile (hitlDir </> "decode-demo-hist.csv") (histCsv dumps)
    writeFile (hitlDir </> "decode-demo-summary.txt") (summary dumps)
    putStrLn $ "Decode demo post-processing: " <> show (length dumps) <> " dumps"
    putStrLn $ summary dumps
