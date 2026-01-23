-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE LambdaCase #-}

{- |
Utilities for collecting and comparing UGN edges in a HITL test. UGN edges represent
clock domain translations between nodes in a distributed system.

= Overview

The module supports two methods of obtaining UGN information:

1. __Hardware Capture__: Direct counter snapshots from 'Bittide.CaptureUgn.captureUgn'
   - Measures propagation delay automatically when a link comes up
   - Provides raw counter values (local and remote), a pair also known as a "timing oracle".

2. __Software Discovery__: Message-based protocol over the network
   - Measures propagation delay through message exchange
   - Provides both incoming and outgoing edge perspectives
-}
module Bittide.Instances.Hitl.Utils.Ugn where

import Prelude

import Clash.Prelude (BitVector, Index, Natural, Unsigned, bitCoerce)

import Bittide.Instances.Hitl.Setup
import Control.Monad (forM_, unless, when)
import Data.Either.Extra (eitherToMaybe, mapLeft)
import Data.Functor (void)
import Data.List (findIndex)
import Data.Maybe (fromJust, mapMaybe)
import Data.String.Interpolate (i)
import Numeric (showHex)
import Project.Handle
import Text.Parsec
import Text.Parsec.String (Parser)

import qualified Clash.Prelude as CP
import qualified Data.List as L
import qualified System.IO as IO

-- * Data Types

-- | Counter values captured from hardware UGN capture units.
data CounterCapture = CounterCapture
  { port :: Index LinkCount
  -- ^ The port number where the capture occurred
  , local :: (Unsigned 64)
  -- ^ Local counter value at capture time
  , remote :: (Unsigned 64)
  -- ^ Remote counter value received in first frame
  }
  deriving (Show, Eq)

{- | Intermediate representation containing complete timing information.

Represents a complete UGN edge with source and destination node/port information
along with the counter values captured at each end.
-}
data TimingOracle = TimingOracle
  { srcNode :: BitVector 32
  -- ^ Source node ID (from DNA)
  , srcPort :: Index LinkCount
  -- ^ Source port number
  , dstNode :: BitVector 32
  -- ^ Destination node ID (from DNA)
  , dstPort :: Index LinkCount
  -- ^ Destination port number
  , srcCounter :: (Unsigned 64)
  -- ^ Counter value at source (remote counter from capture)
  , dstCounter :: (Unsigned 64)
  -- ^ Counter value at destination (local counter from capture)
  }
  deriving (Show, Eq)

{- | A UGN edge representing clock domain translation between two nodes.

Each edge describes the propagation delay from a source node/port to a destination
node/port. The UGN value is a signed integer representing the delay in clock cycles.

In hardware captures, this represents the instantaneous counter difference.
In software discovery, this represents the measured one-way propagation delay.
-}
data UgnEdge = UgnEdge
  { srcNode :: CP.BitVector 32
  -- ^ Source node ID
  , srcPort :: CP.Index LinkCount
  -- ^ Source port number
  , dstNode :: CP.BitVector 32
  -- ^ Destination node ID
  , dstPort :: CP.Index LinkCount
  -- ^ Destination port number
  , ugn :: (CP.Signed 64)
  -- ^ Propagation delay in clock cycles (signed, can be negative due to clock skew)
  }
  deriving (Eq, Ord)

data RoundTripLatency = RoundTripLatency
  { node :: CP.BitVector 32
  -- ^ Node ID where measurement was taken
  , port :: CP.Index LinkCount
  -- ^ Port number
  , cycles :: (CP.Unsigned 64)
  -- ^ Roundtrip latency in clock cycles
  }
  deriving (Show, Eq)

-- | Adjust for additional latency in a ugn edge
addLatencyEdge :: Int -> UgnEdge -> UgnEdge
addLatencyEdge extra edge = edge{ugn = edge.ugn + fromIntegral extra}

-- | Adjust for additional latency in a roundtrip latency
adjustLatencyRoundTrip :: Int -> RoundTripLatency -> RoundTripLatency
adjustLatencyRoundTrip extra rtl = rtl{cycles = rtl.cycles + fromIntegral extra}

{- | Calculate roundtrip latencies from UGN edges.

For each node and port, finds the pair of edges going out and coming back,
then computes the roundtrip latency as: UGN(a:x -> b:y) + UGN(b:y -> a:x)

This matches the algorithm used in the C code (PRINT_ROUNDTRIP_LATENCIES)
and Bittide.Calculator (printAllIgns).

Returns a list of roundtrip latencies, one for each port of each node that
has both outgoing and return edges.
-}
calculateRoundtripLatencies :: [UgnEdge] -> [RoundTripLatency]
calculateRoundtripLatencies edges = mapMaybe computeRoundtrip edges
 where
  computeRoundtrip :: UgnEdge -> Maybe RoundTripLatency
  computeRoundtrip outEdge = do
    returnEdge <- maybeReturnEdge
    Just $
      RoundTripLatency
        { node = outEdge.srcNode
        , port = outEdge.srcPort
        , cycles = fromIntegral (outEdge.ugn + returnEdge.ugn)
        }
   where
    -- Find the return edge: from (dstNode, dstPort) back to (srcNode, srcPort)
    maybeReturnEdge =
      L.find
        ( \e ->
            e.srcNode == outEdge.dstNode
              && e.srcPort == outEdge.dstPort
              && e.dstNode == outEdge.srcNode
              && e.dstPort == outEdge.srcPort
        )
        edges

-- | Custom Show instance for UgnEdge that displays node IDs in hexadecimal.
instance Show UgnEdge where
  show edge =
    let srcNodeHex = showHex (fromIntegral edge.srcNode :: Integer) ""
        srcPortVal = edge.srcPort
        dstNodeHex = showHex (fromIntegral edge.dstNode :: Integer) ""
        dstPortVal = edge.dstPort
        ugnVal = edge.ugn
     in [i|UgnEdge {srcNode = 0x#{srcNodeHex}, srcPort = #{srcPortVal}, dstNode = 0x#{dstNodeHex}, dstPort = #{dstPortVal}, ugn = #{ugnVal}}|]

-- * Conversion functions

{- | Convert node index to node ID using DNA.

Extracts the lower 32 bits of the FPGA DNA to form the node identifier.
-}
indexToNodeId :: CP.Index FpgaCount -> BitVector 32
indexToNodeId idx = CP.resize (demoRigInfo L.!! fromIntegral idx).dna

{- | Convert hardware counter capture to timing oracle.

Takes a destination node index and counter capture, and constructs a complete
timing oracle by looking up the source node from the link configuration and
extracting node IDs from DNA values.
-}
counterCaptureToTimingOracle :: CP.Index FpgaCount -> CounterCapture -> TimingOracle
counterCaptureToTimingOracle dstIndex cc =
  let
    srcNodeId = indexToNodeId srcIndex -- node ID from DNA
    srcIndex = (knownLinkConfigs CP.!! dstIndex) CP.!! cc.port -- index of destination node from fpgaSetup
    dstNodeId = indexToNodeId dstIndex -- node ID from DNA
    dstPortId =
      fromIntegral . fromJust $
        findIndex (== srcIndex) $
          CP.toList (knownLinkConfigs CP.!! dstIndex)
   in
    TimingOracle
      { srcNode = srcNodeId
      , srcPort = cc.port
      , dstNode = dstNodeId
      , dstPort = dstPortId
      , srcCounter = cc.remote
      , dstCounter = cc.local
      }

{- | Convert timing oracle to UGN edge.

Computes the UGN value as the signed difference: @dstCounter - srcCounter@.
This represents the propagation delay from source to destination in clock cycles.
-}
timingOracleToUgnEdge :: TimingOracle -> UgnEdge
timingOracleToUgnEdge oracle =
  UgnEdge
    { srcNode = oracle.srcNode
    , srcPort = oracle.srcPort
    , dstNode = oracle.dstNode
    , dstPort = oracle.dstPort
    , ugn = bitCoerce oracle.dstCounter - bitCoerce oracle.srcCounter
    }

{- | Compare hardware and software UGN edges with detailed difference reporting.

Performs edge-by-edge comparison and reports:

* Total edge counts
* Missing edges (present in one list but not the other)
* Mismatched edges with field-by-field difference analysis

Returns 'True' if edges match exactly, 'False' otherwise.
-}
compareRoundtripLatencies :: [RoundTripLatency] -> [RoundTripLatency] -> IO Bool
compareRoundtripLatencies hwRoundtrips swRoundtrips = do
  putStrLn "\n=== Roundtrip Latency Comparison ==="
  putStrLn [i|Total hardware roundtrips: #{L.length hwRoundtrips}|]
  putStrLn [i|Total software roundtrips: #{L.length swRoundtrips}|]

  -- Match roundtrips by node and port
  let matchRoundtrip hw =
        L.find
          ( \sw ->
              sw.node == hw.node
                && sw.port == hw.port
          )
          swRoundtrips

      hwMatches = [(hw, matchRoundtrip hw) | hw <- hwRoundtrips]
      matchedSwRoundtrips = [sw | (_, Just sw) <- hwMatches]
      unmatchedSwRoundtrips = L.filter (`notElem` matchedSwRoundtrips) swRoundtrips

      allMatch =
        all
          ( \(hw, mSw) -> case mSw of
              Just sw -> hw.cycles == sw.cycles
              Nothing -> False
          )
          hwMatches
          && null unmatchedSwRoundtrips

  if allMatch && L.length hwRoundtrips == L.length swRoundtrips
    then do
      putStrLn "[SUCCESS] All roundtrip latencies match!"
      return True
    else do
      putStrLn "[ERROR] Roundtrip latencies differ!\n"

      -- Report hardware roundtrips and their matches
      forM_ (L.zip [0 :: Int ..] hwMatches) $ \(idx, (hw, mSw)) -> do
        case mSw of
          Nothing -> do
            putStrLn [i|\n[#{idx}] Hardware roundtrip NOT FOUND in software:|]
            putStrLn [i|  HW: #{hw}|]
          Just sw -> do
            when (hw.cycles /= sw.cycles) $ do
              let hwNodeHex = showHex (fromIntegral hw.node :: Integer) ""
                  hwPort = hw.port
                  hwCycles = hw.cycles
                  swNodeHex = showHex (fromIntegral sw.node :: Integer) ""
                  swPort = sw.port
                  swCycles = sw.cycles
                  delta = abs (fromIntegral hw.cycles - fromIntegral sw.cycles :: Integer)
              putStrLn [i|\n[#{idx}] Roundtrip latency differs:|]
              putStrLn [i|  HW: Node 0x#{hwNodeHex} Port #{hwPort} = #{hwCycles} cycles|]
              putStrLn [i|  SW: Node 0x#{swNodeHex} Port #{swPort} = #{swCycles} cycles|]
              putStrLn [i|    ↳ Delta: #{delta} cycles|]

      -- Report software roundtrips not in hardware
      unless (null unmatchedSwRoundtrips) $ do
        putStrLn "\n=== Software roundtrips NOT FOUND in hardware ==="
        forM_ unmatchedSwRoundtrips $ \sw -> do
          putStrLn [i|  SW: #{sw}|]

      return False

compareUgnEdges :: [UgnEdge] -> [UgnEdge] -> IO Bool
compareUgnEdges hardwareUgns softwareUgns = do
  putStrLn "\n=== Detailed Edge Comparison ==="
  putStrLn [i|Total hardware UGN edges: #{L.length hardwareUgns}|]
  putStrLn [i|Total software UGN edges: #{L.length softwareUgns}|]

  -- Compare edges by matching source and destination nodes
  let matchEdge hw =
        L.find
          ( \sw ->
              sw.srcNode == hw.srcNode
                && sw.dstNode == hw.dstNode
          )
          softwareUgns

      hwMatches = [(hw, matchEdge hw) | hw <- hardwareUgns]
      matchedSwEdges = [sw | (_, Just sw) <- hwMatches]
      unmatchedSwEdges = L.filter (`notElem` matchedSwEdges) softwareUgns

      allMatch =
        all
          ( \(hw, mSw) -> case mSw of
              Just sw -> hw.ugn == sw.ugn
              Nothing -> False
          )
          hwMatches
          && null unmatchedSwEdges

  if allMatch && L.length hardwareUgns == L.length softwareUgns
    then do
      putStrLn "[SUCCESS] All UGN edges match!"
      return True
    else do
      putStrLn "[ERROR] UGN edges differ!\n"

      -- Report hardware edges and their matches
      forM_ (L.zip [0 :: Int ..] hwMatches) $ \(idx, (hw, mSw)) -> do
        case mSw of
          Nothing -> do
            putStrLn [i|\n[#{idx}] Hardware edge NOT FOUND in software:|]
            putStrLn [i|  HW: #{hw}|]
          Just sw -> do
            when (hw.ugn /= sw.ugn) $ do
              let delta = abs (hw.ugn - sw.ugn)
              putStrLn [i|\n[#{idx}] UGN value differs:|]
              putStrLn [i|  HW: #{hw}|]
              putStrLn [i|  SW: #{sw}|]
              putStrLn [i|    ↳ UGN delta: #{delta} cycles|]

      -- Report software edges not in hardware
      unless (null unmatchedSwEdges) $ do
        putStrLn "\n=== Software edges NOT FOUND in hardware ==="
        forM_ unmatchedSwEdges $ \sw -> do
          putStrLn [i|  SW: #{sw}|]

      return False

{- | Find UGN edges that don't match between hardware and software.

Returns a list of pairs @(hardwareEdge, softwareEdge)@ where both edges have the
same source and destination nodes, but differ in port numbers or UGN values.

This is useful for identifying wiring or configuration mismatches where the
logical connection exists but the physical details differ.
-}
findMismatchedUgnEdges :: [UgnEdge] -> [UgnEdge] -> [(UgnEdge, UgnEdge)]
findMismatchedUgnEdges hardwareUgns softwareUgns =
  mapMaybe findMismatch hardwareUgns
 where
  findMismatch hw =
    case L.find (matchingSrcDst hw) softwareUgns of
      Just sw
        | hw.srcPort /= sw.srcPort
            || hw.dstPort /= sw.dstPort
            || hw.ugn /= sw.ugn ->
            Just (hw, sw)
      _ -> Nothing
  matchingSrcDst a b =
    a.srcNode == b.srcNode && a.dstNode == b.dstNode

-- * UGN Processing Functions

{- | Post-process hardware UGN edges from multiple nodes.

Flattens the per-node edge lists and sorts them for comparison.
-}
postProcessHardwareUgns :: [[UgnEdge]] -> [UgnEdge]
postProcessHardwareUgns hardwareUgnsPerNode = L.sort $ L.concat hardwareUgnsPerNode

{- | Post-process software UGN edges from multiple nodes.

Combines incoming and outgoing edges, deduplicates them (each edge appears twice
in the software protocol), and sorts the result.
-}
postProcessSoftwareUgns :: [([UgnEdge], [UgnEdge])] -> IO [UgnEdge]
postProcessSoftwareUgns softwareUgnsPerNode = do
  let (softwareUgnsIn, softwareUgnsOut) = L.unzip softwareUgnsPerNode
      softwareUgnsFlat = L.sort $ L.concat softwareUgnsIn <> L.concat softwareUgnsOut

  L.sort <$> deduplicateSoftwareUgns softwareUgnsFlat

{- | Deduplicate software UGN edges that appear in both incoming and outgoing lists.

In the software UGN discovery protocol, each edge appears twice:
once as an incoming edge and once as an outgoing edge.

This function validates that edges appear exactly twice and returns one copy of each.
Throws an error if any edge appears an incorrect number of times.
-}
deduplicateSoftwareUgns :: [UgnEdge] -> IO [UgnEdge]
deduplicateSoftwareUgns edges = do
  let grouped = L.groupBy (\a b -> (a.srcNode, a.dstNode) == (b.srcNode, b.dstNode)) edges -- Group identical edges

  -- Check each group and validate, returning one copy of each edge
  mapM validateAndExtract grouped
 where
  validateAndExtract :: [UgnEdge] -> IO UgnEdge
  validateAndExtract [edge0, edge1] =
    if edge0 /= edge1
      then
        error [i|[ERROR] Edge appears twice but with different UGN values: #{[edge0, edge1]}|]
      else pure edge0
  validateAndExtract groupedEdge = do
    putStrLn
      [i|[ERROR] Edge appears #{L.length groupedEdge} times (expected twice): #{groupedEdge}|]
    error "Software UGN edge appears incorrect number of times"

-- * Parsing Functions

{- | Parse hardware UGN counter captures from UART output.

Reads lines between @[MU] Starting UGN captures@ and @[MU] All UGNs captured@,
parsing each counter capture line into a 'CounterCapture' structure.

The hardware capture unit snapshots both local and remote counters when a link comes up.
-}
parseCaptureCounters :: IO.Handle -> IO [CounterCapture]
parseCaptureCounters handle = do
  -- Collect all lines until we see the end marker
  waitForLine handle "[MU] Starting UGN captures"
  capturedLines <- readUntilLine handle "[MU] All UGNs captured"
  putStrLn [i|Got captured lines: #{capturedLines}|]
  let parseResults = fmap (runParser parseCounterCapture () "counter captures") capturedLines

  -- Parse using high-level parser
  mapM (expectRight . mapLeft show) parseResults

{- | Parse software UGN edges from processing element (PE) output.

Reads UGN edges discovered through the software protocol, which exchanges messages
to measure propagation delays. Returns separate lists for incoming and outgoing edges.

Software UGN discovery measures one-way delays by timestamping messages as they travel
between nodes.
-}
parseSoftwareUgns :: IO.Handle -> IO ([UgnEdge], [UgnEdge])
parseSoftwareUgns handle = do
  -- Collect all lines until we see the completion marker
  waitForLine handle "[PE] Incoming Link UGNs:"
  incomingLines <- readUntilLine handle "[PE] Outgoing Link UGNs:"
  outgoingLines <- readUntilLine handle "[PE] End of UGN Edge edges"
  let
    parseResultsIncoming = fmap (runParser parseUgnEdge () "incoming ugn edges") incomingLines
    parseResultsOutgoing = fmap (runParser parseUgnEdge () "outgoing ugn edges") outgoingLines
    validIncomingEdges = mapMaybe (eitherToMaybe . mapLeft show) parseResultsIncoming
    validOutgoingEdges = mapMaybe (eitherToMaybe . mapLeft show) parseResultsOutgoing

  return (validIncomingEdges, validOutgoingEdges)

{- | Parse a counter capture line from management unit output.

Expected format: @[MU] Capture UGN N: local = L, remote = R@
where N is the port number, L is the local counter, and R is the remote counter.
-}
parseCounterCapture :: Parser CounterCapture
parseCounterCapture = do
  cpuPrefix "MU"
  spaces
  _ <- string "Capture UGN "
  idx <- parseIndex
  _ <- string ": local = "
  local <- parseUnsigned
  spaces
  _ <- string ", remote = "
  remote <- parseUnsigned
  return $ CounterCapture idx local remote

{- | Parse a UGN edge from processing element output.

Expected format:
@[PE] Port N: src_node=S, src_port=SP, dst_node=D, dst_port=DP, ugn=U@
-}
parseUgnEdge :: Parser UgnEdge
parseUgnEdge = do
  cpuPrefix "PE"
  spaces
  _ <- string "Port "
  _ <- parseIndex @LinkCount -- port number not used in edge construction
  _ <- string ": src_node="
  srcNode <- parseBitVector
  _ <- string ", src_port="
  srcPort <- parseIndex
  _ <- string ", dst_node="
  dstNode <- parseBitVector
  _ <- string ", dst_port="
  dstPort <- parseIndex
  _ <- string ", ugn="
  ugnVal <- parseSigned
  return $ UgnEdge srcNode srcPort dstNode dstPort ugnVal

{- | Parse a CPU prefix marker from UART output.

Matches prefixes like @[MU]@, @[PE]@, @[CC]@, etc. that identify which
processing unit generated the output line.

Example:

@
cpuPrefix "MU"  -- matches "[MU]"
cpuPrefix "PE"  -- matches "[PE]"
@
-}
cpuPrefix :: String -> Parser ()
cpuPrefix name = void $ string [i|[#{name}]|]

{- | Parse any CPU prefix and return the prefix string.

Matches any string within square brackets and returns the content.

Example:

@
someCpuPrefix  -- matches "[MU]" and returns "MU"
@
-}
someCpuPrefix :: Parser String
someCpuPrefix = do
  _ <- char '['
  prefix <- manyTill anyChar (char ']')
  return prefix

-- * Parser Primitives

-- | Parse a signed integer (with optional leading minus sign).
parseInteger :: Parser Integer
parseInteger = do
  sign <- optionMaybe (char '-')
  digits <- many1 digit
  let val = read digits
  return $ case sign of
    Just _ -> -val
    Nothing -> val

-- | Parse a natural number (non-negative integer).
parseNatural :: Parser Natural
parseNatural = read <$> many1 digit

-- | Parse an unsigned integer with bounds checking for the target type width.
parseUnsigned :: forall n. (CP.KnownNat n) => Parser (CP.Unsigned n)
parseUnsigned = do
  val <- parseNatural
  if val > fromIntegral (maxBound :: CP.Unsigned n)
    then fail [i|Value #{val} exceeds maximum for Unsigned #{CP.natToNatural @n}|]
    else return $ fromIntegral val

-- | Parse a signed integer with bounds checking for the target type width.
parseSigned :: forall n. (CP.KnownNat n) => Parser (CP.Signed n)
parseSigned = do
  val <- parseInteger
  let maxVal = fromIntegral (maxBound :: CP.Signed n)
      minVal = fromIntegral (minBound :: CP.Signed n)
  if val < minVal || val > maxVal
    then fail [i|Value #{val} out of bounds for Signed #{CP.natToNatural @n}|]
    else return $ fromIntegral val

-- | Parse a bit vector with bounds checking for the target width.
parseBitVector :: forall n. (CP.KnownNat n) => Parser (BitVector n)
parseBitVector = do
  val <- parseNatural
  if val > (fromIntegral (maxBound :: BitVector n))
    then fail [i|Value #{val} exceeds maximum for BitVector #{CP.natToNatural @n}|]
    else return $ fromIntegral val

-- | Parse an index value with bounds checking.
parseIndex :: forall n. (CP.KnownNat n) => Parser (Index n)
parseIndex = do
  val <- parseNatural
  if val > fromIntegral (maxBound :: Index n)
    then fail [i|Value #{val} exceeds maximum for Index #{CP.natToNatural @n}|]
    else return $ fromIntegral val
