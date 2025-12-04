-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

{- |
This module provides utilities for collecting and comparing UGN edges in a HITL test.
UGN edges represent clock domain translations between nodes in a distributed system.

= Overview

The module supports two methods of obtaining UGN information:

1. __Hardware Capture__: Direct counter snapshots from hardware capture units
   - Measures propagation delay automaticatlly when a link comes up
   - Provides raw counter values (local and remote), a pair also known as a "timing oracle".

2. __Software Discovery__: Message-based protocol over the network
   - Measures propagation delay through message exchange
   - Provides both incoming and outgoing edge perspectives
-}
module Bittide.Instances.Hitl.Utils.Ugn where

import Prelude

import Clash.Prelude (BitVector, Index, Natural, Unsigned, bitCoerce)

import Bittide.Instances.Hitl.Setup
import Control.Monad (forM_, when)
import Data.Either.Extra (mapLeft)
import Data.Functor (void)
import Data.List (findIndex)
import Data.Maybe (fromJust)
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
  { ccPort :: Index LinkCount
  -- ^ The port number where the capture occurred
  , ccLocal :: (Unsigned 64)
  -- ^ Local counter value at capture time
  , ccRemote :: (Unsigned 64)
  -- ^ Remote counter value received in first frame
  }
  deriving (Show, Eq)

{- | Intermediate representation containing complete timing information.

Represents a complete UGN edge with source and destination node/port information
along with the counter values captured at each end.
-}
data TimingOracle = TimingOracle
  { toSrcNode :: BitVector 32
  -- ^ Source node ID (from DNA)
  , toSrcPort :: Index LinkCount
  -- ^ Source port number
  , toDstNode :: BitVector 32
  -- ^ Destination node ID (from DNA)
  , toDstPort :: Index LinkCount
  -- ^ Destination port number
  , toSrcCounter :: (Unsigned 64)
  -- ^ Counter value at source (remote counter from capture)
  , toDstCounter :: (Unsigned 64)
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
  { ueSrcNode :: CP.BitVector 32
  -- ^ Source node ID
  , ueSrcPort :: CP.Index LinkCount
  -- ^ Source port number
  , ueDstNode :: CP.BitVector 32
  -- ^ Destination node ID
  , ueDstPort :: CP.Index LinkCount
  -- ^ Destination port number
  , ueUgn :: (CP.Signed 64)
  -- ^ Propagation delay in clock cycles (signed, can be negative due to clock skew)
  }
  deriving (Eq, Ord)

data RoundTripLatency = RoundTripLatency
  { rtlNode :: CP.BitVector 32
  -- ^ Node ID where measurement was taken
  , rtlPort :: CP.Index LinkCount
  -- ^ Port number
  , rtlCycles :: (CP.Unsigned 64)
  -- ^ Roundtrip latency in clock cycles
  }
  deriving (Show, Eq)

-- | Custom Show instance for UgnEdge that displays node IDs in hexadecimal.
instance Show UgnEdge where
  show edge =
    "UgnEdge {ueSrcNode = 0x"
      <> showHex (fromIntegral (edge.ueSrcNode) :: Integer) ""
      <> ", ueSrcPort = "
      <> show edge.ueSrcPort
      <> ", ueDstNode = 0x"
      <> showHex (fromIntegral (edge.ueDstNode) :: Integer) ""
      <> ", ueDstPort = "
      <> show edge.ueDstPort
      <> ", ueUgn = "
      <> show edge.ueUgn
      <> "}"

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
    srcNode = indexToNodeId srcIndex -- node ID from DNA
    srcIndex = (knownLinkConfigs CP.!! dstIndex) CP.!! cc.ccPort -- index of destination node from fpgaSetup
    dstNode = indexToNodeId dstIndex -- node ID from DNA
    dstPort =
      fromIntegral . fromJust $
        findIndex (== srcIndex) $
          CP.toList (knownLinkConfigs CP.!! dstIndex)
   in
    -- dstPort = (knownLinkConfigs CP.!! dstIndex) CP.!! fromIntegral (cc.ccPort) -- port from fpgaSetup by index and dstNode
    TimingOracle
      { toSrcNode = srcNode
      , toSrcPort = cc.ccPort
      , toDstNode = dstNode
      , toDstPort = dstPort
      , toSrcCounter = cc.ccRemote
      , toDstCounter = cc.ccLocal
      }

{- | Convert timing oracle to UGN edge.

Computes the UGN value as the signed difference: @dstCounter - srcCounter@.
This represents the propagation delay from source to destination in clock cycles.
-}
timingOracleToUgnEdge :: TimingOracle -> UgnEdge
timingOracleToUgnEdge oracle =
  UgnEdge
    { ueSrcNode = oracle.toSrcNode
    , ueSrcPort = oracle.toSrcPort
    , ueDstNode = oracle.toDstNode
    , ueDstPort = oracle.toDstPort
    , ueUgn = bitCoerce oracle.toDstCounter - bitCoerce oracle.toSrcCounter
    }

{- | Compare hardware and software UGN edges with detailed difference reporting.

Performs edge-by-edge comparison and reports:

* Total edge counts
* Missing edges (present in one list but not the other)
* Mismatched edges with field-by-field difference analysis

Returns 'True' if edges match exactly, 'False' otherwise.
-}
compareUgnEdges :: [UgnEdge] -> [UgnEdge] -> IO Bool
compareUgnEdges hardwareUgns softwareUgns = do
  putStrLn "\n=== Detailed Comparison ==="
  putStrLn $ "Total hardware UGN edges: " <> show (L.length hardwareUgns)
  putStrLn $ "Total software UGN edges: " <> show (L.length softwareUgns)

  if hardwareUgns == softwareUgns
    then do
      putStrLn "[SUCCESS] All UGN edges match!"
      return True
    else do
      putStrLn "[FAILURE] UGN edges differ!\n"

      -- Compare edge by edge
      let maxLen = max (L.length hardwareUgns) (L.length softwareUgns)
          hwPadded = hardwareUgns L.++ L.replicate (maxLen - L.length hardwareUgns) (error "No HW edge")
          swPadded = softwareUgns L.++ L.replicate (maxLen - L.length softwareUgns) (error "No SW edge")

      forM_ (L.zip3 [0 :: Int ..] hwPadded swPadded) $ \(idx, hw, sw) -> do
        if idx >= L.length hardwareUgns
          then
            putStrLn $
              "\n["
                <> show idx
                <> "] MISSING in hardware, present in software:"
                <> "\n  SW: "
                <> show sw
          else
            if idx >= L.length softwareUgns
              then
                putStrLn $
                  "\n["
                    <> show idx
                    <> "] MISSING in software, present in hardware:"
                    <> "\n  HW: "
                    <> show hw
              else
                if hw /= sw
                  then do
                    putStrLn $ "\n[" <> show idx <> "] MISMATCH:"
                    putStrLn $ "  HW: " <> show hw
                    putStrLn $ "  SW: " <> show sw

                    -- Analyze the differences
                    when (hw.ueSrcNode /= sw.ueSrcNode) $
                      putStrLn $
                        "    ↳ Source node differs: "
                          <> show hw.ueSrcNode
                          <> " vs "
                          <> show sw.ueSrcNode
                    when (hw.ueSrcPort /= sw.ueSrcPort) $
                      putStrLn $
                        "    ↳ Source port differs: "
                          <> show hw.ueSrcPort
                          <> " vs "
                          <> show sw.ueSrcPort
                    when (hw.ueDstNode /= sw.ueDstNode) $
                      putStrLn $
                        "    ↳ Dest node differs: "
                          <> show hw.ueDstNode
                          <> " vs "
                          <> show sw.ueDstNode
                    when (hw.ueDstPort /= sw.ueDstPort) $
                      putStrLn $
                        "    ↳ Dest port differs: "
                          <> show hw.ueDstPort
                          <> " vs "
                          <> show sw.ueDstPort
                    when (hw.ueUgn /= sw.ueUgn) $
                      putStrLn $
                        "    ↳ UGN value differs: "
                          <> show hw.ueUgn
                          <> " vs "
                          <> show sw.ueUgn
                          <> " (delta: "
                          <> show (abs (hw.ueUgn - sw.ueUgn))
                          <> ")"
                  else
                    pure () -- Edges match
      return False

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
      softwareUgnsFlat = L.sort $ L.concat softwareUgnsIn L.++ L.concat softwareUgnsOut

  L.sort <$> deduplicateSoftwareUgns softwareUgnsFlat

{- | Deduplicate software UGN edges that appear in both incoming and outgoing lists.

In the software UGN discovery protocol, each edge appears twice:
once as an incoming edge and once as an outgoing edge.

This function validates that edges appear exactly twice and returns one copy of each.
Throws an error if any edge appears an incorrect number of times.
-}
deduplicateSoftwareUgns :: [UgnEdge] -> IO [UgnEdge]
deduplicateSoftwareUgns edges = do
  let grouped = L.groupBy (\a b -> (a.ueSrcNode, a.ueDstNode) == (b.ueSrcNode, b.ueDstNode)) edges -- Group identical edges

  -- Check each group and validate, returning one copy of each edge
  mapM validateAndExtract grouped
 where
  validateAndExtract :: [UgnEdge] -> IO UgnEdge
  validateAndExtract [] = error "Empty group in deduplicateSoftwareUgns (should not happen)"
  validateAndExtract group@(edge : _) =
    case L.length group of
      1 -> do
        putStrLn $ "ERROR: Edge appears only once (expected twice): " <> show edge
        error "Software UGN edge appears only once"
      2 -> do
        if group L.!! 0 /= group L.!! 1
          then
            putStrLn $ "WARNING: Edge appears twice but with different UGN values: " <> show group
          else pure ()
        -- Normal case: edge appears twice (incoming and outgoing)
        return edge
      n -> do
        putStrLn $ "ERROR: Edge appears " <> show n <> " times (expected twice): " <> show group
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
  putStrLn $ "Got captured lines: " <> show capturedLines
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
  let parseResultsIncoming = fmap (runParser parseUgnEdge () "incoming ugn edges") incomingLines
  validIncomingEdges <- mapM (expectRight . mapLeft show) parseResultsIncoming
  outgoingLines <- readUntilLine handle "[PE] End of UGN Edge edges"
  let parseResultsOutgoing = fmap (runParser parseUgnEdge () "outgoing ugn edges") outgoingLines
  validOutgoingEdges <- mapM (expectRight . mapLeft show) parseResultsOutgoing

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
  idx <- index
  _ <- string ": local = "
  local <- unsigned
  spaces
  _ <- string ", remote = "
  remote <- unsigned
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
  _ <- index @LinkCount -- port number not used in edge construction
  _ <- string ": src_node="
  srcNode <- bitVector
  _ <- string ", src_port="
  srcPort <- index
  _ <- string ", dst_node="
  dstNode <- bitVector
  _ <- string ", dst_port="
  dstPort <- index
  _ <- string ", ugn="
  ugnVal <- signed
  return $ UgnEdge srcNode srcPort dstNode dstPort ugnVal

-- * Parser Primitives

-- | Parse a signed integer (with optional leading minus sign).
integer :: Parser Integer
integer = do
  sign <- optionMaybe (char '-')
  digits <- many1 digit
  let val = read digits
  return $ case sign of
    Just _ -> -val
    Nothing -> val

-- | Parse a natural number (non-negative integer).
natural :: Parser Natural
natural = read <$> many1 digit

-- | Parse an unsigned integer with bounds checking for the target type width.
unsigned :: forall n. (CP.KnownNat n) => Parser (CP.Unsigned n)
unsigned = do
  val <- natural
  if val > fromIntegral (maxBound :: CP.Unsigned n)
    then
      fail $
        "Value " ++ show val ++ " exceeds maximum for Unsigned " ++ show (CP.natToNatural @n)
    else return $ fromIntegral val

-- | Parse a signed integer with bounds checking for the target type width.
signed :: forall n. (CP.KnownNat n) => Parser (CP.Signed n)
signed = do
  val <- integer
  let maxVal = fromIntegral (maxBound :: CP.Signed n)
      minVal = fromIntegral (minBound :: CP.Signed n)
  if val < minVal || val > maxVal
    then
      fail $ "Value " ++ show val ++ " out of bounds for Signed " ++ show (CP.natToNatural @n)
    else return $ fromIntegral val

-- | Parse a bit vector with bounds checking for the target width.
bitVector :: forall n. (CP.KnownNat n) => Parser (BitVector n)
bitVector = do
  val <- natural
  if val > (fromIntegral (maxBound :: BitVector n))
    then
      fail $
        "Value " ++ show val ++ " exceeds maximum for BitVector " ++ show (CP.natToNatural @n)
    else return $ fromIntegral val

-- | Parse an index value with bounds checking.
index :: forall n. (CP.KnownNat n) => Parser (Index n)
index = do
  val <- natural
  if val > fromIntegral (maxBound :: Index n)
    then
      fail $ "Value " ++ show val ++ " exceeds maximum for Index " ++ show (CP.natToNatural @n)
    else return $ fromIntegral val

-- * Parser Combinators

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
cpuPrefix name = void $ string ("[" ++ name ++ "]")

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
