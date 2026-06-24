-- SPDX-FileCopyrightText: 2026 QBayLogic
--
-- SPDX-License-Identifier: Apache-2.0

{- | Generate the Manticore compiler's @latencies.csv@ (the @--hop-latencies@ map)
for the 8-FPGA 2D-torus demo, reusing the WireDemo's golden UGNs — the rig (FPGAs,
links, UGNs) is identical, only the application on top changes.

The 8 ICs form a 2x4 grid of 4x4-core chips (global 8x16 torus). Each IC runs the
SAME RTL and is configured at runtime (its four @seam_*_extend@ pins set after the
FPGAs are programmed), so the topology is purely a compiler concern; 'icTopology' is
its explicit description.

Crucially, the RTL is a FOLDED double-cut torus (see the @Fold@ object in the
manticore-compiler): chaining chips folds each global ring through them, so chip
@(cx, cy)@ owns a NON-contiguous set of global ring positions. The inter-chip seams
therefore sit at the folded positions (X east at x={1,5}, Y north at y={1,3,5,9,11,13},
etc.), NOT at contiguous chip edges. This module derives the seam positions from the
fold ('foldChip') and charges each one the latency of the physical cable it crosses,
found from 'icTopology' + the directed golden UGN of that FPGA pair. The result
reproduces the working hand-built reference positions exactly, with real per-cable
latencies instead of a placeholder constant.
-}
module Bittide.Instances.Hitl.ManticoreDemo.Latencies (
  TorusDir (..),
  Directions (..),
  IcLink (..),
  dirName,
  axisOf,
  icTopology,
  foldChip,
  chipOfX,
  chipOfY,
  icAt,
  chipCols,
  chipRows,
  seamRows,
  seamConfig,
  latenciesCsv,
  writeLatenciesCsv,
) where

import Prelude

import qualified Clash.Prelude as C
import Data.List (elemIndex, find, intercalate)
import Data.Maybe (fromJust)

import Bittide.Instances.Hitl.Setup (FpgaCount, fpgaSetup)
import Bittide.Instances.Hitl.Utils.Ugn (UgnEdge (..), indexToNodeId)
import Bittide.Instances.Hitl.WireDemo.Driver (goldenUgns, internalDelay, marginFrames)

-- | Chip grid: @chipCols x chipRows@ chips of @chipDimX x chipDimY@ cores -> 8x16.
chipCols, chipRows, chipDimX, chipDimY :: Int
chipCols = 2
chipRows = 4
chipDimX = 4
chipDimY = 4

-- | Global torus dimensions.
gDimX, gDimY :: Int
gDimX = chipCols * chipDimX
gDimY = chipRows * chipDimY

-- | TDM slots per link rotation step (matches the chip's @cyclesPerSlot@).
cyclesPerSlot :: Int
cyclesPerSlot = 1

-- | A direction in the 2D torus.
data TorusDir = North | South | East | West
  deriving (Eq, Show, Enum, Bounded)

-- | The compiler's @dir@ column spelling.
dirName :: TorusDir -> String
dirName North = "north"
dirName South = "south"
dirName East = "east"
dirName West = "west"

-- | The axis a bidirectional inter-IC cable lies on.
data Directions = NorthSouth | EastWest
  deriving (Eq, Show)

-- | The cable axis a torus direction travels along.
axisOf :: TorusDir -> Directions
axisOf North = NorthSouth
axisOf South = NorthSouth
axisOf East = EastWest
axisOf West = EastWest

{- | One physical inter-IC cable, BIDIRECTIONAL: it joins the two ICs in @nodes@ along
the @directions@ axis and carries traffic both ways (the TDM bridge multiplexes the
forward and backward NoC links onto the one transceiver).
-}
data IcLink = IcLink
  { nodes :: (Int, Int)
  , directions :: Directions
  }
  deriving (Eq, Show)

{- | The explicit inter-IC cabling of the 2x4 grid. IC id = @cy*chipCols + cx@, which
is also the WireDemo FPGA node index. The folded torus chains chips
neighbour-to-neighbour with NO wrap cable (the chain ends U-turn internally), giving 10
bidirectional cables: one per row on the 'EastWest' axis (the 2-wide dimension collapses
each row's two ring edges onto a single cable) and the three adjacent-chip cables per
column on the 'NorthSouth' axis.
-}
icTopology :: [IcLink]
icTopology =
  [ -- EastWest cables — one per row (cx=0 <-> cx=1)
    IcLink{nodes = (0, 1), directions = EastWest}
  , IcLink{nodes = (2, 3), directions = EastWest}
  , IcLink{nodes = (4, 5), directions = EastWest}
  , IcLink{nodes = (6, 7), directions = EastWest}
  , -- NorthSouth cables, column cx=0 (IC 0,2,4,6) — adjacent chips only, no wrap
    IcLink{nodes = (0, 2), directions = NorthSouth}
  , IcLink{nodes = (2, 4), directions = NorthSouth}
  , IcLink{nodes = (4, 6), directions = NorthSouth}
  , -- NorthSouth cables, column cx=1 (IC 1,3,5,7)
    IcLink{nodes = (1, 3), directions = NorthSouth}
  , IcLink{nodes = (3, 5), directions = NorthSouth}
  , IcLink{nodes = (5, 7), directions = NorthSouth}
  ]

{- | The folded double-cut torus chip-of-ring-position map (must match the
manticore-compiler @Fold.chip@): which chip (0..n-1) of an n-chip dimension of width w
owns global ring position g. Out-chain (g < n*h, h=w/2) ascends; the far end U-turns
and the back-chain descends.
-}
foldChip :: Int -> Int -> Int -> Int
foldChip g n w
  | odd w =
      error
        ("foldChip: chip dimension must be even (the RTL double-cut requires even dims), got w=" <> show w)
  | g < n * h = g `div` h
  | otherwise = n - 1 - (g - n * h) `div` h
 where
  h = w `div` 2

-- | Which chip column / row owns global X / Y coordinate.
chipOfX, chipOfY :: Int -> Int
chipOfX x = foldChip x chipCols chipDimX
chipOfY y = foldChip y chipRows chipDimY

-- | IC id of grid cell (cx, cy).
icAt :: Int -> Int -> Int
icAt cx cy = cy * chipCols + cx

-- | The IC a global core (x, y) lives on.
icOfCore :: Int -> Int -> Int
icOfCore x y = icAt (chipOfX x) (chipOfY y)

{- | Per-FPGA-node inter-chip seam configuration: for each torus edge, whether it is
wired to a grid neighbour (the @seam_<edge>_extend@ bit) and, if so, which Bittide link
on this FPGA carries that edge's TDM frame (the @seam_<edge>_link@ index).

The chip grid is a plain NON-WRAPPING mesh: the global-torus wraps are U-turned inside
the END chips by the fold (see 'foldChip'), so an edge extends iff a grid neighbour
exists in that direction (corner chips extend 2 edges, edge chips 3, interior 4). The
authoritative wiring is the @MultiChipTdmSimKernel@: @east(cx,cy)<->west(cx+1,cy)@,
@north(cx,cy)<->south(cx,cy+1)@, @extend := neighbour-exists@. The link index is this
FPGA's link to the neighbour IC (== FPGA node id), found in 'fpgaSetup' exactly like the
WireDemo's @chainTopology@ (@fromJust . elemIndex target links@).
-}
seamConfig :: Int -> [(TorusDir, Bool, Maybe Int)]
seamConfig node =
  [ (d, ext, if ext then Just (linkTo nb) else Nothing)
  | d <- [North, South, East, West]
  , let (ext, nb) = edge d
  ]
 where
  cx = node `mod` chipCols
  cy = node `div` chipCols
  edge North = (cy < chipRows - 1, icAt cx (cy + 1))
  edge South = (cy > 0, icAt cx (cy - 1))
  edge East = (cx < chipCols - 1, icAt (cx + 1) cy)
  edge West = (cx > 0, icAt (cx - 1) cy)
  -- This FPGA's link vector as plain node ids (link slot -> neighbour node id).
  links = map fromIntegral (C.toList (snd (C.toList fpgaSetup !! node))) :: [Int]
  linkTo nb = fromJust (elemIndex nb links)

-- | The global ring neighbour of (x, y) in a direction (wrap-around).
ringNeighbour :: Int -> Int -> TorusDir -> (Int, Int)
ringNeighbour x y East = ((x + 1) `mod` gDimX, y)
ringNeighbour x y West = ((x - 1) `mod` gDimX, y)
ringNeighbour x y North = (x, (y + 1) `mod` gDimY)
ringNeighbour x y South = (x, (y - 1) `mod` gDimY)

{- | If the directed link (x, y, dir) is an inter-chip seam (its source and
destination cores live on different ICs under the fold), the IC pair (source IC,
destination IC) it connects; otherwise Nothing.
-}
seamPartner :: Int -> Int -> TorusDir -> Maybe (Int, Int)
seamPartner x y d
  | s /= t = Just (s, t)
  | otherwise = Nothing
 where
  (x', y') = ringNeighbour x y d
  s = icOfCore x y
  t = icOfCore x' y'

-- | Whether ICs @a@ and @b@ are joined by a cable on the axis of direction @d@.
cabled :: Int -> Int -> TorusDir -> Bool
cabled a b d =
  any (\l -> (l.nodes == (a, b) || l.nodes == (b, a)) && l.directions == axisOf d) icTopology

-- | Golden UGN of the directed physical link IC @a@ -> IC @b@ (clock cycles).
goldenUgnOf :: [UgnEdge] -> Int -> Int -> Integer
goldenUgnOf edges a b =
  case find (\e -> e.srcNode == nodeId a && e.dstNode == nodeId b) edges of
    Just e -> fromIntegral e.ugn
    Nothing -> error ("latencies: no golden UGN for IC " <> show a <> " -> " <> show b)
 where
  nodeId k = indexToNodeId (fromIntegral k :: C.Index FpgaCount)

{- | Per-crossing seam latency (the @--hop-latencies@ value) for a directed seam from
IC @s@ to IC @t@ in direction @d@: the groomed link latency (golden UGN + safety
margin, backtracked to PE-to-PE by @internalDelay@) plus the chip's TDM seam
serialization (@period + 3@). Matches the chip's @seamLatency = wire + period + 3@
('TdmTorusBoundaryBridge'); @period = 2 * nLinks * cyclesPerSlot@ over the edge's
@nLinks@ boundary links (chipDimY for E/W, chipDimX for N/S). The @+ 3@ (was @+ 2@)
includes the demux io.out pipeline register that breaks the seam->boundary-switch
timing path; only seam crossings carry this extra cycle, not intra-chip hops.
-}
linkLatency :: [UgnEdge] -> Int -> Int -> TorusDir -> Integer
linkLatency edges s t d = wire + period + 3
 where
  wire = goldenUgnOf edges s t + fromIntegral marginFrames + fromIntegral internalDelay
  nLinks = case d of East -> chipDimY; West -> chipDimY; North -> chipDimX; South -> chipDimX
  period = fromIntegral (2 * nLinks * cyclesPerSlot)

{- | Every directed boundary crossing of the folded torus, as rows
@(src_x, src_y, dir, latency)@. A row is emitted for each global core whose hop in a
direction crosses a chip boundary; its latency is that of the physical cable carrying
it (directed golden UGN of the IC pair). The IC pair is checked against 'icTopology'.
-}
seamRows :: [UgnEdge] -> [(Int, Int, TorusDir, Integer)]
seamRows edges =
  [ (x, y, d, linkLatency edges s t d)
  | y <- [0 .. gDimY - 1]
  , x <- [0 .. gDimX - 1]
  , d <- [minBound .. maxBound]
  , Just (s, t) <- [seamPartner x y d]
  , cabled s t d
      || error ("latencies: seam " <> show (x, y, d) <> " crosses uncabled ICs " <> show (s, t))
  ]

-- | The full @latencies.csv@ contents (header + one row per directed boundary link).
latenciesCsv :: [UgnEdge] -> String
latenciesCsv edges =
  unlines ("src_x,src_y,dir,latency" : map fmt (seamRows edges))
 where
  fmt (x, y, d, lat) = intercalate "," [show x, show y, dirName d, show lat]

-- | Write @latencies.csv@ for the demo (using the WireDemo golden UGNs).
writeLatenciesCsv :: FilePath -> IO ()
writeLatenciesCsv path = writeFile path (latenciesCsv goldenUgns)
