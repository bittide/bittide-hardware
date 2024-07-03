-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE NamedFieldPuns #-}

-- | Check a memory map for overlapping address-ranges in its components.
module Protocols.MemoryMap.Check.Overlap where

import Prelude

import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
import GHC.Stack (SrcLoc)
import Protocols.MemoryMap

data OverlapError
  = SizeExceedsError
      { startAddr :: Address
      , availableSize :: Size
      , requestedSize :: Size
      , path :: ComponentPath
      , location :: SrcLoc
      }
  | OverlapError
      { startAddr :: Address
      , componentSize :: Size
      , path :: ComponentPath
      , overlapsWith :: ComponentPath
      , overlapsAt :: Address
      , location :: SrcLoc
      }
  deriving (Show)

checkOverlap :: (Address, Address) -> MemoryMap -> [OverlapError]
checkOverlap availableRange MemoryMap{deviceDefs = M.map deviceSize -> dss, tree} =
  walk Root availableRange tree
 where
  walk _ _ (Interconnect _ _ []) = []
  walk path (start, end) (Interconnect loc _ comps) =
    interconnectSizeError <> overlapErrors <> concat componentErrors
   where
    comps1 = zip (L.map (`InterconnectComponent` path) [0 ..]) comps
    comps2 = L.sortOn (\(_, (addr', _, _)) -> addr') comps1
    (_, (addr, size, _)) = L.last comps2
    interconnectSize = addr + size
    availableSize = end - start
    overlapErrors = checkAllOverlaps loc comps2
    interconnectSizeError =
      [ SizeExceedsError
        { startAddr = start
        , availableSize
        , requestedSize = interconnectSize
        , path
        , location = loc
        }
      | interconnectSize > availableSize
      ]
    componentErrors = flip map comps2 $ \(path', (addr', size', comp)) -> walk path' (start + addr', start + addr' + size') comp
  walk path (start, end) (DeviceInstance loc _ _instanceName deviceName) =
    let
      defSize = fromJust $ M.lookup deviceName dss
      availableSize = end - start
     in
      [ SizeExceedsError
        { startAddr = start
        , availableSize
        , requestedSize = defSize
        , path
        , location = loc
        }
      | defSize > availableSize
      ]

checkAllOverlaps ::
  SrcLoc -> [(ComponentPath, (Address, Size, MemoryMapTree))] -> [OverlapError]
checkAllOverlaps _ [] = []
checkAllOverlaps _ [_] = []
checkAllOverlaps loc ((aName, (aStart, aLen, _)) : b@(bName, (bStart, _, _)) : rest)
  | bStart < aStart + aLen =
      OverlapError
        { startAddr = aStart
        , componentSize = aLen
        , path = aName
        , overlapsWith = bName
        , overlapsAt = bStart
        , location = loc
        }
        : checkAllOverlaps loc (b : rest)
  | otherwise = checkAllOverlaps loc (b : rest)

deviceSize :: DeviceDefinition -> Size
deviceSize DeviceDefinition{registers = map (\(_, _, x) -> x) -> regs} = snd fullRange
 where
  fullRange = L.foldl' mergeRange (0, 0) ranges
  ranges = map walk regs
  walk Register{address, fieldSize} = (address, address + fieldSize)

  mergeRange :: (Address, Address) -> (Address, Address) -> (Address, Address)
  mergeRange (start0, end0) (start1, end1) = (min start0 start1, max end0 end1)
