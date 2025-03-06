-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}

module Bittide.SwitchDemoProcessingElement.Calculator where

import Clash.Prelude

import Data.Bifunctor (Bifunctor (bimap))
import Data.Map (Map)
import Data.Maybe (catMaybes)
import GHC.Stack (HasCallStack)

import qualified Data.Map as Map

type FpgaId = String

-- TODO: find a way to derive this 4 from the code instead of using a magic number.
iNTERNAL_SWITCH_DELAY :: (Num a) => a
iNTERNAL_SWITCH_DELAY = 4

{- | Convert a table (fpga_nr x link_nr) to a vector of maps mapping an fpga index
(instead of a link index) to the value.
-}
toFpgaIndexed ::
  forall nNodes a.
  (KnownNat nNodes, 1 <= nNodes) =>
  Vec nNodes (FpgaId, Vec (nNodes - 1) (Index nNodes)) ->
  Vec nNodes (Vec (nNodes - 1) a) ->
  Vec nNodes (Map (Index nNodes) a)
toFpgaIndexed fpgaSetup table = toMap <$> (goRow <$> indicesI <*> table)
 where
  goRow nodeNr row = goCell nodeNr <$> indicesI <*> row
  goCell nodeNr linkNr a = (snd (fpgaSetup !! nodeNr) !! linkNr, a)
  toMap = Map.fromList . toList

{- | Given a table structured as 'exampleUgnParts' applied to 'toFpgaIndex'
convert it into a table that represents the mapping from an FPGA's local counter
to another FPGA's counter.
-}
toCounterMap ::
  forall nNodes a.
  (HasCallStack, Num a, KnownNat nNodes, 1 <= nNodes) =>
  Vec nNodes (Map (Index nNodes) (a, a)) ->
  -- | n -> m: c
  Vec nNodes (Map (Index nNodes) a)
toCounterMap fpgaIndexed = goSrc <$> indicesI
 where
  goSrc ::
    (HasCallStack) =>
    Index nNodes ->
    Map (Index nNodes) a
  goSrc src = Map.fromList (catMaybes (toList (goSrcDst src <$> indicesI)))

  goSrcDst ::
    (HasCallStack) =>
    Index nNodes ->
    Index nNodes ->
    Maybe (Index nNodes, a)
  goSrcDst src dst | src == dst = Nothing
  goSrcDst src dst =
    let (srcCycle, dstCycle) = fpgaIndexed !! dst Map.! src
     in Just (dst, srcCycle - dstCycle + iNTERNAL_SWITCH_DELAY)

uncons :: Vec (n + 1) a -> (a, Vec n a)
uncons (x :> xs) = (x, xs)
uncons Nil = error "uncons: unreachable"

data PeConfig a b = PeConfig
  { startWriteAt :: a
  , writeForN :: b
  , startReadAt :: a
  , readForN :: b
  }
  deriving (Functor, Eq, Show)

instance Bifunctor PeConfig where
  bimap f g PeConfig{startWriteAt, writeForN, startReadAt, readForN} =
    PeConfig
      { startWriteAt = f startWriteAt
      , writeForN = g writeForN
      , startReadAt = f startReadAt
      , readForN = g readForN
      }

{- | Create a vector of 'PeConfig's that form a chain of write reads, such that
a PE starts writing as soon as it can after reading values from its
predecessor.
-}
fullChainConfiguration ::
  forall nNodes cyclesPerWrite a.
  ( KnownNat nNodes
  , Bounded a
  , Integral a
  , 1 <= nNodes
  ) =>
  -- | Cycles per write
  SNat cyclesPerWrite ->
  -- | FPGA configuration
  Vec nNodes (FpgaId, Vec (nNodes - 1) (Index nNodes)) ->
  -- | UGN parts
  Vec nNodes (Vec (nNodes - 1) (a, a)) ->
  -- | Start offset for the first write
  a ->
  -- | PeConfig for each node in the chain
  Vec nNodes (PeConfig a (Index (nNodes + 1)))
fullChainConfiguration = chainConfiguration SNat

{- | Like 'fullChainConfiguration' but the number of nodes in the chain is
configurable.
-}
chainConfiguration ::
  forall chainLength nNodes cyclesPerWrite a.
  ( KnownNat nNodes
  , KnownNat chainLength
  , Bounded a
  , Integral a
  , 1 <= chainLength
  , 1 <= nNodes
  , chainLength <= nNodes
  ) =>
  SNat chainLength ->
  -- | Cycles per write
  SNat cyclesPerWrite ->
  -- | FPGA configuration
  Vec nNodes (FpgaId, Vec (nNodes - 1) (Index nNodes)) ->
  -- | UGN parts
  Vec nNodes (Vec (nNodes - 1) (a, a)) ->
  -- | Start offset for the first write
  a ->
  -- | PeConfig for each node in the chain
  Vec chainLength (PeConfig a (Index (nNodes + 1)))
chainConfiguration SNat cyclesPerWrite fpgaConfig ugnParts writeOffset =
  resultI
 where
  ugnPartsI = map (map (bimap toInteger toInteger)) ugnParts
  resultI = map (bimap fromIntegral go) result
  result =
    chainConfigurationWorker
      fpgaConfig
      ugnPartsI
      (toInteger writeOffset)
      (snatToNum cyclesPerWrite)

  -- XXX: 'chainConfigurationWorker' calculates the number of cycles to write,
  --      but our demo PE wants the number of TRI-cycles to write (or really,
  --      however many cycles it takes to write a single "packet"). We hence end
  --      up dividing it by 'cyclesPerWrite' here.
  go :: forall b. (Bounded b, Integral b) => Integer -> b
  go n = checkedFromIntegral (n `quot` snatToNum cyclesPerWrite)

{- | Like 'chainConfiguration' but with 'Integer' types. We might rework this to
be more precise in its types, but be prepared for a boatload of type constraints...
-}
chainConfigurationWorker ::
  forall chainLength nNodes.
  ( KnownNat nNodes
  , 1 <= nNodes
  , KnownNat chainLength
  , 1 <= chainLength
  , chainLength <= nNodes
  ) =>
  -- | FPGA configuration
  Vec nNodes (FpgaId, Vec (nNodes - 1) (Index nNodes)) ->
  -- | UGN parts
  Vec nNodes (Vec (nNodes - 1) (Integer, Integer)) ->
  -- | Start offset for the first write
  Integer ->
  -- | Cycles per write (probably 3)
  Integer ->
  Vec chainLength (PeConfig Integer Integer)
chainConfigurationWorker fpgaConfig ugnParts writeOffset cyclesPerWrite =
  id
    $ leToPlus @1 @nNodes
    $ leToPlus @1 @chainLength
    $ leToPlusKN @chainLength @nNodes
    $ let
        start = (0, writeOffset)
        ((_, startReadAt), configs) = mapAccumL mkLink start (takeI indicesI)
        (firstConfig, restConfigs) = uncons configs
        readForN = cyclesPerWrite * natToNum @chainLength
       in
        firstConfig{startReadAt = startReadAt, readForN = readForN} :> restConfigs
 where
  counterMap = toCounterMap (toFpgaIndexed fpgaConfig ugnParts)

  mkLink ::
    (Integer, Integer) ->
    Index nNodes ->
    ((Integer, Integer), PeConfig Integer Integer)
  mkLink (readForN, startReadAt) node =
    ((writeForN, nextStartReadAt), peConfig)
   where
    peConfig = PeConfig{startWriteAt, writeForN, startReadAt, readForN}
    k = findK node nextNode startWriteAtClosestTo
    startWriteAt = startWriteAtClosestTo + k
    startWriteAtClosestTo = startReadAt + readForN
    writeForN = readForN + cyclesPerWrite
    nextStartReadAt = mapCycle startWriteAt node nextNode
    nextNode = satSucc SatWrap node

  mapCycle :: Integer -> Index nNodes -> Index nNodes -> Integer
  mapCycle srcCycle src dst = srcCycle + counterMap !! src Map.! dst

  findK :: Index nNodes -> Index nNodes -> Integer -> Integer
  findK src dst srcCycle
    | dstCycle <= linkStart0 = linkStart0 - dstCycle
    | dstCycle <= linkStart1 = linkStart1 - dstCycle
    | otherwise = error "findK: unreachable"
   where
    -- For a graphical representation of what we're doing here, check out:
    -- bittide-instances/imgs/ugn_calculator.drawio. For a general introduction
    -- see slides "determining read/write cycles" in
    -- https://docs.google.com/presentation/d/1AGbAJQ1zhTPtrekKnQcthd0TUPyQs-zowQpV1ux4k-Y
    dstCycle = mapCycle srcCycle src dst
    linkSelectOffset = offsetsByFpga !! dst Map.! src
    fullCalendarsSeen = dstCycle `quot` calendarLength
    prevCalendarStart = fullCalendarsSeen * calendarLength
    linkStart0 = prevCalendarStart + linkSelectOffset
    linkStart1 = linkStart0 + calendarLength

  calendarEntryLength = cyclesPerWrite * natToNum @nNodes
  calendarLength = calendarEntryLength * natToNum @(nNodes - 1)

  offsets :: (KnownNat nNodes, 1 <= nNodes) => Vec nNodes (Vec (nNodes - 1) Integer)
  offsets = map ((calendarEntryLength *) . fromIntegral) <$> repeat indicesI

  offsetsByFpga :: (KnownNat nNodes, 1 <= nNodes) => Vec nNodes (Map (Index nNodes) Integer)
  offsetsByFpga = toFpgaIndexed fpgaConfig offsets
