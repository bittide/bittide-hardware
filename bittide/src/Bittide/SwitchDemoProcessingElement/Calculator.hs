-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE QuasiQuotes #-}

module Bittide.SwitchDemoProcessingElement.Calculator where

import Clash.Prelude

import Clash.Functor.Extra ((<<$>>))
import Control.Monad (forM_)
import Data.Bifunctor (Bifunctor (bimap))
import Data.Map (Map)
import Data.Maybe (catMaybes, fromJust)
import Data.String.Interpolate (i)
import GHC.Stack (HasCallStack)

import qualified Data.List as L
import qualified Data.Map as Map

type FpgaId = String

-- TODO: find a way to derive this 4 from the code instead of using a magic number.
iNTERNAL_SWITCH_DELAY :: (Num a) => a
iNTERNAL_SWITCH_DELAY = 4

-- TODO: find out why this is necessary
dELAY_FROM_EB_TO_CROSSBAR :: (Num a) => a
dELAY_FROM_EB_TO_CROSSBAR = 1

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
  leToPlus @1 @nNodes
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
    linkStart0 = prevCalendarStart + linkSelectOffset + dELAY_FROM_EB_TO_CROSSBAR
    linkStart1 = linkStart0 + calendarLength

  calendarEntryLength = cyclesPerWrite * natToNum @nNodes
  calendarLength = calendarEntryLength * natToNum @(nNodes - 1)

  offsets :: (KnownNat nNodes, 1 <= nNodes) => Vec nNodes (Vec (nNodes - 1) Integer)
  offsets = map ((calendarEntryLength *) . fromIntegral) <$> repeat indicesI

  offsetsByFpga :: (KnownNat nNodes, 1 <= nNodes) => Vec nNodes (Map (Index nNodes) Integer)
  offsetsByFpga = toFpgaIndexed fpgaConfig offsets

printAllIgns ::
  forall n m l.
  ( HasCallStack
  , KnownNat m
  , KnownNat n
  , KnownNat l
  , l + 1 ~ (n + 1) * m
  ) =>
  Vec (n + 1) (Vec m (Unsigned 64, Unsigned 64)) ->
  Vec (n + 1) (FpgaId, Vec m (Index (n + 1))) ->
  IO ()
printAllIgns ugnPairsTable fpgasTable = do
  let
    ugnsTable :: Vec (n + 1) (Vec m (Unsigned 64))
    ugnsTable = uncurry (-) <<$>> ugnPairsTable
    fpgasMap :: Vec (n + 1) (Vec m (Index (n + 1)))
    fpgasMap = snd <$> fpgasTable
    indicesM :: Vec m (Index m)
    indicesM = indicesI
    indicesN :: Vec (n + 1) (Index (n + 1))
    indicesN = indicesI
    printIgns ::
      (HasCallStack) =>
      Index (n + 1) ->
      (FpgaId, Vec m (Index (n + 1))) ->
      IO (Vec m (Unsigned 64))
    printIgns rowI (myId, row) = do
      putStrLn [i|Finding IGNs for #{myId} (#{rowI})|]
      putStrLn [i|  My connections: #{row}|]
      let
        printIgn :: (HasCallStack) => Index m -> Index (n + 1) -> IO (Unsigned 64)
        printIgn colI otherRowI = do
          putStrLn [i|  Looking at column #{colI} => #{otherRowI}|]
          let
            otherRow :: Vec m (Index (n + 1))
            otherRow = fpgasMap !! otherRowI
            otherRowName :: FpgaId
            otherRowName = fst $ fpgasTable !! otherRowI
          putStrLn [i|    Row name: #{otherRowName}|]
          putStrLn [i|    Row contents: #{otherRow}|]
          let
            myIndex :: Index m
            myIndex = fromJust $ elemIndex rowI otherRow
          putStrLn [i|    Other row has me at index: #{myIndex}|]
          let
            myUgn = ugnsTable !! rowI !! colI
            otherUgn = ugnsTable !! otherRowI !! myIndex
            ign = myUgn + otherUgn
          putStrLn [i|  The IGN for this link is: #{myUgn} + #{otherUgn} = #{ign}|]
          return ign
      igns <- sequence $ zipWith printIgn indicesM row
      putStrLn [i|IGNs for FPGA #{myId} (#{rowI}): #{igns}|]
      return igns

  igns <- sequence $ zipWith printIgns indicesN fpgasTable
  let
    allList :: Vec (l + 1) (Unsigned 64)
    allList = concat igns
    largest :: Unsigned 64
    largest = maximum allList
    largestLen :: Int
    largestLen = L.length $ show largest
    coords :: Vec (n + 1) (Vec (n + 1) (Index (n + 1), Index (n + 1)))
    coords = (\ind -> (,) ind <$> indicesN) <$> indicesN
    printIgn :: Index (n + 1) -> Index (n + 1) -> IO ()
    printIgn row fpgaNum = do
      let
        fpgaNumI = fromIntegral fpgaNum :: Integer
        col = fromJust $ findIndex ((==) fpgaNumI . fromIntegral) (fpgasMap !! row)
        ign = igns !! row !! col
        str =
          if (fromIntegral row :: Integer) == fpgaNumI
            then L.replicate largestLen 'x'
            else show ign
        pad = L.replicate (largestLen - L.length str) ' '
      if fpgaNum == maxBound
        then putStrLn [i|#{pad}#{str}|]
        else putStr [i|#{pad}#{str}, |]

  putStrLn "Final IGNs table:"
  forM_ coords $ mapM_ (uncurry printIgn)
