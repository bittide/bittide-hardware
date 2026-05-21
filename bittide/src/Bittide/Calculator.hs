-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE UndecidableInstances #-}

module Bittide.Calculator where

import Clash.Prelude

import Clash.Functor.Extra ((<<$>>))
import Control.Monad (forM_)
import Data.Map (Map)
import Data.Maybe (catMaybes, fromJust)
import Data.String.Interpolate (i)
import GHC.Stack (HasCallStack)

import qualified Data.List as L
import qualified Data.Map as Map

type FpgaId = String

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
  Int ->
  Vec nNodes (Map (Index nNodes) (a, a)) ->
  -- | n -> m: c
  Vec nNodes (Map (Index nNodes) a)
toCounterMap internalDelay fpgaIndexed = goSrc <$> indicesI
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
     in Just (dst, srcCycle - dstCycle + fromIntegral internalDelay)

{- | Prints the measured IGNs in a matrix-like format.

For example:

@
xxxx,   67, 1299,   68,   69,   69,   69,   70
  67, xxxx,   80,   69,   69,   69,   70,   70
1299,   80, xxxx,   69,   69,   69,   69,   69
  68,   69,   69, xxxx,   70,   70,   69,   69
  69,   69,   69,   70, xxxx,   69,   69,   69
  69,   69,   69,   70,   69, xxxx,   68,   70
  69,   70,   69,   69,   69,   68, xxxx,   69
  70,   70,   69,   69,   69,   70,   69, xxxx
@
-}
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
    ugnsTable :: Vec (n + 1) (Vec m (Signed 66))
    ugnsTable = toUgn <<$>> ugnPairsTable

    toUgn :: (Unsigned 64, Unsigned 64) -> Signed 66
    toUgn (a, b) = numConvert a - numConvert b

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
      IO (Vec m (Signed 66))
    printIgns rowI (myId, row) = do
      putStrLn [i|Finding IGNs for #{myId} (#{rowI})|]
      putStrLn [i|  My connections: #{row}|]
      let
        printIgn :: (HasCallStack) => Index m -> Index (n + 1) -> IO (Signed 66)
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
    allList :: Vec (l + 1) (Signed 66)
    allList = concat igns

    largest :: Signed 66
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
