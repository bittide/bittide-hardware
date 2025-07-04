-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module Bittide.GeneralPurposeProcessingElement.Calculator where

import Bittide.SwitchDemoProcessingElement.Calculator (
  CyclePeConfig (..),
  FpgaId,
  toFpgaIndexed,
  uncons,
 )
import Clash.Prelude
import Data.Bifunctor (Bifunctor (..))
import Data.Map (Map)
import Data.Maybe (catMaybes)

import qualified Data.Map as Map
import GHC.Stack (HasCallStack)

-- TODO: find a way to derive this 4 from the code instead of using a magic number.
iNTERNAL_SWITCH_DELAY :: (Num a) => a
iNTERNAL_SWITCH_DELAY = 4

-- TODO: find out why this is necessary (issue #753)
dELAY_FROM_EB_TO_CROSSBAR :: (Num a) => a
dELAY_FROM_EB_TO_CROSSBAR = 1

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
     in Just (dst, srcCycle - dstCycle)

type TotalWriteCycles (nNodes :: Nat) (cyclesPerWrite :: Nat) = nNodes * cyclesPerWrite
type WindowCycles (nNodes :: Nat) (cyclesPerWrite :: Nat) =
  (nNodes - 1) * (TotalWriteCycles nNodes cyclesPerWrite)

-- The 2 is because in order to guarantee one whole write opportunity occurs on
-- a destination node per metacycle, we must repeat the window.
type MetacycleLength (nNodes :: Nat) (cyclesPerWrite :: Nat) (padding :: Nat) =
  2 * WindowCycles nNodes cyclesPerWrite + padding

data MetaPeConfig a b c = MetaPeConfig
  { writeMetacycle :: a
  , writeOffset :: b
  , writeForN :: c
  , readMetacycle :: a
  , readOffset :: b
  , readForN :: c
  }
  deriving (Functor, Eq, Show)

metaPeConfigMap ::
  (a -> d) ->
  (b -> e) ->
  (c -> f) ->
  MetaPeConfig a b c ->
  MetaPeConfig d e f
metaPeConfigMap aMap bMap cMap cfg =
  MetaPeConfig
    { writeMetacycle = aMap cfg.writeMetacycle
    , writeOffset = bMap cfg.writeOffset
    , writeForN = cMap cfg.writeForN
    , readMetacycle = aMap cfg.readMetacycle
    , readOffset = bMap cfg.readOffset
    , readForN = cMap cfg.readForN
    }

metaPeConfigToCyclePeConfig ::
  forall a b c.
  (Num a, Integral b, Integral c) =>
  a ->
  a ->
  MetaPeConfig a b c ->
  CyclePeConfig a c
metaPeConfigToCyclePeConfig writesPerCycle metacycleLength config =
  CyclePeConfig
    { startWriteAt =
        metacycleLength
          * config.writeMetacycle
          + writesPerCycle
          * (fromIntegral config.writeOffset)
    , writeForN = fromIntegral config.writeForN
    , startReadAt =
        metacycleLength * config.readMetacycle + writesPerCycle * (fromIntegral config.readOffset)
    , readForN = fromIntegral config.readForN
    }

{- | Create a vector of 'MetaPeConfig's that form a chain of reads and writes, such
that a GPPE starts writing as soon as it can after reading values from its predecessor.
-}
fullChainConfiguration ::
  forall nNodes cyclesPerWrite padding a.
  ( HasCallStack
  , KnownNat nNodes
  , KnownNat cyclesPerWrite
  , KnownNat padding
  , Bounded a
  , Integral a
  , 1 <= nNodes
  ) =>
  -- | Cycles per write
  SNat cyclesPerWrite ->
  -- | Padding between metacycle repetitions
  SNat padding ->
  -- | FPGA configuration
  Vec nNodes (FpgaId, Vec (nNodes - 1) (Index nNodes)) ->
  -- | UGN parts
  Vec nNodes (Vec (nNodes - 1) (a, a)) ->
  -- | Start offset for the first write
  a ->
  -- | 'MetaPeConfig' for each node in the chain
  Vec
    nNodes
    (MetaPeConfig a (Index (2 * WindowCycles nNodes cyclesPerWrite)) (Index (nNodes + 1)))
fullChainConfiguration = chainConfiguration SNat

chainConfiguration ::
  forall chainLength nNodes padding cyclesPerWrite a.
  ( HasCallStack
  , KnownNat nNodes
  , KnownNat chainLength
  , KnownNat padding
  , KnownNat cyclesPerWrite
  , Bounded a
  , Integral a
  , 1 <= chainLength
  , 1 <= nNodes
  , chainLength <= nNodes
  ) =>
  SNat chainLength ->
  -- | Cycles per write
  SNat cyclesPerWrite ->
  -- | Padding between metacycle repetitions
  SNat padding ->
  -- | FPGA configuration
  Vec nNodes (FpgaId, Vec (nNodes - 1) (Index nNodes)) ->
  -- | UGN parts
  Vec nNodes (Vec (nNodes - 1) (a, a)) ->
  -- | Start offset for the first write
  a ->
  -- | 'MetaPeConfig' for each node in the chain
  Vec
    chainLength
    (MetaPeConfig a (Index (2 * WindowCycles nNodes cyclesPerWrite)) (Index (nNodes + 1)))
chainConfiguration SNat cyclesPerWrite padding fpgaConfig ugnParts writeOffset =
  resultI
 where
  ugnPartsI :: Vec nNodes (Vec (nNodes - 1) (Integer, Integer))
  ugnPartsI = map (map (bimap toInteger toInteger)) ugnParts
  resultI ::
    Vec
      chainLength
      (MetaPeConfig a (Index (2 * WindowCycles nNodes cyclesPerWrite)) (Index (nNodes + 1)))
  resultI = map (metaPeConfigMap fromIntegral go go) result
  result :: Vec chainLength (MetaPeConfig Integer Integer Integer)
  result =
    chainConfigurationWorker
      fpgaConfig
      ugnPartsI
      (toInteger writeOffset)
      (snatToNum cyclesPerWrite)
      (snatToNum padding)

  -- XXX: 'chainConfigurationWorker' calculates the number of cycles to write,
  --      but our demo PE wants the number of TRI-cycles to write (or really,
  --      however many cycles it takes to write a single "packet"). We hence end
  --      up dividing it by 'cyclesPerWrite' here.
  go :: forall b. (HasCallStack, Bounded b, Integral b) => Integer -> b
  go n = checkedFromIntegral (n `quot` snatToNum cyclesPerWrite)

{- | Like 'chainConfiguration' but with 'Integer' types. We might rework this to
be more precise in its types, but be prepared for a boatload of type constraints...
-}
chainConfigurationWorker ::
  forall chainLength nNodes.
  ( HasCallStack
  , KnownNat nNodes
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
  -- | Padding between metacycle repetitions
  Integer ->
  Vec chainLength (MetaPeConfig Integer Integer Integer)
chainConfigurationWorker fpgaConfig ugnParts writeOffset cyclesPerWrite padding =
  leToPlus @1 @nNodes
    $ leToPlus @1 @chainLength
    $ leToPlusKN @chainLength @nNodes
    $ let
        (startQuot, startRem) = quotRem writeOffset metacycleLength
        startMetacycle = startQuot + min 1 startRem
        actualWriteOffset0 = startMetacycle * metacycleLength
        -- Unsure if this is necessary just yet. Will have to see.
        -- actualWriteOffset1 =
        --   actualWriteOffset0 + findK minBound (satSucc SatWrap minBound) actualWriteOffset0

        start = (0, actualWriteOffset0)
        ((_, startReadAt), configs) = mapAccumL mkLink start (takeI indicesI)
        (firstConfig, restConfigs) = uncons configs
        readForN = cyclesPerWrite * natToNum @chainLength
        (readMetacycle, readOffset) = quotRem startReadAt metacycleLength
       in
        firstConfig
          { readMetacycle = readMetacycle
          , readOffset = readOffset
          , readForN = readForN
          }
          :> restConfigs
 where
  totalWriteCycles :: Integer
  totalWriteCycles = natToNum @nNodes * cyclesPerWrite
  windowCycles :: Integer
  windowCycles = natToNum @(nNodes - 1) * totalWriteCycles
  metacycleLength :: Integer
  metacycleLength = 2 * windowCycles + padding

  counterMap :: Vec nNodes (Map (Index nNodes) Integer)
  counterMap = toCounterMap (toFpgaIndexed fpgaConfig ugnParts)

  mkLink ::
    (Integer, Integer) ->
    Index nNodes ->
    ((Integer, Integer), MetaPeConfig Integer Integer Integer)
  mkLink (readForN, startReadAt) node =
    ((writeForN, nextStartReadAt), peConfig)
   where
    writeForN = readForN + cyclesPerWrite
    (rM, rO) = quotRem startReadAt metacycleLength

    -- Writes have to happen in the _next_ metacycle, so adjust for that.
    startWriteAtClosestTo = roundUpMetacycle $ startReadAt + readForN
    k = findK node nextNode startWriteAtClosestTo
    startWriteAt = startWriteAtClosestTo + k - dELAY_FROM_EB_TO_CROSSBAR
    (wM, wO) = quotRem startWriteAt metacycleLength
    peConfig =
      MetaPeConfig
        { writeMetacycle = wM
        , writeOffset = wO
        , readMetacycle = rM
        , readOffset = rO
        , ..
        }

    nextStartReadAt = mapCycle startWriteAt node nextNode
    nextNode = satSucc SatWrap node

  -- This might not be what I need.
  roundUpMetacycle :: Integer -> Integer
  roundUpMetacycle cyc = cyc + adjust
   where
    diff = rem cyc metacycleLength
    adjust = (min 1 diff) * metacycleLength - diff

  mapCycle :: Integer -> Index nNodes -> Index nNodes -> Integer
  mapCycle srcCycle src dst = srcCycle + counterMap !! src Map.! dst

  findK :: Index nNodes -> Index nNodes -> Integer -> Integer
  findK src dst srcCycle
    | offsetInDstMetacycle <= linkSelectOffset0 = linkSelectOffset0 - offsetInDstMetacycle
    | offsetInDstMetacycle <= linkSelectOffset1 = linkSelectOffset1 - offsetInDstMetacycle
    | otherwise = linkSelectOffset0 + metacycleLength - offsetInDstMetacycle
   where
    -- For a graphical representation of what we're doing here, check out:
    -- bittide-instances/imgs/ugn_calculator.drawio. For a general introduction
    -- see slides "determining read/write cycles" in
    -- https://docs.google.com/presentation/d/1AGbAJQ1zhTPtrekKnQcthd0TUPyQs-zowQpV1ux4k-Y
    linkSelectOffset0 = offsetsByFpga !! dst Map.! src
    linkSelectOffset1 = linkSelectOffset0 + windowCycles
    dstCycle = mapCycle (srcCycle + dELAY_FROM_EB_TO_CROSSBAR + iNTERNAL_SWITCH_DELAY) src dst
    offsetInDstMetacycle = rem dstCycle metacycleLength

  offsets :: (KnownNat nNodes, 1 <= nNodes) => Vec nNodes (Vec (nNodes - 1) Integer)
  offsets = map ((totalWriteCycles *) . fromIntegral) <$> repeat indicesI

  offsetsByFpga :: (KnownNat nNodes, 1 <= nNodes) => Vec nNodes (Map (Index nNodes) Integer)
  offsetsByFpga = toFpgaIndexed fpgaConfig offsets
