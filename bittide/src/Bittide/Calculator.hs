-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

module Bittide.Calculator where

import Clash.Prelude

import Bittide.SharedTypes (divRU)
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

-- TODO: find a way to derive the delays from the code instead of using a magic number.
--
-- '4', because we traverse two switches and each switch has registered inputs and outputs.
-- The other delays are derived from the type synonyms defined in the Core.
-- However, we can't import them because it would create a cyclic dependency.
rxPreSwitch, rxPostSwitch, txPreSwitch, txPostSwitch :: (Num a) => a
rxPreSwitch = 1
rxPostSwitch = 1
txPreSwitch = 1
txPostSwitch = 1

iNTERNAL_SWITCH_DELAY :: (Num a) => a
iNTERNAL_SWITCH_DELAY = 4 + rxPreSwitch + rxPostSwitch + txPreSwitch + txPostSwitch

dELAY_CROSSBAR_TO_PE :: (Num a) => a
dELAY_CROSSBAR_TO_PE = 1 + rxPostSwitch

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

-- | Generic calculator type for the switch demos.
data
  CalendarCalculator
    (nNodes :: Nat)
    (cyclesPerWrite :: Nat)
    (padding :: Nat)
    (reps :: Nat)
  where
  -- | Calculates the calendar for a system using general purpose processing elements,
  --   where there may be padding and repetition is required due to double-buffered
  --   memories in the scatter units.
  GppeCalculator ::
    forall (nNodes :: Nat) (cyclesPerWrite :: Nat) (padding :: Nat).
    ( HasCallStack
    , KnownNat nNodes
    , KnownNat cyclesPerWrite
    , KnownNat padding
    , 1 <= nNodes
    , 1 <= cyclesPerWrite
    ) =>
    SNat nNodes ->
    SNat cyclesPerWrite ->
    SNat padding ->
    CalendarCalculator nNodes cyclesPerWrite padding 2
  -- | Calculates the calendar for a system using switch demo processing elements,
  --   where no padding and no additional repetition are required.
  SdpeCalculator ::
    forall (nNodes :: Nat) (cyclesPerWrite :: Nat).
    ( HasCallStack
    , KnownNat nNodes
    , KnownNat cyclesPerWrite
    , 1 <= nNodes
    , 1 <= cyclesPerWrite
    ) =>
    SNat nNodes ->
    SNat cyclesPerWrite ->
    CalendarCalculator nNodes cyclesPerWrite 0 1

-- | Class for accessing the parameters and derived types from a calendar calculator.
class CalendarProps cal where
  -- | The number of nodes this calculator considers.
  type CalNumNodes cal :: Nat

  -- | The number of cycles per write each node must do.
  type CalCyclesPerWrite cal :: Nat

  -- | The number of cycles used for padding.
  type CalPadding cal :: Nat

  -- | The number of times to repeat windows.
  type CalRepetitions cal :: Nat

  -- | The total number of cycles any given node will write for per window.
  type CalGroupCycles cal :: Nat

  -- | The number of cycles in a window - one complete repetition of all nodes
  --   writing out their data.
  type CalWindowCycles cal :: Nat

  -- | The number of total cycles for all windows to complete.
  type CalActiveCycles cal :: Nat

  -- | The total length (in cycles) of a metacycle.
  type CalMetacycleLength cal :: Nat

{- | Type-level computation of the total number of cycles for which any given node
will write for per window.
-}
type GroupCycles (nNodes :: Nat) (cyclesPerWrite :: Nat) = nNodes * cyclesPerWrite

{- | Type-level computation of the number of cycles in a window, which is one complete
repetition of all nodes writing out their data.
-}
type WindowCycles (nNodes :: Nat) (cyclesPerWrite :: Nat) =
  (nNodes - 1) * (GroupCycles nNodes cyclesPerWrite)

{- | Type-level computation of the number of active cycles per metacycle, which
is equal to the number of repetitions times the cycles per window.
-}
type ActiveCycles (nNodes :: Nat) (cyclesPerWrite :: Nat) (reps :: Nat) =
  reps * WindowCycles nNodes cyclesPerWrite

{- | Type-level computation of the number of cycles in a metacycle, which is equal
to the number of active cycles plus the number of padding cycles.
-}
type MetacycleLength (nNodes :: Nat) (cyclesPerWrite :: Nat) (padding :: Nat) (reps :: Nat) =
  padding + ActiveCycles nNodes cyclesPerWrite reps

instance
  forall n c p r.
  CalendarProps (CalendarCalculator n c p r)
  where
  type CalNumNodes (CalendarCalculator n c p r) = n
  type CalCyclesPerWrite (CalendarCalculator n c p r) = c
  type CalPadding (CalendarCalculator n c p r) = p
  type CalRepetitions (CalendarCalculator n c p r) = r
  type CalGroupCycles (CalendarCalculator n c p r) = GroupCycles n c
  type CalWindowCycles (CalendarCalculator n c p r) = WindowCycles n c
  type CalActiveCycles (CalendarCalculator n c p r) = ActiveCycles n c r
  type CalMetacycleLength (CalendarCalculator n c p r) = MetacycleLength n c p r

{- | Type alias for the default GPPE calculator configuration, in which there are
3 cycles per write and 2 windows per active period.
-}
type DefaultGppeConfig (nNodes :: Nat) (padding :: Nat) =
  CalendarCalculator nNodes 3 padding 2

{- | Type alias for the default SDPE calculator configuration, in which there are
3 cycles per write and 1 window per active period.
-}
type DefaultSdpeConfig (nNodes :: Nat) = CalendarCalculator nNodes 3 0 1

-- | Constructor alias for the default GPPE calculator configuration.
defaultGppeCalcConfig ::
  forall nNodes padReps.
  ( KnownNat nNodes
  , KnownNat padReps
  , 1 <= nNodes
  ) =>
  DefaultGppeConfig nNodes padReps
defaultGppeCalcConfig = GppeCalculator SNat SNat SNat

-- | Constructor alias for the default SDPE calculator configuration.
defaultSdpeCalcConfig ::
  forall nNodes. (KnownNat nNodes, 1 <= nNodes) => CalendarCalculator nNodes 3 0 1
defaultSdpeCalcConfig = SdpeCalculator SNat SNat

{- | Configuration for the ASIC processing element. This should tell it when to
begin writing and reading in order to facilitate moving data through successive
FPGAs.
-}
data CyclePeConfig a b = CyclePeConfig
  { startWriteAt :: a
  , writeForN :: b
  , startReadAt :: a
  , readForN :: b
  }
  deriving (Functor, Eq, Show)

instance Bifunctor CyclePeConfig where
  bimap f g CyclePeConfig{startWriteAt, writeForN, startReadAt, readForN} =
    CyclePeConfig
      { startWriteAt = f startWriteAt
      , writeForN = g writeForN
      , startReadAt = f startReadAt
      , readForN = g readForN
      }

{- | Configuration for the general purpose processing element. This should tell
it when to begin writing and reading in order to facilitate moving data through
successive FPGAs.
-}
data MetaPeConfig a b c = MetaPeConfig
  { writeMetacycle :: a
  , writeOffset :: b
  , writeForN :: c
  , readMetacycle :: a
  , readOffset :: b
  , readForN :: c
  }
  deriving (Functor, Eq, Show)

-- | There's no @Trifunctor@ and @trimap@, so do this instead.
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

-- | Converts a GPPE configuration to a SDPE configuration.
metaPeConfigToCyclePeConfig ::
  forall a b c.
  (Num a, Integral b, Integral c) =>
  a ->
  MetaPeConfig a b c ->
  CyclePeConfig a c
metaPeConfigToCyclePeConfig metacycleLength config =
  CyclePeConfig
    { startWriteAt =
        metacycleLength * config.writeMetacycle + (fromIntegral config.writeOffset)
    , writeForN = fromIntegral config.writeForN
    , startReadAt =
        metacycleLength * config.readMetacycle + (fromIntegral config.readOffset)
    , readForN = fromIntegral config.readForN
    }

type DefaultMetaPeConfig
  a
  (nNodes :: Nat)
  (cyclesPerWrite :: Nat)
  (padding :: Nat)
  (reps :: Nat) =
  MetaPeConfig
    a
    (Index (MetacycleLength nNodes cyclesPerWrite padding reps))
    (Index (nNodes + 1))

type DefaultGppeMetaPeConfig a (nNodes :: Nat) (cyclesPerWrite :: Nat) (padding :: Nat) =
  DefaultMetaPeConfig a nNodes cyclesPerWrite padding 2
type DefaultSdpeMetaPeConfig a (nNodes :: Nat) (cyclesPerWrite :: Nat) =
  DefaultMetaPeConfig a nNodes cyclesPerWrite 0 1

{- | Create a vector of 'MetaPeConfig's that form a chain of reads and writes, such
that a GPPE starts writing as soon as it can after reading values from its predecessor.
-}
fullChainConfiguration ::
  forall nNodes cyclesPerWrite padding reps a.
  ( HasCallStack
  , KnownNat cyclesPerWrite
  , KnownNat nNodes
  , KnownNat padding
  , KnownNat reps
  , Bounded a
  , Integral a
  , 1 <= nNodes
  ) =>
  CalendarCalculator nNodes cyclesPerWrite padding reps ->
  -- | FPGA configuration
  Vec nNodes (FpgaId, Vec (nNodes - 1) (Index nNodes)) ->
  -- | UGN parts
  Vec nNodes (Vec (nNodes - 1) (a, a)) ->
  -- | Start offset for the first write
  a ->
  -- | 'MetaPeConfig' for each node in the chain
  Vec nNodes (DefaultMetaPeConfig a nNodes cyclesPerWrite padding reps)
fullChainConfiguration = chainConfiguration SNat

{- | Like 'fullChainConfiguration' but the number of nodes in the chain is
configurable.
-}
chainConfiguration ::
  forall chainLength nNodes cyclesPerWrite padding reps a.
  ( HasCallStack
  , KnownNat chainLength
  , KnownNat cyclesPerWrite
  , KnownNat nNodes
  , KnownNat padding
  , KnownNat reps
  , Bounded a
  , Integral a
  , 1 <= chainLength
  , 1 <= nNodes
  , chainLength <= nNodes
  ) =>
  SNat chainLength ->
  CalendarCalculator nNodes cyclesPerWrite padding reps ->
  -- | FPGA configuration
  Vec nNodes (FpgaId, Vec (nNodes - 1) (Index nNodes)) ->
  -- | UGN parts
  Vec nNodes (Vec (nNodes - 1) (a, a)) ->
  -- | Start offset for the first write
  a ->
  -- | 'MetaPeConfig' for each node in the chain
  Vec chainLength (DefaultMetaPeConfig a nNodes cyclesPerWrite padding reps)
chainConfiguration SNat cal fpgaConfig ugnParts writeOffset =
  resultI
 where
  ugnPartsI :: Vec nNodes (Vec (nNodes - 1) (Integer, Integer))
  ugnPartsI = map (map (bimap toInteger toInteger)) ugnParts
  resultI ::
    Vec chainLength (DefaultMetaPeConfig a nNodes cyclesPerWrite padding reps)
  resultI = map (metaPeConfigMap checkedFromIntegral checkedFromIntegral go) result
  result :: Vec chainLength (MetaPeConfig Integer Integer Integer)
  result =
    chainConfigurationWorker
      cal
      fpgaConfig
      ugnPartsI
      (toInteger writeOffset)

  -- XXX: 'chainConfigurationWorker' calculates the number of cycles to write,
  --      but our demo PE wants the number of TRI-cycles to write (or really,
  --      however many cycles it takes to write a single "packet"). We hence end
  --      up dividing it by 'cyclesPerWrite' here.
  go :: forall b. (HasCallStack, Bounded b, Integral b) => Integer -> b
  go n = checkedFromIntegral (n `quot` natToNum @cyclesPerWrite)

{- | Like 'chainConfiguration' but with 'Integer' types. We might rework this to
be more precise in its types, but be prepared for a boatload of type constraints...
-}
chainConfigurationWorker ::
  forall chainLength nNodes cyclesPerWrite padding reps.
  ( HasCallStack
  , KnownNat nNodes
  , 1 <= nNodes
  , KnownNat chainLength
  , 1 <= chainLength
  , chainLength <= nNodes
  , KnownNat cyclesPerWrite
  , KnownNat padding
  , KnownNat reps
  ) =>
  CalendarCalculator nNodes cyclesPerWrite padding reps ->
  -- | FPGA configuration
  Vec nNodes (FpgaId, Vec (nNodes - 1) (Index nNodes)) ->
  -- | UGN parts
  Vec nNodes (Vec (nNodes - 1) (Integer, Integer)) ->
  -- | Start offset for the first write
  Integer ->
  Vec chainLength (MetaPeConfig Integer Integer Integer)
chainConfigurationWorker _ fpgaConfig ugnParts writeOffset =
  leToPlus @1 @nNodes
    $ leToPlus @1 @chainLength
    $ leToPlusKN @chainLength @nNodes
    $ let
        start = (0, writeOffset)
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
  cyclesPerWrite :: Integer
  cyclesPerWrite = natToNum @cyclesPerWrite
  groupCycles :: Integer
  groupCycles = natToNum @(GroupCycles nNodes cyclesPerWrite)
  windowCycles :: Integer
  windowCycles = natToNum @(WindowCycles nNodes cyclesPerWrite)
  metacycleLength :: Integer
  metacycleLength = natToNum @(MetacycleLength nNodes cyclesPerWrite padding reps)

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
    startWriteAt = startWriteAtClosestTo + k
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

  roundUpMetacycle :: Integer -> Integer
  roundUpMetacycle cyc = metacycleLength * (cyc `divRU` metacycleLength)

  mapCycle :: Integer -> Index nNodes -> Index nNodes -> Integer
  mapCycle srcCycle src dst = srcCycle + counterMap !! src Map.! dst

  findK :: Index nNodes -> Index nNodes -> Integer -> Integer
  findK src dst srcCycle
    | offsetInDstMetacycle <= linkSelectOffset0 = linkSelectOffset0 - offsetInDstMetacycle
    | offsetInDstMetacycle <= linkSelectOffset1 = linkSelectOffset1 - offsetInDstMetacycle
    | otherwise = linkSelectOffset0 + metacycleLength - offsetInDstMetacycle
   where
    -- For a graphical representation of what we're doing here, check out:
    -- `bittide/imgs/metacycle-ugn-calculator.svg`. For a general introduction
    -- see slides "determining read/write cycles" in
    -- https://docs.google.com/presentation/d/1JryWl8EjcWlwOW7OO24nXui5wx_WPlDJOtp1Jlf_fmE
    linkSelectOffset0 = offsetsByFpga !! dst Map.! src
    linkSelectOffset1 = linkSelectOffset0 + windowCycles
    crossbarDstCycle = mapCycle (srcCycle - dELAY_CROSSBAR_TO_PE) src dst
    offsetInDstMetacycle = rem crossbarDstCycle metacycleLength

  offsets :: (KnownNat nNodes, 1 <= nNodes) => Vec nNodes (Vec (nNodes - 1) Integer)
  offsets = map ((groupCycles *) . fromIntegral) <$> repeat indicesI

  offsetsByFpga :: (KnownNat nNodes, 1 <= nNodes) => Vec nNodes (Map (Index nNodes) Integer)
  offsetsByFpga = toFpgaIndexed fpgaConfig offsets

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
