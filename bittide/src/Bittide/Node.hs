-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=6 #-}

{-# OPTIONS -fplugin=Protocols.Plugin #-}

module Bittide.Node where

import Clash.Prelude
import Clash.Sized.Vector.ToTuple (vecToTuple)

import Data.Maybe
import Protocols
import Protocols.Idle
import Protocols.Internal (vecCircuits)
import Protocols.Wishbone
import VexRiscv

import Bittide.Calendar
import Bittide.DoubleBufferedRam
import Bittide.ProcessingElement
import Bittide.ScatterGather
import Bittide.SharedTypes
import Bittide.Switch

{- | A simple node consisting of one external bidirectional link and two 'gppe's.
This node's 'switch' has a 'CalendarConfig' of for a 'calendar' with up to @1024@ entries,
however, the 'calendar' is initialized with a single entry of repeated zeroes.
The 'scatterUnitWb's and 'gatherUnitWb's are initialized with 'CalendarConfig's of all
zeroes. The 'gppe's initial memories are both undefined and the 'MemoryMap' is a
vector of ever increasing base addresses (increments of 0x1000).
-}
simpleNodeConfig :: NodeConfig 1 2
simpleNodeConfig =
  NodeConfig
    (ManagementConfig (ScatterConfig sgConfig) (GatherConfig sgConfig) nmuConfig)
    switchCal
    (repeat (GppeConfig (ScatterConfig sgConfig) (GatherConfig sgConfig) peConfig))
 where
  switchCal = CalendarConfig (SNat @1024) (switchEntry :> Nil) (switchEntry :> Nil)
  sgConfig = CalendarConfig (SNat @1024) (sgEntry :> Nil) (sgEntry :> Nil)
  peConfig = PeConfig memMapPe (Undefined @8192) (Undefined @8192)
  nmuConfig = PeConfig memMapNmu (Undefined @8192) (Undefined @8192)
  memMapPe = iterateI (+ 0x1000) 0
  memMapNmu = iterateI (+ 0x1000) 0
  switchEntry = ValidEntry{veEntry = repeat 0, veRepeat = 0 :: Unsigned 0}
  sgEntry = ValidEntry{veEntry = 0 :: Index 1024, veRepeat = 0 :: Unsigned 0}

{- | Each 'gppe' results in 2 busses for the 'managementUnit', namely:
* The 'calendar' for the 'scatterUnitWB'.
* The 'calendar' for the 'gatherUnitWB'.
-}
type BussesPerGppe = 2

-- | Configuration of a 'node'.
data NodeConfig externalLinks gppes where
  NodeConfig ::
    ( KnownNat nmuBusses
    , nmuBusses ~ ((BussesPerGppe * gppes) + 1 + NmuInternalBusses)
    , KnownNat nmuRemBusWidth
    , nmuRemBusWidth ~ (30 - CLog 2 nmuBusses)
    , CLog 2 nmuBusses <= 30
    ) =>
    -- | Configuration for the 'node's 'managementUnit'.
    ManagementConfig ((BussesPerGppe * gppes) + 1) ->
    -- | Configuratoin for the 'node's 'switch'.
    CalendarConfig 4 nmuRemBusWidth (CalendarEntry (externalLinks + gppes + 1)) ->
    -- | Configuration for all the node's 'gppe's.
    Vec gppes (GppeConfig nmuRemBusWidth) ->
    NodeConfig externalLinks gppes

-- | A 'node' consists of a 'switch', 'managementUnit' and @0..n@ 'gppe's.
node ::
  forall dom extLinks gppes.
  (HiddenClockResetEnable dom, KnownNat extLinks, KnownNat gppes) =>
  NodeConfig extLinks gppes ->
  Circuit
    (Vec extLinks (CSignal dom (DataLink 64)))
    (Vec extLinks (CSignal dom (DataLink 64)))
node (NodeConfig nmuConfig switchConfig gppeConfigs) = circuit $ \linksIn -> do
  switchOut <- switchC @_ @_ @_ @_ @64 switchConfig -< (switchIn, swWb)
  switchIn <- vecMap (cSigMap fromJust) <| appendC3 -< ([nmuLinkOut], pesToSwitch, linksIn)
  ([nmuLinkIn], switchToPes, linksOut) <- splitC3 <| vecMap (cSigMap Just) -< switchOut
  (nmuLinkOut, nmuWbs0) <- managementUnitC nmuConfig -< nmuLinkIn
  ([swWb], nmuWbs1) <- splitAtC d1 -< nmuWbs0
  peWbs <- unconcatC d2 -< nmuWbs1

  pesToSwitch <- vecCircuits (map gppeC gppeConfigs) <| zipC -< (switchToPes, peWbs)
  idC -< linksOut

type NmuInternalBusses = 6
type NmuRemBusWidth nodeBusses = 30 - CLog 2 (nodeBusses + NmuInternalBusses)

{- | Configuration for the 'managementUnit' and its 'Bittide.Link'.
The management unit contains the 4 wishbone busses that each pe has
and also the management busses for itself and all other pe's in this node.
Furthermore it also has access to the 'calendar' for the 'switch'.
-}
data ManagementConfig nodeBusses where
  ManagementConfig ::
    (KnownNat nodeBusses) =>
    ScatterConfig 4 (NmuRemBusWidth nodeBusses) ->
    GatherConfig 4 (NmuRemBusWidth nodeBusses) ->
    PeConfig (nodeBusses + NmuInternalBusses) ->
    ManagementConfig nodeBusses

{- | Configuration for a general purpose processing element together with its link to the
switch.
-}
data GppeConfig nmuRemBusWidth where
  GppeConfig ::
    ScatterConfig 4 nmuRemBusWidth ->
    GatherConfig 4 nmuRemBusWidth ->
    -- | Configuration for a 'gppe's 'processingElement', which statically
    -- has four external busses connected to the instruction memory, data memory
    -- , 'scatterUnitWb' and 'gatherUnitWb'.
    PeConfig 4 ->
    GppeConfig nmuRemBusWidth

{-# NOINLINE gppe #-}

{- | A general purpose 'processingElement' to be part of a Bittide Node. It contains
a 'processingElement', 'linkToPe' and 'peToLink' which create the interface for the
Bittide Link. It takes a 'GppeConfig', incoming link and two incoming 'WishboneM2S'
signals and produces the outgoing link alongside two 'WishhboneS2M' signals.
The order of Wishbone busses is as follows:
('scatterUnitWb' :> 'gatherUnitWb' :> Nil).
-}
gppe ::
  ( HiddenClockResetEnable dom
  , KnownNat nmuRemBusWidth
  ) =>
  -- |
  -- ( Configures all local parameters
  -- , Incoming 'Bittide.Link'
  -- , Incoming @Vector@ of master busses
  -- )
  ( GppeConfig nmuRemBusWidth
  , Signal dom (DataLink 64)
  , Vec 2 (Signal dom (WishboneM2S nmuRemBusWidth 4 (Bytes 4)))
  ) ->
  -- |
  -- ( Outgoing 'Bittide.Link'
  -- , Outgoing @Vector@ of slave busses
  -- )
  ( Signal dom (DataLink 64)
  , Vec 2 (Signal dom (WishboneS2M (Bytes 4)))
  )
gppe (GppeConfig scatterConfig gatherConfig peConfig, linkIn, vecToTuple -> (nmuM2S0, nmuM2S1)) =
  (linkOut, nmuS2M0 :> nmuS2M1 :> Nil)
 where
  (suS2M, nmuS2M0) = scatterUnitWb scatterConfig nmuM2S0 linkIn suM2S
  (linkOut, guS2M, nmuS2M1) = gatherUnitWb gatherConfig nmuM2S1 guM2S
  (_, wbM2Ss) = toSignals (processingElement peConfig) (pure $ JtagIn low low low, wbS2Ms)
  (suM2S, guM2S) = vecToTuple wbM2Ss
  wbS2Ms = suS2M :> guS2M :> Nil

{-# NOINLINE managementUnit #-}

gppeC ::
  (KnownNat nmuRemBusWidth, HiddenClockResetEnable dom) =>
  -- | Configures all local parameters
  GppeConfig nmuRemBusWidth ->
  -- |
  -- ( Incoming 'Bittide.Link'
  -- , Incoming @Vector@ of master busses
  -- )
  Circuit
    (CSignal dom (DataLink 64), Vec 2 (Wishbone dom 'Standard nmuRemBusWidth (Bytes 4)))
    (CSignal dom (DataLink 64))
gppeC (GppeConfig scatterConfig gatherConfig peConfig) = circuit $ \(linkIn, nmuWbs) -> do
  [wbScatCal, wbGathCal] <- idC -< nmuWbs
  jtag <- idleSource -< ()
  [wbScat, wbGu] <- processingElement peConfig -< jtag
  linkOut <- gatherUnitWbC gatherConfig -< (wbGu, wbGathCal)
  scatterUnitWbC scatterConfig -< (linkIn, wbScat, wbScatCal)
  idC -< linkOut

{- | A special purpose 'processingElement' that manages a Bittide Node. It contains
a 'processingElement', 'linkToPe' and 'peToLink' which create the interface for the
Bittide Link. It takes a 'ManagementConfig', incoming link and a vector of incoming
'WishboneS2M' signals and produces the outgoing link alongside a vector of
'WishboneM2S' signals.
-}
managementUnit ::
  forall dom nodeBusses.
  ( HiddenClockResetEnable dom
  , KnownNat nodeBusses
  , CLog 2 (nodeBusses + NmuInternalBusses) <= 30
  ) =>
  -- | Configures all local parameters.
  ManagementConfig nodeBusses ->
  -- | Incoming 'Bittide.Link'.
  Signal dom (DataLink 64) ->
  -- | Incoming @Vector@ of slave busses.
  Vec nodeBusses (Signal dom (WishboneS2M (Bytes 4))) ->
  -- |
  -- ( Outgoing 'Bittide.Link'
  -- , Outgoing @Vector@ of master busses)
  ( Signal dom (DataLink 64)
  , Vec nodeBusses (Signal dom (WishboneM2S (NmuRemBusWidth nodeBusses) 4 (Bytes 4)))
  )
managementUnit (ManagementConfig scatterConfig gatherConfig peConfig) linkIn nodeS2Ms =
  (linkOut, nodeM2Ss)
 where
  (suS2M, nmuS2M0) = scatterUnitWb scatterConfig nmuM2S0 linkIn suM2S
  (linkOut, guS2M, nmuS2M1) = gatherUnitWb gatherConfig nmuM2S1 guM2S
  (vecToTuple -> (suM2S, guM2S), rest) = splitAtI nmuM2Ss
  (vecToTuple -> (nmuM2S0, nmuM2S1), nodeM2Ss) = splitAtI rest
  (_, nmuM2Ss) = toSignals (processingElement peConfig) (pure $ JtagIn low low low, nmuS2Ms)
  nmuS2Ms = suS2M :> guS2M :> nmuS2M0 :> nmuS2M1 :> nodeS2Ms

managementUnitC ::
  forall dom nodeBusses.
  ( HiddenClockResetEnable dom
  , CLog 2 (nodeBusses + NmuInternalBusses) <= 30
  ) =>
  -- |
  -- ( Configures all local parameters
  -- , Incoming 'Bittide.Link'
  -- , Incoming @Vector@ of master busses
  -- )
  ManagementConfig nodeBusses ->
  Circuit
    (CSignal dom (DataLink 64))
    ( CSignal dom (DataLink 64)
    , Vec nodeBusses (Wishbone dom 'Standard (NmuRemBusWidth nodeBusses) (Bytes 4))
    )
managementUnitC (ManagementConfig scatterConfig gatherConfig peConfig) = circuit $ \linkIn -> do
  jtag <- idleSource -< ()
  peWbs <- processingElement peConfig -< jtag
  ([wbScatCal, wbScat, wbGathCal, wbGu], nmuWbs) <- splitAtC d4 -< peWbs
  linkOut <- gatherUnitWbC gatherConfig -< (wbGu, wbGathCal)
  scatterUnitWbC scatterConfig -< (linkIn, wbScat, wbScatCal)
  idC -< (linkOut, nmuWbs)

-- These functions should be added to `clash-protocols`, there is a PR for this:
-- https://github.com/clash-lang/clash-protocols/pull/116
-- And a bittide-hardware issue:
-- https://github.com/bittide/bittide-hardware/issues/645
-- Append two separate vectors of the same circuits into one vector of circuits
appendC ::
  (KnownNat n0) =>
  Circuit (Vec n0 circuit, Vec n1 circuit) (Vec (n0 + n1) circuit)
appendC = Circuit go
 where
  go ((fwd0, fwd1), splitAtI -> (bwd0, bwd1)) = ((bwd0, bwd1), (fwd0 ++ fwd1))

-- Append three separate vectors of the same circuits into one vector of circuits
appendC3 ::
  (KnownNat n0, KnownNat n1) =>
  Circuit (Vec n0 circuit, Vec n1 circuit, Vec n2 circuit) (Vec (n0 + n1 + n2) circuit)
appendC3 = Circuit go
 where
  go ((fwd0, fwd1, fwd2), splitAtI -> (bwd0, splitAtI -> (bwd1, bwd2))) = ((bwd0, bwd1, bwd2), (fwd0 ++ fwd1 ++ fwd2))

-- Transforms two vectors of circuits into a vector of tuples of circuits.
-- Only works if the two vectors have the same length.
zipC ::
  (KnownNat n) =>
  Circuit (Vec n a, Vec n b) (Vec n (a, b))
zipC = Circuit go
 where
  go ((fwd0, fwd1), bwd) = (unzip bwd, zip fwd0 fwd1)

-- Transforms three vectors of circuits into a vector of tuples of circuits.
-- Only works if the three vectors have the same length.
zipC3 ::
  (KnownNat n) =>
  Circuit (Vec n a, Vec n b, Vec n c) (Vec n (a, b, c))
zipC3 = Circuit go
 where
  go ((fwd0, fwd1, fwd2), bwd) = (unzip3 bwd, zip3 fwd0 fwd1 fwd2)

-- Split a vector of circuits into two vectors of circuits.
splitC ::
  (KnownNat n0) =>
  Circuit (Vec (n0 + n1) circuit) (Vec n0 circuit, Vec n1 circuit)
splitC = Circuit go
 where
  go (splitAtI -> (fwd0, fwd1), (bwd0, bwd1)) = (bwd0 ++ bwd1, (fwd0, fwd1))

-- Split a vector of circuits into three vectors of circuits.
splitC3 ::
  (KnownNat n0, KnownNat n1) =>
  Circuit (Vec (n0 + n1 + n2) circuit) (Vec n0 circuit, Vec n1 circuit, Vec n2 circuit)
splitC3 = Circuit go
 where
  go (splitAtI -> (fwd0, splitAtI -> (fwd1, fwd2)), (bwd0, bwd1, bwd2)) = (bwd0 ++ bwd1 ++ bwd2, (fwd0, fwd1, fwd2))

-- Unzip a vector of tuples of circuits into a tuple of vectors of circuits.
unzipC ::
  (KnownNat n) =>
  Circuit (Vec n (a, b)) (Vec n a, Vec n b)
unzipC = Circuit go
 where
  go (fwd, (bwd0, bwd1)) = (zip bwd0 bwd1, unzip fwd)

-- Unzip a vector of 3-tuples of circuits into a 3-tuple of vectors of circuits.
unzipC3 ::
  (KnownNat n) =>
  Circuit (Vec n (a, b, c)) (Vec n a, Vec n b, Vec n c)
unzipC3 = Circuit go
 where
  go (fwd, (bwd0, bwd1, bwd2)) = (zip3 bwd0 bwd1 bwd2, unzip3 fwd)

concatC ::
  (KnownNat n0, KnownNat n1) =>
  Circuit (Vec n0 (Vec n1 circuit)) (Vec (n0 * n1) circuit)
concatC = Circuit go
 where
  go (fwd, bwd) = (unconcat SNat bwd, concat fwd)

unconcatC ::
  (KnownNat n, KnownNat m) =>
  SNat m ->
  Circuit (Vec (n * m) circuit) (Vec n (Vec m circuit))
unconcatC SNat = Circuit go
 where
  go (fwd, bwd) = (concat bwd, unconcat SNat fwd)

-- | Map a circuit over a vector of circuits
vecMap :: forall n a b. Circuit a b -> Circuit (Vec n a) (Vec n b)
vecMap (Circuit f) = Circuit go
 where
  go (fwds, bwds) = unzip $ zipWith (curry f) fwds bwds

-- | Map a function over a Circuit of CSignals
cSigMap ::
  forall dom a b.
  (KnownDomain dom) =>
  (a -> b) ->
  Circuit (CSignal dom a) (CSignal dom b)
cSigMap fn = Circuit $ \(m, _) -> (pure (), fn <$> m)
