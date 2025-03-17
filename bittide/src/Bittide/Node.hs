-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=6 #-}

{-# OPTIONS -fplugin=Protocols.Plugin #-}
-- {-# OPTIONS -fplugin-opt=Protocols.Plugin:debug #-}

module Bittide.Node where

import Clash.Prelude

import Protocols
import Protocols.Idle
import Protocols.Wishbone
import VexRiscv

import Bittide.Calendar
import Bittide.ProcessingElement
import Bittide.ScatterGather
import Bittide.SharedTypes
import Bittide.Switch
import GHC.Stack (HasCallStack)
import Protocols.MemoryMap (ConstB, MM, MemoryMap, constB)

{- | A simple node consisting of one external bidirectional link and two 'gppe's.
This node's 'switch' has a 'CalendarConfig' of for a 'calendar' with up to @1024@ entries,
however, the 'calendar' is initialized with a single entry of repeated zeroes.
The 'scatterUnitWb's and 'gatherUnitWb's are initialized with 'CalendarConfig's of all
zeroes. The 'gppe's initial memories are both undefined and the 'MemoryMap' is a
vector of ever increasing base addresses (increments of 0x1000).
-}

-- simpleNodeConfig :: NodeConfig 1 2
-- simpleNodeConfig =
--   NodeConfig
--     (ManagementConfig (ScatterConfig sgConfig) (GatherConfig sgConfig) nmuConfig NoDumpVcd)
--     switchCal
--     (repeat (GppeConfig (ScatterConfig sgConfig) (GatherConfig sgConfig) peConfig NoDumpVcd))
--  where
--   switchCal = CalendarConfig (SNat @1024) (switchEntry :> Nil) (switchEntry :> Nil)
--   sgConfig = CalendarConfig (SNat @1024) (sgEntry :> Nil) (sgEntry :> Nil)
--   peConfig = PeConfig memMapPe (Undefined @8192) (Undefined @8192) d0 d0
--   nmuConfig = PeConfig memMapNmu (Undefined @8192) (Undefined @8192) d0 d0
--   memMapPe = iterateI (+ 0x1000) 0
--   memMapNmu = iterateI (+ 0x1000) 0
--   switchEntry = ValidEntry{veEntry = repeat 0, veRepeat = 0 :: Unsigned 0}
--   sgEntry = ValidEntry{veEntry = 0 :: Index 1024, veRepeat = 0 :: Unsigned 0}

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
    -- | prefixes for the calendar wishbone interfaces of all GPPEs
    Vec gppes (Vec 2 (Unsigned (CLog 2 nmuBusses))) ->
    NodeConfig externalLinks gppes

-- | A 'node' consists of a 'switch', 'managementUnit' and @0..n@ 'gppe's.
node ::
  forall dom extLinks gppes.
  (HiddenClockResetEnable dom, KnownNat extLinks, KnownNat gppes) =>
  NodeConfig extLinks gppes ->
  Circuit
    (ConstB MM, Vec gppes (ConstB MM), Vec extLinks (CSignal dom (BitVector 64)))
    (Vec extLinks (CSignal dom (BitVector 64)))
node (NodeConfig nmuConfig switchConfig gppeConfigs prefixes) = circuit $ \(mmNmu, mms, linksIn) -> do
  switchOut <- switchC @_ @_ @_ @_ @64 switchConfig -< (mmSwWb, (switchIn, swWb))
  switchIn <- appendC3 -< ([nmuLinkOut], pesToSwitch, linksIn)
  ([nmuLinkIn], switchToPes, linksOut) <- splitC3 -< switchOut
  (nmuLinkOut, nmuWbs0) <- managementUnitC nmuConfig -< (mmNmu, nmuLinkIn)
  ([(preSw, (mmSwWb, swWb))], nmuWbs1) <- splitAtC d1 -< nmuWbs0
  peWbs <- unconcatC d2 -< nmuWbs1

  constB 0b0 -< preSw

  pesToSwitch <- nodeGppes gppeConfigs prefixes -< (mms, switchToPes, peWbs)
  idC -< linksOut

nodeGppes ::
  forall gppes dom nmuBusses nmuRemBusWidth.
  (KnownNat gppes, HiddenClockResetEnable dom, KnownNat nmuBusses, KnownNat nmuRemBusWidth) =>
  Vec gppes (GppeConfig nmuRemBusWidth) ->
  Vec gppes (Vec 2 (Unsigned (CLog 2 nmuBusses))) ->
  Circuit
    ( Vec gppes (ConstB MM)
    , Vec gppes (CSignal dom (BitVector 64))
    , Vec
        gppes
        ( Vec
            2
            ( ConstB (Unsigned (CLog 2 nmuBusses))
            , ( ConstB MM
              , Wishbone dom Standard nmuRemBusWidth (BitVector 32)
              )
            )
        )
    )
    (Vec gppes (CSignal dom (BitVector 64)))
nodeGppes configs prefixes = Circuit go
 where
  go ::
    ( ( Vec gppes ()
      , Vec gppes (Signal dom (BitVector 64))
      , Vec gppes (Vec 2 ((), ((), Signal dom (WishboneM2S nmuRemBusWidth 4 (BitVector 32)))))
      )
    , Vec gppes (Signal dom ())
    ) ->
    ( ( Vec gppes (SimOnly MemoryMap)
      , Vec gppes (Signal dom ())
      , Vec
          gppes
          ( Vec
              2
              ( Unsigned (CLog 2 nmuBusses)
              , (SimOnly MemoryMap, Signal dom (WishboneS2M (BitVector 32)))
              )
          )
      )
    , Vec gppes (Signal dom (BitVector 64))
    )
  go ((_, linksIn, map (snd . snd <$>) -> m2ss), _) = ((mms, repeat (pure ()), interfaces), linksOut)
   where
    (unzip3 -> (mms, interfaces, linksOut)) = singleGppe <$> configs <*> linksIn <*> prefixes <*> m2ss

  singleGppe ::
    -- forall nmuRemBusWidth dom nmuBusses.
    -- (KnownNat nmuRemBusWidth, HiddenClockResetEnable dom) =>
    GppeConfig nmuRemBusWidth ->
    Signal dom (BitVector 64) ->
    Vec 2 (Unsigned (CLog 2 nmuBusses)) ->
    Vec 2 (Signal dom (WishboneM2S nmuRemBusWidth 4 (BitVector 32))) ->
    ( SimOnly MemoryMap
    , Vec
        2
        ( Unsigned (CLog 2 nmuBusses)
        , (SimOnly MemoryMap, Signal dom (WishboneS2M (BitVector 32)))
        )
    , Signal dom (BitVector 64)
    )
  singleGppe config linkIn prefixes' m2ss = (mm, zip prefixes' interfacesOut, linkOut)
   where
    ((mm, (_, interfacesOut)), linkOut) = toSignals (gppeC config) (((), (linkIn, ((),) <$> m2ss)), pure ())

-- node ::
--   forall dom extLinks gppes.
--   (HiddenClockResetEnable dom, KnownNat extLinks, KnownNat gppes) =>
--   NodeConfig extLinks gppes ->
--   Circuit
--     (ConstB MM, Vec gppes (ConstB MM), Vec extLinks (CSignal dom (BitVector 64)))
--     (Vec extLinks (CSignal dom (BitVector 64)))
-- node (NodeConfig nmuConfig switchConfig gppeConfigs) = Circuit go
--  where
--   go :: (((), Vec gppes (), Vec extLinks (Signal dom (BitVector 64))), Vec extLinks (Signal dom ())) -> ((SimOnly MemoryMap, Vec gppes (SimOnly MemoryMap), Vec extLinks (Signal dom ())), Vec extLinks (Signal dom (BitVector 64)))
--   go (((), _, linksIn), _) = ((mmNmu, mmPes, repeat (pure ())), linksOut)
--    where
--     (_, _) = toSignals (switchC @_ @_ @_ @_ @64 switchConfig) (((), (_, _)), repeat (pure ()))
--
--     (_, _) = toSignals (managementUnitC nmuConfig) (((), _), (pure (), _))

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
    Unsigned (CLog 2 (nodeBusses + NmuInternalBusses)) ->
    Unsigned (CLog 2 (nodeBusses + NmuInternalBusses)) ->
    GatherConfig 4 (NmuRemBusWidth nodeBusses) ->
    Unsigned (CLog 2 (nodeBusses + NmuInternalBusses)) ->
    Unsigned (CLog 2 (nodeBusses + NmuInternalBusses)) ->
    PeConfig (nodeBusses + NmuInternalBusses) ->
    DumpVcd ->
    ManagementConfig nodeBusses

{- | Configuration for a general purpose processing element together with its link to the
switch.
-}
data GppeConfig nmuRemBusWidth where
  GppeConfig ::
    ScatterConfig 4 nmuRemBusWidth ->
    -- | Interconnect prefix for the scatter engine
    Unsigned 2 ->
    GatherConfig 4 nmuRemBusWidth ->
    -- | Interconnect prefix for the gather engine
    Unsigned 2 ->
    -- | Configuration for a 'gppe's 'processingElement', which statically
    -- has four external busses connected to the instruction memory, data memory
    -- , 'scatterUnitWb' and 'gatherUnitWb'.
    PeConfig 4 ->
    DumpVcd ->
    GppeConfig nmuRemBusWidth

{-# NOINLINE gppeC #-}

{- | A general purpose 'processingElement' to be part of a Bittide Node. It contains
a 'processingElement', 'linkToPe' and 'peToLink' which create the interface for the
Bittide Link. It takes a 'GppeConfig', incoming link and two incoming 'WishboneM2S'
signals and produces the outgoing link alongside two 'WishhboneS2M' signals.
The order of Wishbone busses is as follows:
('scatterUnitWb' :> 'gatherUnitWb' :> Nil).
-}
gppeC ::
  (HasCallStack, KnownNat nmuRemBusWidth, HiddenClockResetEnable dom) =>
  -- | Configures all local parameters
  GppeConfig nmuRemBusWidth ->
  -- |
  -- ( Incoming 'Bittide.Link'
  -- , Incoming @Vector@ of master busses
  -- )
  Circuit
    ( ConstB MM
    , ( CSignal dom (BitVector 64)
      , Vec 2 (ConstB MM, Wishbone dom 'Standard nmuRemBusWidth (Bytes 4))
      )
    )
    (CSignal dom (BitVector 64))
gppeC (GppeConfig scatterConfig prefixS gatherConfig prefixG peConfig dumpVcd) = circuit $ \(mm, (linkIn, nmuWbs)) -> do
  [(mmSCal, wbScatCal), (mmGCal, wbGathCal)] <- idC -< nmuWbs
  jtag <- idleSource -< ()
  [(preS, (mmS, wbScat)), (preG, (mmG, wbGu))] <-
    processingElement dumpVcd peConfig -< (mm, jtag)
  constB prefixS -< preS
  constB prefixG -< preG
  scatterUnitWbC scatterConfig -< ((mmS, (linkIn, wbScat)), (mmSCal, wbScatCal))
  linkOut <- gatherUnitWbC gatherConfig -< ((mmG, wbGu), (mmGCal, wbGathCal))
  idC -< linkOut

-- gppeC ::
--   forall dom nmuRemBusWidth .
--   (HasCallStack, KnownNat nmuRemBusWidth, HiddenClockResetEnable dom) =>
--   -- | Configures all local parameters
--   GppeConfig nmuRemBusWidth ->
--   -- |
--   -- ( Incoming 'Bittide.Link'
--   -- , Incoming @Vector@ of master busses
--   -- )
--   Circuit
--     (ConstB MM, (CSignal dom (BitVector 64), Vec 2 (ConstB MM, Wishbone dom 'Standard nmuRemBusWidth (Bytes 4))))
--     (CSignal dom (BitVector 64))
-- gppeC (GppeConfig scatterConfig prefixS gatherConfig prefixG peConfig dumpVcd) = Circuit go
--  where
--   go (((), (linkIn, nmuWbs)), _) = ((mmPe, (pure (), calOut)), linkOut)
--    where
--     ((), jtagIn) = toSignals (idleSource @(Jtag dom)) ((), jtagOut)
--     peC = processingElement dumpVcd peConfig
--     ((mmPe, jtagOut), unzip -> (_, unzip -> (_, vecToTuple -> (scatM2S, gathM2S)))) = toSignals peC (((), jtagIn), s2mss)
--
--     (((), scatCalM2S), ((), gatCalM2S)) = vecToTuple nmuWbs
--
--     calOut = (mmScatCal, scatCalS2M) :> (mmGathCal, gathCalS2M) :> Nil
--
--     (((mmScat, (_, scatS2M)), (mmScatCal, scatCalS2M)), ()) =
--       toSignals (scatterUnitWbC scatterConfig) ((((), (linkIn, scatM2S)), ((), scatCalM2S)), ())
--
--     (((mmGath, gathS2M), (mmGathCal, gathCalS2M)), linkOut) =
--       toSignals (gatherUnitWbC gatherConfig) ((((), gathM2S), ((), gatCalM2S)), pure ())
--
--     s2mss = (prefixS, (mmScat, scatS2M)) :> (prefixG, (mmGath, gathS2M)) :> Nil

{- | A special purpose 'processingElement' that manages a Bittide Node. It contains
a 'processingElement', 'linkToPe' and 'peToLink' which create the interface for the
Bittide Link. It takes a 'ManagementConfig', incoming link and a vector of incoming
'WishboneS2M' signals and produces the outgoing link alongside a vector of
'WishboneM2S' signals.
-}
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
    (ConstB MM, CSignal dom (BitVector 64))
    ( CSignal dom (BitVector 64)
    , Vec
        nodeBusses
        ( ConstB (Unsigned (CLog 2 (nodeBusses + NmuInternalBusses)))
        , (ConstB MM, Wishbone dom 'Standard (NmuRemBusWidth nodeBusses) (Bytes 4))
        )
    )
managementUnitC
  ( ManagementConfig
      scatterConfig
      prefixSCal
      prefixS
      gatherConfig
      prefixGCal
      prefixG
      peConfig
      dumpVcd
    ) = circuit $ \(mm, linkIn) -> do
    jtag <- idleSource -< ()
    peWbs <- processingElement dumpVcd peConfig -< (mm, jtag)
    ( [ (preSCal, wbScatCal)
        , (preS, (mmScat, wbScat))
        , (preGCal, wbGathCal)
        , (preG, wbGu)
        ]
      , nmuWbs
      ) <-
      splitAtC d4 -< peWbs
    constB prefixSCal -< preSCal
    constB prefixS -< preS
    constB prefixGCal -< preGCal
    constB prefixG -< preG
    linkOut <- gatherUnitWbC gatherConfig -< (wbGu, wbGathCal)
    scatterUnitWbC scatterConfig -< ((mmScat, (linkIn, wbScat)), wbScatCal)
    idC -< (linkOut, nmuWbs)

-- managementUnitC ::
--   forall dom nodeBusses.
--   ( HiddenClockResetEnable dom
--   , CLog 2 (nodeBusses + NmuInternalBusses) <= 30
--   ) =>
--   -- |
--   -- ( Configures all local parameters
--   -- , Incoming 'Bittide.Link'
--   -- , Incoming @Vector@ of master busses
--   -- )
--   ManagementConfig nodeBusses ->
--   Circuit
--     (ConstB MM, CSignal dom (BitVector 64))
--     ( CSignal dom (BitVector 64)
--     , Vec nodeBusses (ConstB (Unsigned (CLog 2 (nodeBusses + NmuInternalBusses))), (ConstB MM, Wishbone dom 'Standard (NmuRemBusWidth nodeBusses) (Bytes 4))
--     ))
-- managementUnitC (ManagementConfig scatterConfig prefixSCal prefixS gatherConfig prefixGCal prefixG peConfig dumpVcd) = Circuit go
--  where
--   go :: (((), Signal dom (BitVector 64)), (Signal dom (), Vec nodeBusses (Unsigned (CLog 2 (nodeBusses + NmuInternalBusses)), (SimOnly MemoryMap, Signal dom (WishboneS2M (BitVector 32)))))) -> ((SimOnly MemoryMap, Signal dom ()), (Signal dom (BitVector 64), Vec nodeBusses ((), ((), Signal dom (WishboneM2S (30 - CLog 2 (nodeBusses + NmuInternalBusses)) 4 (BitVector 32))))))
--   go (((), linkIn), (_, nmuWbsS2M)) = ((mmPe, pure ()), (linkOut, nmuWbsM2S))
--    where
--     ((), jtagIn) = toSignals (idleSource @(Jtag dom)) ((), jtagOut)
--     peC = processingElement dumpVcd peConfig
--     ((mmPe, jtagOut), peWbsM2S) = toSignals peC (((), jtagIn), peWbsS2M)
--
--     (vecToTuple . (snd . snd <$>) -> (scatCalM2S, scatM2S, gathCalM2S, gathM2S), nmuWbsM2S) = splitAtI @4 peWbsM2S
--
--
--     (((mmScat, (_, scatS2M)), (mmScatCal, scatCalS2M)), ()) =
--       toSignals (scatterUnitWbC scatterConfig) ((((), (linkIn, scatM2S)), ((), scatCalM2S)), ())
--
--     (((mmGath, gathS2M), (mmGathCal, gathCalS2M)), linkOut) =
--       toSignals (gatherUnitWbC gatherConfig) ((((), gathM2S), ((), gathCalM2S)), pure ())
--
--     peWbsS2M =
--       (prefixSCal, (mmScatCal, scatCalS2M)) :>
--       (prefixS, (mmScat, scatS2M)) :>
--       (prefixGCal, (mmGathCal, gathCalS2M)) :>
--       (prefixG, (mmGath, gathS2M)) :> nmuWbsS2M

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
