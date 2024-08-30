-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=6 #-}

module Bittide.Node where

import Clash.Prelude
import Clash.Sized.Vector.ToTuple (vecToTuple)

import Protocols
import Protocols.Wishbone

import VexRiscv

import Bittide.Calendar
import Bittide.DoubleBufferedRam
import Bittide.ProcessingElement
import Bittide.ScatterGather
import Bittide.SharedTypes
import Bittide.Switch

import Control.Arrow ((&&&))

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
    , nmuRemBusWidth ~ (32 - CLog 2 nmuBusses)
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
  Vec extLinks (Signal dom (DataLink 64)) ->
  Vec extLinks (Signal dom (DataLink 64))
node (NodeConfig nmuConfig switchConfig gppeConfigs) linksIn = linksOut
 where
  (switchOut, swS2M) = switch switchConfig swM2S switchIn
  switchIn = nmuToSwitch :> pesToSwitch ++ linksIn
  (splitAtI -> ((head &&& tail) -> (switchToNmu, switchToPes), linksOut)) = switchOut
  (nmuToSwitch, nmuM2Ss) = managementUnit nmuConfig switchToNmu nmuS2Ms
  (swM2S, peM2Ss) = (head &&& tail) nmuM2Ss

  nmuS2Ms = swS2M :> peS2Ms

  (pesToSwitch, concat -> peS2Ms) =
    unzip $ gppe <$> zip3 gppeConfigs switchToPes (unconcatI peM2Ss)

type NmuInternalBusses = 6
type NmuRemBusWidth nodeBusses = 32 - CLog 2 (nodeBusses + NmuInternalBusses)

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
  (KnownNat nmuRemBusWidth, 2 <= nmuRemBusWidth, HiddenClockResetEnable dom) =>
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

{- | A special purpose 'processingElement' that manages a Bittide Node. It contains
a 'processingElement', 'linkToPe' and 'peToLink' which create the interface for the
Bittide Link. It takes a 'ManagementConfig', incoming link and a vector of incoming
'WishboneS2M' signals and produces the outgoing link alongside a vector of
'WishhboneM2S' signals.
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
