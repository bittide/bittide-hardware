-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# OPTIONS -fplugin=Protocols.Plugin #-}
-- {-# OPTIONS -fplugin-opt=Protocols.Plugin:debug #-}

module Bittide.Node where

import Clash.Prelude

import Clash.Class.BitPackC (ByteOrder)
import GHC.Stack (HasCallStack)
import Protocols
import Protocols.Extra
import Protocols.Idle
import Protocols.MemoryMap (ConstBwd, MM)
import Protocols.Vec (vecCircuits)
import Protocols.Wishbone
import VexRiscv

import Bittide.Calendar
import Bittide.Jtag
import Bittide.ProcessingElement
import Bittide.ScatterGather
import Bittide.SharedTypes
import Bittide.Switch

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
    , nmuRemBusWidth ~ RemainingBusWidth nmuBusses
    , nmuRemBusWidth <= 30
    ) =>
    -- | Configuration for the 'node's 'managementUnit'.
    ManagementConfig ((BussesPerGppe * gppes) + 1) ->
    -- | Configuratoin for the 'node's 'switch'.
    CalendarConfig nmuRemBusWidth (CalendarEntry (externalLinks + gppes + 1)) ->
    -- | Configuration for all the node's 'gppe's.
    Vec gppes (GppeConfig nmuRemBusWidth) ->
    NodeConfig externalLinks gppes

-- | A 'node' consists of a 'switch', 'managementUnit' and @0..n@ 'gppe's.
node ::
  forall dom extLinks gppes.
  ( HiddenClockResetEnable dom
  , KnownNat extLinks
  , KnownNat gppes
  , ?busByteOrder :: ByteOrder
  , ?regByteOrder :: ByteOrder
  , PrefixWidth ((BussesPerGppe * gppes) + 1 + NmuInternalBusses) <= 30
  ) =>
  NodeConfig extLinks gppes ->
  Circuit
    ( ConstBwd MM, Vec gppes (ConstBwd MM), Vec extLinks (CSignal dom (BitVector 64)), Jtag dom)
    (Vec extLinks (CSignal dom (BitVector 64)))
node (NodeConfig nmuConfig switchConfig gppeConfigs) =
  circuit $ \(mmNmu, mms, linksIn, jtagIn) -> do
    (switchOut, _cal) <- switchC switchConfig -< (mmSwWb, (switchIn, swWb))
    switchIn <- appendC3 -< ([nmuLinkOut], pesToSwitch, linksIn)
    ([Fwd nmuLinkIn], Fwd switchToPes, linksOut) <- split3CI -< switchOut
    
    -- Create JTAG chain for management unit + all GPPEs
    jtagChainOut <- jtagChain @dom @(gppes + 1) -< jtagIn
    ([jtagNmu], jtagGppes) <- splitAtCI -< jtagChainOut
    
    (nmuLinkOut, nmuWbs0) <- managementUnitC nmuConfig nmuLinkIn -< (mmNmu, jtagNmu)
    ([(mmSwWb, swWb)], nmuWbs1) <- splitAtCI -< nmuWbs0
    peWbs <- unconcatC d2 -< nmuWbs1

    pesToSwitch <- vecCircuits (gppeC <$> gppeConfigs <*> switchToPes) <| zipC -< (mms, peWbs, jtagGppes)
    idC -< linksOut

type NmuInternalBusses = 6
type NmuRemBusWidth nodeBusses = RemainingBusWidth (nodeBusses + NmuInternalBusses)

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
    DumpVcd ->
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
  ( HasCallStack
  , KnownNat nmuRemBusWidth
  , HiddenClockResetEnable dom
  , ?busByteOrder :: ByteOrder
  , ?regByteOrder :: ByteOrder
  ) =>
  -- | Configures all local parameters
  GppeConfig nmuRemBusWidth ->
  -- |
  -- ( Incoming 'Bittide.Link'
  -- , Incoming @Vector@ of master busses
  -- )
  Signal dom (BitVector 64) ->
  Circuit
    ( ConstBwd MM
    , Vec 2 (ConstBwd MM, Wishbone dom 'Standard nmuRemBusWidth (Bytes 4))
    , Jtag dom
    )
    (CSignal dom (BitVector 64))
gppeC (GppeConfig scatterConfig gatherConfig peConfig dumpVcd) linkIn = circuit $ \(mm, nmuWbs, jtag) -> do
  [(mmSCal, wbScatCal), (mmGCal, wbGathCal)] <- idC -< nmuWbs
  [(mmS, wbScat), (mmG, wbGu)] <- processingElement dumpVcd peConfig -< (mm, jtag)
  scatterUnitWbC scatterConfig linkIn -< ((mmS, wbScat), (mmSCal, wbScatCal))
  linkOut <- gatherUnitWbC gatherConfig -< ((mmG, wbGu), (mmGCal, wbGathCal))
  idC -< linkOut

{- | A special purpose 'processingElement' that manages a Bittide Node. It contains
a 'processingElement', 'linkToPe' and 'peToLink' which create the interface for the
Bittide Link. It takes a 'ManagementConfig', incoming link and a vector of incoming
'WishboneS2M' signals and produces the outgoing link alongside a vector of
'WishboneM2S' signals.
-}
managementUnitC ::
  forall dom nodeBusses.
  ( HiddenClockResetEnable dom
  , ?busByteOrder :: ByteOrder
  , ?regByteOrder :: ByteOrder
  , PrefixWidth (nodeBusses + NmuInternalBusses) <= 30
  ) =>
  -- |
  -- ( Configures all local parameters
  -- , Incoming 'Bittide.Link'
  -- , Incoming @Vector@ of master busses
  -- )
  ManagementConfig nodeBusses ->
  Signal dom (BitVector 64) ->
  Circuit
    (ConstBwd MM, Jtag dom)
    ( CSignal dom (BitVector 64)
    , Vec
        nodeBusses
        (ConstBwd MM, Wishbone dom 'Standard (NmuRemBusWidth nodeBusses) (Bytes 4))
    )
managementUnitC
  ( ManagementConfig
      scatterConfig
      gatherConfig
      peConfig
      dumpVcd
    )
  linkIn = circuit $ \(mm, jtag) -> do
    peWbs <- processingElement dumpVcd peConfig -< (mm, jtag)
    ( [ wbScatCal
        , wbScat
        , wbGathCal
        , wbGu
        ]
      , nmuWbs
      ) <-
      splitAtCI -< peWbs
    linkOut <- gatherUnitWbC gatherConfig -< (wbGu, wbGathCal)
    scatterUnitWbC scatterConfig linkIn -< (wbScat, wbScatCal)
    idC -< (linkOut, nmuWbs)
