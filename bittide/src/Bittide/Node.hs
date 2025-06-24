-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# OPTIONS -fplugin=Protocols.Plugin #-}
-- {-# OPTIONS -fplugin-opt=Protocols.Plugin:debug #-}

module Bittide.Node where

import Clash.Prelude

import GHC.Stack (HasCallStack)
import Protocols
import Protocols.Extra
import Protocols.Idle
import Protocols.MemoryMap (ConstBwd, MM, MemoryMap, constBwd)
import Protocols.Wishbone
import VexRiscv

import Bittide.Calendar
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
    (ConstBwd MM, Vec gppes (ConstBwd MM), Vec extLinks (CSignal dom (BitVector 64)))
    (Vec extLinks (CSignal dom (BitVector 64)))
node (NodeConfig nmuConfig switchConfig gppeConfigs prefixes) = circuit $ \(mmNmu, mms, linksIn) -> do
  (switchOut, _cal) <- switchC switchConfig -< (mmSwWb, (switchIn, swWb))
  switchIn <- appendC3 -< ([nmuLinkOut], pesToSwitch, linksIn)
  ([Fwd nmuLinkIn], switchToPes, linksOut) <- split3CI -< switchOut
  (nmuLinkOut, nmuWbs0) <- managementUnitC nmuConfig nmuLinkIn -< mmNmu
  ([(preSw, (mmSwWb, swWb))], nmuWbs1) <- splitAtCI -< nmuWbs0
  peWbs <- unconcatC d2 -< nmuWbs1

  constBwd 0b0 -< preSw

  pesToSwitch <- nodeGppes gppeConfigs prefixes -< (mms, switchToPes, peWbs)
  idC -< linksOut

nodeGppes ::
  forall gppes dom nmuBusses nmuRemBusWidth.
  (KnownNat gppes, HiddenClockResetEnable dom, KnownNat nmuBusses, KnownNat nmuRemBusWidth) =>
  Vec gppes (GppeConfig nmuRemBusWidth) ->
  Vec gppes (Vec 2 (Unsigned (CLog 2 nmuBusses))) ->
  Circuit
    ( Vec gppes (ConstBwd MM)
    , Vec gppes (CSignal dom (BitVector 64))
    , Vec
        gppes
        ( Vec
            2
            ( ConstBwd (Unsigned (CLog 2 nmuBusses))
            , ( ConstBwd MM
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
    ((mm, interfacesOut), linkOut) = toSignals (gppeC config linkIn) (((), ((),) <$> m2ss), pure ())

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
  Signal dom (BitVector 64) ->
  Circuit
    ( ConstBwd MM
    , Vec 2 (ConstBwd MM, Wishbone dom 'Standard nmuRemBusWidth (Bytes 4))
    )
    (CSignal dom (BitVector 64))
gppeC (GppeConfig scatterConfig prefixS gatherConfig prefixG peConfig dumpVcd) linkIn = circuit $ \(mm, nmuWbs) -> do
  [(mmSCal, wbScatCal), (mmGCal, wbGathCal)] <- idC -< nmuWbs
  jtag <- idleSource
  [(preS, (mmS, wbScat)), (preG, (mmG, wbGu))] <-
    processingElement dumpVcd peConfig -< (mm, jtag)
  constBwd prefixS -< preS
  constBwd prefixG -< preG
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
  , CLog 2 (nodeBusses + NmuInternalBusses) <= 30
  ) =>
  -- |
  -- ( Configures all local parameters
  -- , Incoming 'Bittide.Link'
  -- , Incoming @Vector@ of master busses
  -- )
  ManagementConfig nodeBusses ->
  Signal dom (BitVector 64) ->
  Circuit
    (ConstBwd MM)
    ( CSignal dom (BitVector 64)
    , Vec
        nodeBusses
        ( ConstBwd (Unsigned (CLog 2 (nodeBusses + NmuInternalBusses)))
        , (ConstBwd MM, Wishbone dom 'Standard (NmuRemBusWidth nodeBusses) (Bytes 4))
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
    )
  linkIn = circuit $ \mm -> do
    jtag <- idleSource
    peWbs <- processingElement dumpVcd peConfig -< (mm, jtag)
    ( [ (preSCal, wbScatCal)
        , (preS, (mmScat, wbScat))
        , (preGCal, wbGathCal)
        , (preG, wbGu)
        ]
      , nmuWbs
      ) <-
      splitAtCI -< peWbs
    constBwd prefixSCal -< preSCal
    constBwd prefixS -< preS
    constBwd prefixGCal -< preGCal
    constBwd prefixG -< preG
    linkOut <- gatherUnitWbC gatherConfig -< (wbGu, wbGathCal)
    scatterUnitWbC scatterConfig linkIn -< ((mmScat, wbScat), wbScatCal)
    idC -< (linkOut, nmuWbs)
