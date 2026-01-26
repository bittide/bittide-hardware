-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
-- {-# OPTIONS -fplugin-opt=Protocols.Plugin:debug #-}

module Bittide.Node where

import Clash.Prelude

import Clash.Class.BitPackC (ByteOrder)
import Clash.Cores.Xilinx.Unisim.DnaPortE2 (simDna2)
import GHC.Stack (HasCallStack)
import Protocols
import Protocols.MemoryMap (
  MemoryMap (..),
  MemoryMapTree (WithName),
  Mm,
  locCaller,
 )
import Protocols.Vec (vecCircuits)
import VexRiscv

import Bittide.Calendar
import Bittide.CaptureUgn (captureUgn)
import Bittide.ElasticBuffer (ElasticBufferData)
import Bittide.Jtag
import Bittide.MetaPeConfig (metaPeConfig)
import Bittide.ProcessingElement
import Bittide.ScatterGather
import Bittide.SharedTypes
import Bittide.Switch
import Bittide.Wishbone (readDnaPortE2Wb, timeWb, uartBytes, uartInterfaceWb)

import qualified Protocols.Vec as Vec

{- | Each 'gppe' results in 2 busses for the 'managementUnit', namely:

* The 'calendar' for the 'scatterUnitWbC'.
* The 'calendar' for the 'gatherUnitWbC'.
-}
type BussesPerGppe = GppeBusses - GppeBussesNotExposed

-- | Configuration of a 'node'.
data Config linkCount gppes metaPeBufferWidth where
  Config ::
    forall linkCount gppes metaPeBufferWidth.
    ( KnownNat linkCount
    , KnownNat gppes
    , NmuPrefixWidth linkCount gppes <= 30
    ) =>
    { management :: ManagementConfig linkCount gppes
    -- ^ Configuration for the 'node's 'managementUnit'.
    , calendar ::
        CalendarConfig (NmuRemBusWidth linkCount gppes) (CalendarEntry (linkCount + gppes + 1))
    -- ^ Configuration for the 'node's 'switch'.
    , gppes :: Vec gppes (GppeConfig (NmuRemBusWidth linkCount gppes) metaPeBufferWidth)
    -- ^ Configuration for all the node's 'gppe's.
    } ->
    Config linkCount gppes metaPeBufferWidth

-- | A 'node' consists of a 'switch', 'managementUnitC' and @0..n@ 'gppeC's.
node ::
  forall dom linkCount gppes metaPeBufferWidth.
  ( HiddenClockResetEnable dom
  , 1 <= DomainPeriod dom
  , ?busByteOrder :: ByteOrder
  , ?regByteOrder :: ByteOrder
  ) =>
  Config linkCount gppes metaPeBufferWidth ->
  Circuit
    ( ToConstBwd Mm
    , Vec gppes (ToConstBwd Mm)
    , Jtag dom
    , Vec linkCount (CSignal dom (ElasticBufferData (Maybe (BitVector 64))))
    )
    ( Vec linkCount (CSignal dom (BitVector 64))
    , CSignal dom (Unsigned 64)
    , (ToConstBwd Mm, NmuWishbone dom linkCount gppes)
    , Vec gppes (CSignal dom (BitVector 64))
    , Vec gppes (CSignal dom (BitVector 64))
    , Vec gppes (Df dom Byte)
    , CSignal dom (Vec (linkCount + gppes + 1) (Index (linkCount + gppes + 2)))
    )
node (Config muConfig switchConfig gppeConfigs) =
  circuit $ \(muMM, gppeMMs, jtag, Fwd rxs) -> do
    [muJtag, gppesJtagTap] <- jtagChain -< jtag
    gppesJtag <- jtagChain -< gppesJtagTap

    ( nmuLinkOut
      , Fwd localCounter
      , (switchMM, switchWb)
      , externalMMWb
      , captureUgnsMMWb
      , scatterCalsMMWb
      , gatherCalsMMWb
      ) <-
      managementUnitC muConfig -< (muMM, nmuLinkIn, muJtag)

    ugnRxs <- vecCircuits (captureUgn localCounter <$> rxs) -< captureUgnsMMWb

    (Fwd peLinksOut, peUartsOut) <-
      Vec.unzip
        <| vecCircuits (zipWith gppeC gppeConfigs (fromIntegral <$> indicesI))
        <| Vec.zip5
        -< (gppeMMs, Fwd switchToGppes, scatterCalsMMWb, gatherCalsMMWb, gppesJtag)

    switchIn <- Vec.append3 -< ([nmuLinkOut], Fwd peLinksOut, ugnRxs)
    (switchOut, cal) <-
      switchC @_ @_ @_ @_ @64 switchConfig -< (switchMM, (switchIn, switchWb))
    ([nmuLinkIn], Fwd switchToGppes, linksOut) <- Vec.split3 -< switchOut

    idC
      -< ( linksOut
         , Fwd localCounter
         , externalMMWb
         , Fwd switchToGppes
         , Fwd peLinksOut
         , peUartsOut
         , cal
         )

{- Type-level number of GPPE internal busses.

Internal busses:

  * Instruction bus (not exposed to MU, PE internal)
  * Data bus (not exposed to MU, PE internal)
  * 'whoAmIWb' (not exposed to MU, PE internal)
  * 'metaPeConfig' (not exposed to MU)
  * Scatter unit
  * Gather unit
  * UART (not exposed to MU)
-}
type GppeBussesNotExposed = PeInternalBusses + 3
type GppeBusses = GppeBussesNotExposed + 2
type GppeBusWidth = 30 - CLog 2 GppeBusses

{- | Configuration for a general purpose processing element together with its link to the
switch.
-}
data GppeConfig nmuRemBusWidth metaPeBufferWidth where
  GppeConfig ::
    forall nmuRemBusWidth metaPeBufferWidth.
    (KnownNat metaPeBufferWidth, 1 <= metaPeBufferWidth) =>
    { scatterConfig :: ScatterConfig 4 nmuRemBusWidth
    -- ^ Configuration for the scatter engine
    , gatherConfig :: GatherConfig 4 nmuRemBusWidth
    -- ^ Configuration for the gather engine
    , peConfig :: PeConfig GppeBusses
    -- ^ Configuration for a 'gppe's 'processingElement', which statically
    -- has four external busses connected to the instruction memory, data memory
    -- , 'scatterUnitWb' and 'gatherUnitWb'.
    , dumpVcd :: DumpVcd
    , metaPeConfigBufferWidth :: SNat metaPeBufferWidth
    } ->
    GppeConfig nmuRemBusWidth metaPeBufferWidth

{-# OPAQUE gppeC #-}

{- | A general purpose 'processingElement' to be part of a Bittide Node. It contains
a 'processingElement', a 'scatterUnitWbC', a 'gatherUnitWbC', and a 'readDnaPortE2Wb'.
It takes a 'GppeConfig', the incoming link from the switch, the scatter unit calendar
Wishbone bus, the gather unit calendar Wishbone bus, and a JTAG connection, and it
produces an outgoing link to the switch, and a UART output.
-}
gppeC ::
  forall dom nmuRemBusWidth metaPeBufferWidth.
  ( HasCallStack
  , KnownNat nmuRemBusWidth
  , HiddenClockResetEnable dom
  , ?busByteOrder :: ByteOrder
  , ?regByteOrder :: ByteOrder
  ) =>
  -- | Configures all local parameters
  GppeConfig nmuRemBusWidth metaPeBufferWidth ->
  -- | Index number of this GPPE
  Integer ->
  Circuit
    ( -- \| GPPE memory map
      ToConstBwd Mm
    , -- \| Incoming link from switch
      CSignal dom (BitVector 64)
    , -- \| Scatter unit calendar memory map and Wishbone bus
      (BitboneMm dom nmuRemBusWidth)
    , -- \| Gather unit calendar memory map and Wishbone bus
      (BitboneMm dom nmuRemBusWidth)
    , -- \| JTAG connection
      Jtag dom
    )
    (CSignal dom (BitVector 64), Df dom Byte)
gppeC cfg@GppeConfig{} (show -> idx) =
  circuit $ \(mm, Fwd linkIn, (scatterCalMM, scatterCalWb), (gatherCalMM, gatherCalWb), jtag) -> do
    [dnaBus, (scatterMM, scatterWb), (gatherMM, gatherWb), metaPeBus, uartBus] <-
      processingElement cfg.dumpVcd cfg.peConfig -< (mm, jtag)
    _dna <- readDnaPortE2Wb simDna2 -< dnaBus
    metaPeConfig cfg.metaPeConfigBufferWidth -< metaPeBus
    withNamesSG
      ("ScatterUnitPe" <> idx)
      ("ScatterCalPe" <> idx)
      (scatterUnitWbC cfg.scatterConfig linkIn)
      -< ((scatterMM, scatterWb), (scatterCalMM, scatterCalWb))
    linkOut <-
      withNamesSG
        ("GatherUnitPe" <> idx)
        ("GatherCalPe" <> idx)
        (gatherUnitWbC cfg.gatherConfig)
        -< ((gatherMM, gatherWb), (gatherCalMM, gatherCalWb))
    (gppeUartBytes, _uartStatus) <-
      uartInterfaceWb d16 d16 uartBytes -< (uartBus, Fwd (pure Nothing))
    idC -< (linkOut, gppeUartBytes)

{- Type-level number of node management unit internal busses

Internal busses:

  * Instruction bus
  * Data bus
  * Scatter unit
  * Scatter calendar
  * Gather unit
  * Gather calendar
  * Switch
  * 'timeWb'
  * External link
  * 'linkCount' number of 'captureUgn' components
  * 'gppes' number of Scatter calendars
  * 'gppes' number of Gather calendars
-}
type NmuBusses linkCount gppes = PeInternalBusses + 7 + linkCount + 2 * gppes
type NmuPrefixWidth linkCount gppes = CLog 2 (NmuBusses linkCount gppes + 1)
type NmuRemBusWidth linkCount gppes = 30 - NmuPrefixWidth linkCount gppes
type NmuWishbone dom linkCount gppes = Bitbone dom (NmuRemBusWidth linkCount gppes)

{- | Configuration for the 'managementUnit' and its 'Bittide.Link'.
The management unit contains the 4 wishbone busses that each pe has
and also the management busses for itself and all other pe's in this node.
Furthermore it also has access to the 'calendar' for the 'switch'.
-}
data ManagementConfig linkCount gppes where
  ManagementConfig ::
    ( KnownNat gppes
    , KnownNat linkCount
    , NmuPrefixWidth linkCount gppes <= 30
    ) =>
    { scatterConfig :: ScatterConfig 4 (NmuRemBusWidth linkCount gppes)
    -- ^ Configuration for the management unit scatter unit
    , gatherConfig :: GatherConfig 4 (NmuRemBusWidth linkCount gppes)
    -- ^ Configuration for the management unit gather unit
    , peConfig :: PeConfig (NmuBusses linkCount gppes)
    -- ^ Processing element configuration
    , dumpVcd :: DumpVcd
    -- ^ VCD dump configuration
    } ->
    ManagementConfig linkCount gppes

{- | A special purpose 'processingElement' that manages a Bittide Node. It contains
a 'processingElement', 'scatterUnitWbC', 'gatherUnitWbC', and 'timeWb'. It takes a
'ManagementConfig', incoming link from the switch, and a JTAG connection, and outputs
an outgoing link to the switch, its local counter, a 'WishboneM2S' for a 'switchC', a
'WishboneM2S' for an arbitrary external Wishbone connection, a 'WishboneM2S' for each
'captureUgn' in the 'node', a vector of 'WishboneM2S's for the calendar of each GPPE's
'gatherUnitWbC', and a vector of 'WishboneM2S's for the calendar of each GPPE's
'scatterUnitWbC'.
-}
managementUnitC ::
  forall dom linkCount gppes.
  ( HiddenClockResetEnable dom
  , 1 <= DomainPeriod dom
  , ?busByteOrder :: ByteOrder
  , ?regByteOrder :: ByteOrder
  ) =>
  ManagementConfig linkCount gppes ->
  Circuit
    (ToConstBwd Mm, CSignal dom (BitVector 64), Jtag dom)
    ( -- \| Node management unit link output
      CSignal dom (BitVector 64)
    , -- \| Node management unit local counter
      CSignal dom (Unsigned 64)
    , -- \| Switch memory map and Wishbone bus
      (ToConstBwd Mm, NmuWishbone dom linkCount gppes)
    , -- \| External connection memory map and Wishbone bus
      (ToConstBwd Mm, NmuWishbone dom linkCount gppes)
    , -- \| 'captureUgn's memory maps and Wishbone busses
      Vec
        linkCount
        (ToConstBwd Mm, NmuWishbone dom linkCount gppes)
    , -- \| GPPE scatter unit memory maps and Wishbone busses
      Vec
        gppes
        (ToConstBwd Mm, NmuWishbone dom linkCount gppes)
    , -- \| GPPE gather unit memory maps and Wishbone busses
      Vec
        gppes
        (ToConstBwd Mm, NmuWishbone dom linkCount gppes)
    )
managementUnitC (ManagementConfig{scatterConfig, gatherConfig, peConfig, dumpVcd}) =
  circuit $ \(mm, Fwd linkIn, jtag) -> do
    peWbs <- processingElement dumpVcd peConfig -< (mm, jtag)
    ( [ (myScatterMM, myScatterWb)
        , myScatterCalMMWb
        , myGatherMMWb
        , myGatherCalMMWb
        , switchMMWb
        , timeMMWb
        , externalMMWb
        ]
      , myWbRest
      ) <-
      Vec.split -< peWbs

    localCounter <- timeWb Nothing -< timeMMWb

    withNamesSG "ScatterUnitMu" "ScatterCalMu" (scatterUnitWbC scatterConfig linkIn)
      -< ((myScatterMM, myScatterWb), myScatterCalMMWb)
    linkOut <-
      withNamesSG "GatherUnitMu" "GatherCalMu" (gatherUnitWbC gatherConfig)
        -< (myGatherMMWb, myGatherCalMMWb)

    (captureUgnMMWbs, scatterMMWbs, gatherMMWbs) <- Vec.split3 -< myWbRest

    idC
      -< ( linkOut
         , localCounter
         , switchMMWb
         , externalMMWb
         , captureUgnMMWbs
         , scatterMMWbs
         , gatherMMWbs
         )

{- | Like the existing 'Protocols.MemoryMap.withName' function, but it handles the structure
of the scatter/gather engines' types.
-}
withNamesSG ::
  forall a b c.
  (HasCallStack) =>
  String ->
  String ->
  Circuit ((ToConstBwd Mm, a), (ToConstBwd Mm, b)) c ->
  Circuit ((ToConstBwd Mm, a), (ToConstBwd Mm, b)) c
withNamesSG nameA nameB (Circuit f) = Circuit go
 where
  go ((((), fwdA), ((), fwdB)), bwdC) = (((SimOnly mmA', bwdA), (SimOnly mmB', bwdB)), fwdC)
   where
    (((SimOnly mmA, bwdA), (SimOnly mmB, bwdB)), fwdC) = f ((((), fwdA), ((), fwdB)), bwdC)
    mmA' = mmA{tree = WithName locCaller nameA mmA.tree}
    mmB' = mmB{tree = WithName locCaller nameB mmB.tree}
