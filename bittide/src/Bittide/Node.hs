-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=6 #-}

{-# OPTIONS -fplugin=Protocols.Plugin #-}
-- {-# OPTIONS -fplugin-opt=Protocols.Plugin:debug #-}

module Bittide.Node where

import Clash.Prelude

import Clash.Class.BitPackC (ByteOrder)
import Clash.Cores.Xilinx.Unisim.DnaPortE2 (simDna2)
import GHC.Stack (HasCallStack)
import Protocols
import Protocols.Extra
import Protocols.Idle
import Protocols.MemoryMap (ConstBwd, MM, constBwd)
import Protocols.Wishbone
import VexRiscv

import Bittide.Calendar
import Bittide.CaptureUgn (captureUgn)
import Bittide.Jtag
import Bittide.MetaPeConfig (metaPeConfig)
import Bittide.ProcessingElement
import Bittide.ScatterGather
import Bittide.SharedTypes
import Bittide.Switch
import Bittide.Wishbone (readDnaPortE2Wb, timeWb, uartBytes, uartInterfaceWb)

{- | Each 'gppe' results in 2 busses for the 'managementUnit', namely:
* The 'calendar' for the 'scatterUnitWB'.
* The 'calendar' for the 'gatherUnitWB'.
-}
type BussesPerGppe = GppeBusses - GppeBussesNotExposed

-- | Configuration of a 'node'.
data NodeConfig linkCount gppes metaPeBufferWidth where
  NodeConfig ::
    forall linkCount gppes metaPeBufferWidth.
    ( KnownNat linkCount
    , KnownNat gppes
    , NmuPrefixWidth linkCount gppes <= 30
    ) =>
    { managementConfig :: ManagementConfig linkCount gppes
    -- ^ Configuration for the 'node's 'managementUnit'.
    , calendarConfig ::
        CalendarConfig (NmuRemBusWidth linkCount gppes) (CalendarEntry (linkCount + gppes + 1))
    -- ^ Configuratoin for the 'node's 'switch'.
    , gppeConfigs :: Vec gppes (GppeConfig (NmuRemBusWidth linkCount gppes) metaPeBufferWidth)
    -- ^ Configuration for all the node's 'gppe's.
    } ->
    NodeConfig linkCount gppes metaPeBufferWidth

-- | A 'node' consists of a 'switch', 'managementUnit' and @0..n@ 'gppe's.
node ::
  forall dom linkCount gppes metaPeBufferWidth.
  ( HiddenClockResetEnable dom
  , 1 <= DomainPeriod dom
  , ?busByteOrder :: ByteOrder
  , ?regByteOrder :: ByteOrder
  ) =>
  NodeConfig linkCount gppes metaPeBufferWidth ->
  Circuit
    ( ConstBwd MM
    , Vec gppes (ConstBwd MM)
    , Jtag dom
    , Vec linkCount (CSignal dom (Maybe (BitVector 64)))
    )
    ( Vec linkCount (CSignal dom (BitVector 64))
    , CSignal dom (Unsigned 64)
    , (ConstBwd MM, NmuWishbone dom linkCount gppes)
    , Vec gppes (CSignal dom (BitVector 64))
    , Vec gppes (CSignal dom (BitVector 64))
    , Vec gppes (Df dom Byte)
    , CSignal dom (Vec (linkCount + gppes + 1) (Index (linkCount + gppes + 2)))
    )
node (NodeConfig muConfig switchConfig gppeConfigs) =
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

    ugnRxs <- zipCircuit (captureUgn localCounter <$> rxs) -< captureUgnsMMWb

    (Fwd peLinksOut, peUartsOut) <-
      unzipC
        <| zipCircuit (gppeC <$> gppeConfigs)
        <| zipC5
        -< (gppeMMs, Fwd switchToGppes, scatterCalsMMWb, gatherCalsMMWb, gppesJtag)

    switchIn <- appendC3 -< ([nmuLinkOut], Fwd peLinksOut, ugnRxs)
    (switchOut, cal) <-
      switchC @_ @_ @_ @_ @64 switchConfig -< (switchMM, (switchIn, switchWb))
    ([nmuLinkIn], Fwd switchToGppes, linksOut) <- split3CI -< switchOut

    idC -< (linksOut, Fwd localCounter, externalMMWb, Fwd switchToGppes, Fwd peLinksOut, peUartsOut, cal)
 where
  zipC5 ::
    forall a b c d e n.
    (KnownNat n) =>
    Circuit (Vec n a, Vec n b, Vec n c, Vec n d, Vec n e) (Vec n (a, b, c, d, e))
  zipC5 = applyC (\(a, b, c, d, e) -> zip5 a b c d e) unzip5

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
    , scatterPrefix :: Unsigned (CLog 2 GppeBusses)
    -- ^ Interconnect prefix for the scatter engine
    , gatherConfig :: GatherConfig 4 nmuRemBusWidth
    -- ^ Configuration for the gather engine
    , gatherPrefix :: Unsigned (CLog 2 GppeBusses)
    -- ^ Interconnect prefix for the gather engine
    , peConfig :: PeConfig GppeBusses
    -- ^ Configuration for a 'gppe's 'processingElement', which statically
    -- has four external busses connected to the instruction memory, data memory
    -- , 'scatterUnitWb' and 'gatherUnitWb'.
    , dnaPrefix :: Unsigned (CLog 2 GppeBusses)
    -- ^ For testing purposes. This should be removed once more than one GPPE is used, or
    -- if access to the device DNA is required elsewhere on the system.
    , uartPrefix :: Unsigned (CLog 2 GppeBusses)
    -- ^ Prefix for the UART device
    , dumpVcd :: DumpVcd
    , metaPeConfigPrefix :: Unsigned (CLog 2 GppeBusses)
    , metaPeConfigBufferWidth :: SNat metaPeBufferWidth
    } ->
    GppeConfig nmuRemBusWidth metaPeBufferWidth

{-# NOINLINE gppeC #-}

{- | A general purpose 'processingElement' to be part of a Bittide Node. It contains
a 'processingElement', 'linkToPe' and 'peToLink' which create the interface for the
Bittide Link. It takes a 'GppeConfig', incoming link and two incoming 'WishboneM2S'
signals and produces the outgoing link alongside two 'WishhboneS2M' signals.
The order of Wishbone busses is as follows:
('scatterUnitWb' :> 'gatherUnitWb' :> Nil).
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
  Circuit
    ( -- \| GPPE memory map
      ConstBwd MM
    , -- \| Incoming link from switch
      CSignal dom (BitVector 64)
    , -- \| Scatter unit calendar memory map and Wishbone bus
      (ConstBwd MM, Wishbone dom 'Standard nmuRemBusWidth (Bytes 4))
    , -- \| Gather unit calendar memory map and Wishbone bus
      (ConstBwd MM, Wishbone dom 'Standard nmuRemBusWidth (Bytes 4))
    , -- \| JTAG connection
      Jtag dom
    )
    (CSignal dom (BitVector 64), Df dom Byte)
gppeC (GppeConfig{..}) =
  circuit $ \(mm, Fwd linkIn, (scatterCalMM, scatterCalWb), (gatherCalMM, gatherCalWb), jtag) -> do
    [ (dnaPfx, dnaBus)
      , (scatterPfx, (scatterMM, scatterWb))
      , (gatherPfx, (gatherMM, gatherWb))
      , (metaPeConfigPfx, metaPeBus)
      , (uartPfx, uartBus)
      ] <-
      processingElement dumpVcd peConfig -< (mm, jtag)
    constBwd dnaPrefix -< dnaPfx
    constBwd scatterPrefix -< scatterPfx
    constBwd gatherPrefix -< gatherPfx
    constBwd metaPeConfigPrefix -< metaPeConfigPfx
    constBwd uartPrefix -< uartPfx
    _dna <- readDnaPortE2Wb simDna2 -< dnaBus
    metaPeConfig metaPeConfigBufferWidth -< metaPeBus
    scatterUnitWbC scatterConfig linkIn
      -< ((scatterMM, scatterWb), (scatterCalMM, scatterCalWb))
    linkOut <-
      gatherUnitWbC gatherConfig -< ((gatherMM, gatherWb), (gatherCalMM, gatherCalWb))
    (gppeUartBytes, _uartStatus) <-
      uartInterfaceWb d16 d16 uartBytes -< (uartBus, Fwd (pure Nothing))
    idC -< (linkOut, gppeUartBytes)

{- Type-level number of node management unit internal busses

Internal busses:
  * Instruction bus
  * Data bus
  * 'whoAmIC'
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
type NmuPrefixWidth linkCount gppes = CLog 2 (NmuBusses linkCount gppes)
type NmuRemBusWidth linkCount gppes = 30 - NmuPrefixWidth linkCount gppes
type NmuWishbone dom linkCount gppes =
  Wishbone dom 'Standard (NmuRemBusWidth linkCount gppes) (Bytes 4)

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
    , scatterCalPrefix :: Unsigned (NmuPrefixWidth linkCount gppes)
    -- ^ Memory map prefix for the scatter unit calendar
    , scatterPrefix :: Unsigned (NmuPrefixWidth linkCount gppes)
    -- ^ Memory map prefix for the scatter unit
    , gatherConfig :: GatherConfig 4 (NmuRemBusWidth linkCount gppes)
    -- ^ Configuration for the management unit gather unit
    , gatherCalPrefix :: Unsigned (NmuPrefixWidth linkCount gppes)
    -- ^ Memory map prefix for the gather unit calendar
    , gatherPrefix :: Unsigned (NmuPrefixWidth linkCount gppes)
    -- ^ Memory map prefix for the gather unit
    , timerPrefix :: Unsigned (NmuPrefixWidth linkCount gppes)
    -- ^ Memory map prefix for the timer component
    , switchPrefix :: Unsigned (NmuPrefixWidth linkCount gppes)
    -- ^ Memory map prefix for the switch.
    , externalPrefix :: Unsigned (NmuPrefixWidth linkCount gppes)
    -- ^ Memory map prefix for an external Wishbone connection
    , captureUgnPrefixes ::
        Vec linkCount (Unsigned (NmuPrefixWidth linkCount gppes))
    -- ^ Memory map prefixes for all of the 'captureUgn' components
    , peScatterGatherPrefixes ::
        Vec
          gppes
          ( Unsigned (NmuPrefixWidth linkCount gppes)
          , Unsigned (NmuPrefixWidth linkCount gppes)
          )
    , peConfig :: PeConfig (NmuBusses linkCount gppes)
    -- ^ Processing element configuration
    , dumpVcd :: DumpVcd
    -- ^ VCD dump configuration
    } ->
    ManagementConfig linkCount gppes

{- | A special purpose 'processingElement' that manages a Bittide Node. It contains
a 'processingElement', 'linkToPe' and 'peToLink' which create the interface for the
Bittide Link. It takes a 'ManagementConfig', incoming link and a vector of incoming
'WishboneS2M' signals and produces the outgoing link alongside a vector of
'WishboneM2S' signals.
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
    (ConstBwd MM, CSignal dom (BitVector 64), Jtag dom)
    ( -- \| Node management unit link output
      CSignal dom (BitVector 64)
    , -- \| Node management unit local counter
      CSignal dom (Unsigned 64)
    , -- \| Switch memory map and Wishbone bus
      (ConstBwd MM, NmuWishbone dom linkCount gppes)
    , -- \| External connection memory map and Wishbone bus
      (ConstBwd MM, NmuWishbone dom linkCount gppes)
    , -- \| 'captureUgn's memory maps and Wishbone busses
      Vec
        linkCount
        (ConstBwd MM, NmuWishbone dom linkCount gppes)
    , -- \| GPPE scatter unit memory maps and Wishbone busses
      Vec
        gppes
        (ConstBwd MM, NmuWishbone dom linkCount gppes)
    , -- \| GPPE gather unit memory maps and Wishbone busses
      Vec
        gppes
        (ConstBwd MM, NmuWishbone dom linkCount gppes)
    )
managementUnitC
  ( ManagementConfig
      { peScatterGatherPrefixes = unzip -> (peScatterPrefixes, peGatherPrefixes)
      , ..
      }
    ) = circuit $ \(mm, Fwd linkIn, jtag) -> do
    peWbs <- processingElement dumpVcd peConfig -< (mm, jtag)
    ( [ (myScatterPfx, (myScatterMM, myScatterWb))
        , (myScatterCalPfx, myScatterCalMMWb)
        , (myGatherPfx, myGatherMMWb)
        , (myGatherCalPfx, myGatherCalMMWb)
        , (switchPfx, switchMMWb)
        , (timePfx, timeMMWb)
        , (externalPfx, externalMMWb)
        ]
      , myWbRest
      ) <-
      splitAtCI -< peWbs

    constBwd scatterPrefix -< myScatterPfx
    constBwd scatterCalPrefix -< myScatterCalPfx
    constBwd gatherPrefix -< myGatherPfx
    constBwd gatherCalPrefix -< myGatherCalPfx
    constBwd switchPrefix -< switchPfx
    constBwd timerPrefix -< timePfx
    constBwd externalPrefix -< externalPfx

    localCounter <- timeWb -< timeMMWb

    scatterUnitWbC scatterConfig linkIn -< ((myScatterMM, myScatterWb), myScatterCalMMWb)
    linkOut <- gatherUnitWbC gatherConfig -< (myGatherMMWb, myGatherCalMMWb)

    (captureUgnPfxMMWbs, scatterPfxMMWbs, gatherPfxMMWbs) <- split3CI -< myWbRest

    (captureUgnPfxs, captureUgnMMWbs) <- unzipC -< captureUgnPfxMMWbs
    idleSink <| zipCircuit (constBwd <$> captureUgnPrefixes) -< captureUgnPfxs

    (scatterPfxs, scatterMMWbs) <- unzipC -< scatterPfxMMWbs
    idleSink <| zipCircuit (constBwd <$> peScatterPrefixes) -< scatterPfxs

    (gatherPfxs, gatherMMWbs) <- unzipC -< gatherPfxMMWbs
    idleSink <| zipCircuit (constBwd <$> peGatherPrefixes) -< gatherPfxs

    idC
      -< ( linkOut
         , localCounter
         , switchMMWb
         , externalMMWb
         , captureUgnMMWbs
         , scatterMMWbs
         , gatherMMWbs
         )
