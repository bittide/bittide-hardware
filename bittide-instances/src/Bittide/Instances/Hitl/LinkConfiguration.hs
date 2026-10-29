-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Tests whether the transceiver link setup matches with the
configuration defined in 'Bittide.Instances.Hitl.Setup.fpgaSetup'.
To this end, each node sends its own index to all of it's
neighbors. A boot CPU programs the clock board, enables the
transceivers and waits for the links to be stable. The driver then
reads the received indices and verifies that they match with the
definition. Also see @firmware-binaries/hitl-tests/link-configuration-test@.
-}
module Bittide.Instances.Hitl.LinkConfiguration (
  linkConfigurationTest,
  memoryMap,
  tests,
) where

import Clash.Explicit.Prelude
import Clash.Prelude (withClockResetEnable)
import Protocols

import Bittide.BootPe (BootPeBusses, bootPe)
import Bittide.ClockControl.Si539xSpi (si539xSpiWb)
import Bittide.Hitl
import Bittide.Instances.Domains
import Bittide.Instances.Hitl.Setup
import Bittide.ProcessingElement (PeConfig (..))
import Bittide.SharedTypes (BitboneMm, withLittleEndian)
import Bittide.Transceiver (COutputs (..))
import Bittide.Wishbone (uartDf)

import Clash.Annotations.TH (makeTopEntity)
import Clash.Class.BitPackC (ByteOrder)
#ifdef SIM_BAUD_RATE
import Clash.Cores.Uart.Extra (MaxBaudRate)
#endif
import Clash.Cores.Xilinx.Xpm.Cdc.Handshake.Extra
import Clash.Xilinx.ClockGen
import Data.Maybe (fromMaybe, isJust)
import GHC.Stack (HasCallStack)
import Protocols.MemoryMap (Access (ReadOnly), MemoryMap, Mm)
import Protocols.MemoryMap.Registers.WishboneStandard (
  RegisterConfig (access),
  deviceConfig,
  deviceWb,
  registerConfig,
  registerWb_,
 )
import System.FilePath ((</>))
import VexRiscv (Jtag, JtagIn (..), JtagOut)

import qualified Bittide.Cpus.Riscv32imc as Riscv32imc
import qualified Bittide.Instances.Hitl.Driver.LinkConfiguration as Driver
import qualified Bittide.Instances.Tef0008 as Tef0008
import qualified Bittide.Transceiver as Transceiver
import qualified Bittide.Transceiver.Wishbone as Transceiver
import qualified Clash.Cores.Xilinx.Gth as Gth
import qualified Data.Map.Strict as Map
import qualified Protocols.Spi as Spi

#ifdef SIM_BAUD_RATE
type Baud = MaxBaudRate Basic125
#else
type Baud = 921_600
#endif

baud :: SNat Baud
baud = SNat

{- | Synchronizes the fixed FPGA index from some given source domain
to the given target domain. Lossy synchronization is sufficient
here, as the index is considered to be stable stable once the test
has been started.
-}
xpmCdcStable ::
  ( KnownDomain src
  , KnownDomain dst
  , BitPack a
  , NFDataX a
  , 1 <= BitSize a
  , BitSize a <= 1024
  ) =>
  Clock src ->
  Signal src a ->
  Clock dst ->
  Reset dst ->
  Signal dst (Maybe a)
xpmCdcStable srcClk idx dstClk dstRst = mIdx
 where
  mIdx =
    register dstClk dstRst enableGen Nothing
      $ (<|>)
      <$> xpmCdcMaybeLossy srcClk dstClk (pure <$> idx)
      <*> mIdx

{- | Synchronizes the data received on a link to the given target domain. Holds
the last word received after the link came up, or 'Nothing' if no such word has
been received yet.
-}
receivedData ::
  (KnownDomain src, KnownDomain dst) =>
  Clock dst ->
  Reset dst ->
  Clock src ->
  -- | Receive data initialization done
  Signal src Bool ->
  Signal src (BitVector 64) ->
  Signal dst (Maybe (BitVector 64))
receivedData dstClk dstRst srcClk rxReady rxData = received
 where
  received =
    register dstClk dstRst enableGen Nothing
      $ (<|>)
      <$> xpmCdcMaybeLossy srcClk dstClk (mux rxReady (Just <$> rxData) (pure Nothing))
      <*> received

{- | Memory mapped component exposing this FPGA's index and the data received
on each link to the CPU.
-}
linkConfigurationCheck ::
  forall aw dom.
  ( HasCallStack
  , KnownDomain dom
  , HasSynchronousReset dom
  , HasDefinedInitialValues dom
  , KnownNat aw
  , 4 <= aw
  , ?byteOrder :: ByteOrder
  ) =>
  Clock dom ->
  Reset dom ->
  Circuit
    ( BitboneMm dom aw
    , CSignal dom (Index FpgaCount)
    , CSignal dom (Vec LinkCount (Maybe (BitVector 64)))
    )
    ()
linkConfigurationCheck clk rst = circuit $ \(wb, Fwd myIndex, Fwd received) -> do
  [myIndexBus, receivedBus] <-
    deviceWb clk rst (deviceConfig "LinkConfigurationCheck") -< wb
  registerWb_ clk rst myIndexConfig 0 -< (myIndexBus, Fwd (Just <$> myIndex))
  registerWb_ clk rst receivedConfig (repeat Nothing) -< (receivedBus, Fwd (Just <$> received))
  idC
 where
  myIndexConfig =
    (registerConfig "my_index" "Index of this FPGA, as set by the HITL test.")
      { access = ReadOnly
      }
  receivedConfig =
    ( registerConfig
        "received_indices"
        "Per link: last word received since the link came up. Should be the index of the neighbor."
    )
      { access = ReadOnly
      }

-- | Configuration of the boot CPU. The test firmware does not fit in 4 KiB.
peConfig :: PeConfig (BootPeBusses + 2)
peConfig =
  PeConfig
    { cpu = Riscv32imc.vexRiscv0
    , depthI = SNat @(Div (8 * 1024) 4)
    , depthD = SNat @(Div (16 * 1024) 4)
    , initI = Nothing
    , initD = Nothing
    , iBusTimeout = d0
    , dBusTimeout = d0
    , includeIlaWb = False
    }

{- | Boot CPU, transceivers and 'linkConfigurationCheck'. The CPU programs the
clock board over SPI and controls the transceivers. Each transceiver sends
this FPGA's index.
-}
linkConfigurationPe ::
  (HasCallStack, ?byteOrder :: ByteOrder) =>
  Clock Basic125 ->
  Reset Basic125 ->
  Circuit
    ( ToConstBwd Mm
    , Jtag Basic125
    , Gth.Gths GthRx GthRxS GthTx GthTxS Ext200 LinkCount
    , "MY_INDEX" ::: CSignal Basic125 (Index FpgaCount)
    )
    ( Spi.Spi Basic125
    , "UART_TX" ::: CSignal Basic125 Bit
    )
linkConfigurationPe sysClk sysRst = circuit $ \(mm, jtag, gths, Fwd myIndex) -> do
  (uartBytes, siBus, [transceiverBus, checkBus]) <-
    withClockResetEnable sysClk sysRst enableGen
      $ bootPe @2 peConfig
      -< (mm, jtag)

  (_spiDone, spi) <-
    withClockResetEnable sysClk sysRst enableGen
      $ si539xSpiWb (SNat @(Microseconds 10))
      -< siBus

  (_uartRx, uartTx) <-
    withClockResetEnable sysClk sysRst enableGen
      $ uartDf baud
      -< (uartBytes, Fwd (pure 0))

  Fwd tOutputs <-
    Transceiver.transceiverPrbsNWb sysClk sysRst Transceiver.defConfig
      -< (transceiverBus, gths, Fwd (pure <$> myIndexTx))

  linkConfigurationCheck sysClk sysRst
    -< (checkBus, Fwd myIndex, Fwd (bundle received))

  let
    -- synchronizes the FPGA's stable index to the TX clock domain of the
    -- transceivers
    myIndexTx =
      fmap
        (zeroExtend . pack . fromMaybe 0)
        (xpmCdcStable sysClk myIndex tOutputs.txClock tOutputs.txReset)

    received =
      zipWith3
        (receivedData sysClk sysRst)
        tOutputs.rxClocks
        tOutputs.rxDataInitDones
        tOutputs.rxDatas

  idC -< (spi, uartTx)

memoryMap :: MemoryMap
memoryMap = mm
 where
  Circuit circuitFn = withLittleEndian $ linkConfigurationPe clockGen noReset
  ((SimOnly mm, _, _, _), _) =
    circuitFn
      (
        ( ()
        , pure (JtagIn 0 0 0)
        , (clockGen, SimOnly (repeat 0), 0, 0, repeat "", repeat "")
        , pure 0
        )
      , (pure (Spi.S2M low), ())
      )

-- | Top entity for this test. See module documentation for more information.
linkConfigurationTest ::
  "SMA_MGT_REFCLK_C" ::: DiffClock Ext200 ->
  "SYSCLK_125" ::: DiffClock Ext125 ->
  "SYNC_IN" ::: Signal Basic125 Bool ->
  "GTH_RX_S" ::: Gth.SimWires GthRx LinkCount ->
  "GTH_RX_NS" ::: Gth.Wires GthRxS LinkCount ->
  "GTH_RX_PS" ::: Gth.Wires GthRxS LinkCount ->
  Signal Basic125 Spi.S2M ->
  "JTAG" ::: Signal Basic125 JtagIn ->
  "USB_UART_TXD" ::: Signal Basic125 Bit ->
  ( "GTH_TX_S" ::: Gth.SimWires GthTx LinkCount
  , "GTH_TX_NS" ::: Gth.Wires GthTxS LinkCount
  , "GTH_TX_PS" ::: Gth.Wires GthTxS LinkCount
  , "SYNC_OUT" ::: Signal Basic125 Bool
  , "" ::: Signal Basic125 Spi.M2S
  , "JTAG" ::: Signal Basic125 JtagOut
  , "USB_UART_RXD" ::: Signal Basic125 Bit
  , "" ::: Tef0008.SfpControl Basic125
  )
linkConfigurationTest refClkDiff sysClkDiff _syncIn rxs rxns rxps spiS2M jtagIn _uartRx =
  (txs, txns, txps, syncOut, spiM2S, jtagOut, uartTx, Tef0008.sfpControl)
 where
  (refClk, _) = Gth.ibufds_gte3 refClkDiff
  (sysClk, sysRst) = clockWizardDifferential sysClkDiff noReset

  -- SYNC_IN and SYNC_OUT are unused, but kept in the top entity because
  -- 'wireDemoTest.xdc' constrains them.
  syncOut = pure False

  -- The test starts with a valid configuration coming in. The test result is
  -- determined by the driver, so done is only used to signal that the test
  -- started, and success is not used.
  testConfig :: Signal Basic125 (Maybe (Index FpgaCount))
  testConfig = hitlVio 0 sysClk testStart (pure True)

  testStart = isJust <$> testConfig
  testRst = sysRst `orReset` unsafeFromActiveLow testStart
  myIndex = fromMaybe 0 <$> testConfig

  ((_mm, jtagOut, (txs, txns, txps), _), (spiM2S, uartTx)) =
    toSignals
      (withLittleEndian $ linkConfigurationPe sysClk testRst)
      ( ((), jtagIn, (refClk, rxs, rxns, rxps, channelNames, clockPaths), myIndex)
      , (spiS2M, ())
      )

makeTopEntity 'linkConfigurationTest

tests :: HitlTestGroup
tests =
  HitlTestGroup
    { topEntity = 'linkConfigurationTest
    , targetXdcs =
        [ "wireDemoTest.xdc"
        , "jtag" </> "config.xdc"
        , "jtag" </> "pmod1.xdc"
        , "uart" </> "pmod1.xdc"
        , "si539x" </> "spi.xdc"
        , "tef0008" </> "sfpControl.xdc"
        ]
    , externalHdl = []
    , testCases =
        [ HitlTestCase
            { name = "LinkConfiguration"
            , parameters =
                Map.fromList
                  [ (HwTargetByIndex (fromIntegral i), i)
                  | i <- [0 ..] :: [Index FpgaCount]
                  ]
            , postProcData = ()
            }
        ]
    , mDriverProc = Just (Driver.driver memoryMap)
    , mPostProc = Nothing
    }
