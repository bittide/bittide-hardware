-- SPDX-FileCopyrightText: 2023 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE CPP #-}

{- | Test whether clock boards are configurable and transceiver links come
online. This assumes to run on a fully connected mesh of 8 FPGAs. Also see
'c_CHANNEL_NAMES' and 'c_CLOCK_PATHS'.

A boot CPU programs the clock board, enables the transceivers and monitors the
links. Each link transmits a counter, which is checked in hardware by
'expectCounter'. The boot CPU reads the results of these checks. Also see
@firmware-binaries/hitl-tests/transceivers-up-test@.

This test will succeed if all links have been up for ten seconds, without any
errors in the received data.
-}
module Bittide.Instances.Hitl.Transceivers where

import Clash.Explicit.Prelude
import Clash.Prelude (withClockResetEnable)
import Protocols

import Bittide.BootPe (BootPeBusses, bootPe)
import Bittide.ElasticBuffer (stickyE)
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
import Clash.Cores.Xilinx.Xpm.Cdc.Single (xpmCdcSingle)
import Clash.Xilinx.ClockGen
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
import qualified Bittide.Instances.Hitl.Driver.Transceivers as Driver
import qualified Bittide.Instances.Hitl.Tef0008 as Tef0008
import qualified Bittide.Transceiver as Transceiver
import qualified Bittide.Transceiver.Wishbone as Transceiver
import qualified Clash.Cores.Xilinx.Gth as Gth
import qualified Protocols.Spi as Spi

#ifdef SIM_BAUD_RATE
type Baud = MaxBaudRate Basic125
#else
type Baud = 921_600
#endif

baud :: SNat Baud
baud = SNat

{- | Start value of the counters used in 'counter' and 'expectCounter'. This is
a non-zero start value, as a regression test for a bug where the transceivers
would not come up if the counters started at zero.
-}
counterStart :: BitVector 64
counterStart = 0xDEAD_BEEF_0000_0000

-- | A counter starting at 'counterStart'
counter ::
  (KnownDomain dom) =>
  Clock dom ->
  Reset dom ->
  Signal dom (BitVector 64)
counter clk rst = let c = register clk rst enableGen counterStart (c + 1) in c

{- | Expect a counter starting at 'counterStart' and incrementing by one on each
cycle. Starts running when the received value equals 'counterStart'.
-}
expectCounter ::
  (KnownDomain dom) =>
  Clock dom ->
  Reset dom ->
  -- | Received data
  Signal dom (BitVector 64) ->
  -- | Error
  Signal dom Bool
expectCounter clk rst = stickyE clk rst . mealy clk rst enableGen go Nothing
 where
  go Nothing e
    | e == counterStart = (Just (e + 1), False)
    | otherwise = (Nothing, False)
  go (Just c) e = (Just (c + 1), c /= e)

-- | Memory mapped component exposing the results of 'expectCounter' to the CPU.
transceiverCheck ::
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
    , CSignal dom (Vec LinkCount Bool)
    )
    ()
transceiverCheck clk rst = circuit $ \(wb, Fwd counterErrors) -> do
  [errorsBus] <- deviceWb clk rst (deviceConfig "TransceiverCheck") -< wb
  registerWb_ clk rst counterErrorsConfig (repeat False)
    -< (errorsBus, Fwd (Just <$> counterErrors))
  idC
 where
  counterErrorsConfig =
    ( registerConfig
        "counter_errors"
        "Per link: whether the received data deviated from the expected counter since the link came up."
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

{- | Boot CPU, transceivers and 'transceiverCheck'. The CPU programs the clock
board over SPI and controls the transceivers.
-}
transceiversUpPe ::
  (HasCallStack, ?byteOrder :: ByteOrder) =>
  Clock Basic125 ->
  Reset Basic125 ->
  Circuit
    ( ToConstBwd Mm
    , Jtag Basic125
    , Gth.Gths GthRx GthRxS GthTx GthTxS Ext200 LinkCount
    )
    ( Spi.Spi Basic125
    , "UART_TX" ::: CSignal Basic125 Bit
    )
transceiversUpPe sysClk sysRst = circuit $ \(mm, jtag, gths) -> do
  (uartBytes, _spiDone, spi, [transceiverBus, checkBus]) <-
    withClockResetEnable sysClk sysRst enableGen
      $ bootPe @2 peConfig
      -< (mm, jtag)

  (_uartRx, uartTx) <-
    withClockResetEnable sysClk sysRst enableGen
      $ uartDf baud
      -< (uartBytes, Fwd (pure 0))

  Fwd tOutputs <-
    Transceiver.transceiverPrbsNWb sysClk sysRst Transceiver.defConfig
      -< (transceiverBus, gths, Fwd (bundle txCounters))

  transceiverCheck sysClk sysRst -< (checkBus, Fwd (bundle counterErrorsSys))

  let
    txCounters :: Vec LinkCount (Signal GthTx (BitVector 64))
    txCounters =
      counter tOutputs.txClock . unsafeFromActiveLow <$> tOutputs.txDataInitDones

    rxDataResets = unsafeFromActiveLow <$> tOutputs.rxDataInitDones

    counterErrors =
      zipWith3
        expectCounter
        tOutputs.rxClocks
        (zipWith orReset tOutputs.rxResets rxDataResets)
        tOutputs.rxDatas

    counterErrorsSys = zipWith (`xpmCdcSingle` sysClk) tOutputs.rxClocks counterErrors

  idC -< (spi, uartTx)

memoryMap :: MemoryMap
memoryMap = mm
 where
  Circuit circuitFn = withLittleEndian $ transceiversUpPe clockGen noReset
  ((SimOnly mm, _, _), _) =
    circuitFn
      ( ((), pure (JtagIn 0 0 0), (clockGen, SimOnly (repeat 0), 0, 0, repeat "", repeat ""))
      , (pure (Spi.S2M low), ())
      )

-- | Top entity for this test. See module documentation for more information.
transceiversUpTest ::
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
transceiversUpTest refClkDiff sysClkDiff _syncIn rxs rxns rxps spiS2M jtagIn _uartRx =
  testStart
    `hwSeqX` (txs, txns, txps, syncOut, spiM2S, jtagOut, uartTx, Tef0008.sfpControl)
 where
  (refClk, _) = Gth.ibufds_gte3 refClkDiff

  (sysClk, sysRst) = clockWizardDifferential sysClkDiff noReset

  -- SYNC_IN and SYNC_OUT are unused, but kept in the top entity because
  -- 'wireDemoTest.xdc' constrains them.
  syncOut = pure False

  -- We don't need the VIO for this test, but our current HITL infrastructure always
  -- tries to generate a probes file, which it can't if there is no VIO.
  testStart = hitlVioBool sysClk testDone testSuccess
  testDone = register sysClk testRst enableGen False (pure True)
  testSuccess = register sysClk testRst enableGen False (pure True)
  testRst = unsafeFromActiveLow testStart

  ((_mm, jtagOut, (txs, txns, txps)), (spiM2S, uartTx)) =
    toSignals
      (withLittleEndian $ transceiversUpPe sysClk sysRst)
      ( ((), jtagIn, (refClk, rxs, rxns, rxps, channelNames, clockPaths))
      , (spiS2M, ())
      )

makeTopEntity 'transceiversUpTest

tests :: HitlTestGroup
tests =
  HitlTestGroup
    { topEntity = 'transceiversUpTest
    , externalHdl = []
    , targetXdcs =
        [ "wireDemoTest.xdc"
        , "jtag" </> "config.xdc"
        , "jtag" </> "pmod1.xdc"
        , "uart" </> "pmod1.xdc"
        , "si539x" </> "spi.xdc"
        , "tef0008" </> "sfpControl.xdc"
        ]
    , testCases =
        [ HitlTestCase
            { name = "TransceiversUpTest"
            , parameters = paramForHwTargets allHwTargets ()
            , postProcData = ()
            }
        ]
    , mDriverProc = Just Driver.driver
    , mPostProc = Nothing
    }
