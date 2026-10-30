-- SPDX-FileCopyrightText: 2023 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE CPP #-}

{- | Test whether the clock chip on the FMC card is configurable and transceiver
links come online. This assumes to run on a fully connected mesh of 8 FPGAs. Also see
'c_CHANNEL_NAMES' and 'c_CLOCK_PATHS'.

A boot CPU sets VADJ, programs the clock chip, enables the transceivers and monitors the
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

import Bittide.BootPe (DefSi5345SclPeriod, defFmcBootPeConfig, fmcBootPe)
import Bittide.ClockControl.Si5345I2c (splitSdaPins)
import Bittide.ElasticBuffer (stickyE)
import Bittide.Hitl
import Bittide.I2c (I2cIn, I2cOut, openDrainPins)
import Bittide.Instances.Domains
import Bittide.Instances.Hitl.Setup
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

import qualified Bittide.Instances.Hitl.Driver.Transceivers as Driver
import qualified Bittide.Instances.Tef0008 as Tef0008
import qualified Bittide.Transceiver as Transceiver
import qualified Bittide.Transceiver.Wishbone as Transceiver
import qualified Clash.Cores.Xilinx.Gth as Gth

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

{- | Boot CPU, transceivers and 'transceiverCheck'. The CPU sets VADJ, programs
the clock chip on the FMC card and controls the transceivers.
-}
transceiversUpPe ::
  (HasCallStack, ?byteOrder :: ByteOrder) =>
  Clock Basic125 ->
  Reset Basic125 ->
  Circuit
    ( ToConstBwd Mm
    , Jtag Basic125
    , Gth.Gths GthRx GthRxS GthTx GthTxS Ext200 LinkCount
    , "MAIN_I2C" ::: CSignal Basic125 I2cIn
    , "FMC_I2C" ::: CSignal Basic125 I2cIn
    )
    ( "MAIN_I2C" ::: CSignal Basic125 I2cOut
    , "FMC_I2C" ::: CSignal Basic125 I2cOut
    , "UART_TX" ::: CSignal Basic125 Bit
    )
transceiversUpPe sysClk sysRst = circuit $ \(mm, jtag, gths, mainI2cIn, fmcI2cIn) -> do
  (uartBytes, _clockChipDone, mainI2cOut, fmcI2cOut, [transceiverBus, checkBus]) <-
    withClockResetEnable sysClk sysRst enableGen
      $ fmcBootPe (SNat @DefSi5345SclPeriod) defFmcBootPeConfig
      -< (mm, jtag, mainI2cIn, fmcI2cIn)

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

  idC -< (mainI2cOut, fmcI2cOut, uartTx)

memoryMap :: MemoryMap
memoryMap = mm
 where
  Circuit circuitFn = withLittleEndian $ transceiversUpPe clockGen noReset
  ((SimOnly mm, _, _, _, _), _) =
    circuitFn
      (
        ( ()
        , pure (JtagIn 0 0 0)
        , (clockGen, SimOnly (repeat 0), 0, 0, repeat "", repeat "")
        , pure (high, high)
        , pure (high, high)
        )
      , ((), (), ())
      )

-- | Top entity for this test. See module documentation for more information.
transceiversUpTest ::
  -- Driven by the Si5345 on the FMC card:
  "FMC_HPC_GBTCLK1_M2C_C" ::: DiffClock Ext200 ->
  "SYSCLK_125" ::: DiffClock Ext125 ->
  "SYNC_IN" ::: Signal Basic125 Bool ->
  "GTH_RX_S" ::: Gth.SimWires GthRx LinkCount ->
  "GTH_RX_NS" ::: Gth.Wires GthRxS LinkCount ->
  "GTH_RX_PS" ::: Gth.Wires GthRxS LinkCount ->
  -- I2C to the Si5345 on the FMC card:
  "FMC_SCL" ::: BiSignalIn 'PullUp Basic125 1 ->
  "FMC_SDA_IN" ::: Signal Basic125 Bit ->
  -- Main I2C bus of the KCU105, shared with the system controller
  "IIC_SCL_MAIN_LS" ::: BiSignalIn 'PullUp Basic125 1 ->
  "IIC_SDA_MAIN_LS" ::: BiSignalIn 'PullUp Basic125 1 ->
  "JTAG" ::: Signal Basic125 JtagIn ->
  "USB_UART_TXD" ::: Signal Basic125 Bit ->
  ( "GTH_TX_S" ::: Gth.SimWires GthTx LinkCount
  , "GTH_TX_NS" ::: Gth.Wires GthTxS LinkCount
  , "GTH_TX_PS" ::: Gth.Wires GthTxS LinkCount
  , "SYNC_OUT" ::: Signal Basic125 Bool
  , "FMC_SCL_OUT" ::: BiSignalOut 'PullUp Basic125 1
  , "FMC_SDA_OUT" ::: Signal Basic125 Bit
  , "FMC_MUX_SELECT" ::: Signal Basic125 (BitVector 3)
  , "IIC_SCL_MAIN_LS_OUT" ::: BiSignalOut 'PullUp Basic125 1
  , "IIC_SDA_MAIN_LS_OUT" ::: BiSignalOut 'PullUp Basic125 1
  , "IIC_MUX_RESET_B_LS" ::: Signal Basic125 Bit
  , "JTAG" ::: Signal Basic125 JtagOut
  , "USB_UART_RXD" ::: Signal Basic125 Bit
  , "" ::: Tef0008.SfpControl Basic125
  )
transceiversUpTest refClkDiff sysClkDiff _syncIn rxs rxns rxps fmcSclIn fmcSdaIn mainSclIn mainSdaIn jtagIn _uartRx =
  testStart
    `hwSeqX` ( txs
             , txns
             , txps
             , syncOut
             , fmcSclOut
             , fmcSdaOut
             , Tef0008.si5345MuxSelect
             , mainSclOut
             , mainSdaOut
             , Tef0008.iicMuxResetB
             , jtagOut
             , uartTx
             , Tef0008.sfpControl
             )
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

  ((_mm, jtagOut, (txs, txns, txps), _, _), (mainI2cOut, fmcI2cOut, uartTx)) =
    toSignals
      (withLittleEndian $ transceiversUpPe sysClk sysRst)
      ( ((), jtagIn, (refClk, rxs, rxns, rxps, channelNames, fmcClockPaths), mainI2cIn, fmcI2cIn)
      , ((), (), ())
      )

  (mainI2cIn, mainSclOut, mainSdaOut) = openDrainPins mainSclIn mainSdaIn mainI2cOut
  (fmcI2cIn, fmcSclOut, fmcSdaOut) = splitSdaPins fmcSclIn fmcSdaIn fmcI2cOut

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
        , "tef0008" </> "sfpControl.xdc"
        , "tef0008" </> "si5345I2c.xdc"
        , "tef0008" </> "mainI2c.xdc"
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
