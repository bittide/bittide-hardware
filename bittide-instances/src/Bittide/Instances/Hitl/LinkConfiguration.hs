-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Tests whether the transceiver link setup matches with the
configuration defined in 'Bittide.Instances.Hitl.Setup.fpgaSetup'.
To this end, each node sends its own index to all of it's
neighbors. A boot CPU sets VADJ, programs the clock chip on the FMC
card, enables the transceivers and waits for the links to be stable. The driver then
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

import Bittide.BootPe (DefSi5345SclPeriod, defFmcBootPeConfig, fmcBootPe)
import Bittide.ClockControl.Si5345I2c (splitSdaPins)
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

import qualified Bittide.Instances.Hitl.Driver.LinkConfiguration as Driver
import qualified Bittide.Instances.Tef0008 as Tef0008
import qualified Bittide.Transceiver as Transceiver
import qualified Bittide.Transceiver.Wishbone as Transceiver
import qualified Clash.Cores.Xilinx.Gth as Gth
import qualified Data.Map.Strict as Map

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

{- | Boot CPU, transceivers and 'linkConfigurationCheck'. The CPU sets VADJ,
programs the clock chip on the FMC card and controls the transceivers. Each transceiver sends
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
    , "MAIN_I2C" ::: CSignal Basic125 I2cIn
    , "FMC_I2C" ::: CSignal Basic125 I2cIn
    )
    ( "MAIN_I2C" ::: CSignal Basic125 I2cOut
    , "FMC_I2C" ::: CSignal Basic125 I2cOut
    , "UART_TX" ::: CSignal Basic125 Bit
    )
linkConfigurationPe sysClk sysRst = circuit $ \(mm, jtag, gths, Fwd myIndex, mainI2cIn, fmcI2cIn) -> do
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

  idC -< (mainI2cOut, fmcI2cOut, uartTx)

memoryMap :: MemoryMap
memoryMap = mm
 where
  Circuit circuitFn = withLittleEndian $ linkConfigurationPe clockGen noReset
  ((SimOnly mm, _, _, _, _, _), _) =
    circuitFn
      (
        ( ()
        , pure (JtagIn 0 0 0)
        , (clockGen, SimOnly (repeat 0), 0, 0, repeat "", repeat "")
        , pure 0
        , pure (high, high)
        , pure (high, high)
        )
      , ((), (), ())
      )

-- | Top entity for this test. See module documentation for more information.
linkConfigurationTest ::
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
linkConfigurationTest refClkDiff sysClkDiff _syncIn rxs rxns rxps fmcSclIn fmcSdaIn mainSclIn mainSdaIn jtagIn _uartRx =
  ( txs
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

  -- The test starts with a valid configuration coming in. The test result is
  -- determined by the driver, so done is only used to signal that the test
  -- started, and success is not used.
  testConfig :: Signal Basic125 (Maybe (Index FpgaCount))
  testConfig = hitlVio 0 sysClk testStart (pure True)

  testStart = isJust <$> testConfig
  testRst = sysRst `orReset` unsafeFromActiveLow testStart
  myIndex = fromMaybe 0 <$> testConfig

  ((_mm, jtagOut, (txs, txns, txps), _, _, _), (mainI2cOut, fmcI2cOut, uartTx)) =
    toSignals
      (withLittleEndian $ linkConfigurationPe sysClk testRst)
      ( ((), jtagIn, (refClk, rxs, rxns, rxps, channelNames, fmcClockPaths), myIndex, mainI2cIn, fmcI2cIn)
      , ((), (), ())
      )

  (mainI2cIn, mainSclOut, mainSdaOut) = openDrainPins mainSclIn mainSdaIn mainI2cOut
  (fmcI2cIn, fmcSclOut, fmcSdaOut) = splitSdaPins fmcSclIn fmcSdaIn fmcI2cOut

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
        , "tef0008" </> "sfpControl.xdc"
        , "tef0008" </> "si5345I2c.xdc"
        , "tef0008" </> "mainI2c.xdc"
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
