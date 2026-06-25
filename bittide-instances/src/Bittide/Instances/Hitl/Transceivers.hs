-- SPDX-FileCopyrightText: 2023 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE CPP #-}

{- | Test whether clock boards are configurable and transceiver links come
online. This assumes to run on a fully connected mesh of 8 FPGAs. Also see
'c_CHANNEL_NAMES' and 'c_CLOCK_PATHS'. It has two tricks up its sleeve:

  1. It uses @SYNC_IN@/@SYNC_OUT@ to make sure each board starts programming
     its clock boards at the same time.

  2. It keeps track of how many times the GTH's reset manager had to reset
     the connection and how often it lost connections after establishing
     them.

This test will succeed if all links have been up for ten seconds.
-}
module Bittide.Instances.Hitl.Transceivers where

import Clash.Explicit.Prelude
import Protocols

import Data.Maybe (isJust)
import System.FilePath ((</>))

import Bittide.Arithmetic.Time
import Bittide.BootPe (simpleBootPe, BootPeBusses)
import Bittide.ElasticBuffer (stickyE)
import Bittide.Hitl
import Bittide.Instances.Domains
import Bittide.Instances.Hitl.Setup (FpgaCount, LinkCount, channelNames, clockPaths)
import Bittide.ProcessingElement (PeConfig (..))
import Bittide.SharedTypes (withLittleEndian)

import Clash.Annotations.TH (makeTopEntity)
import Clash.Cores.Xilinx.Xpm.Cdc.Single (xpmCdcSingle)
import Clash.Xilinx.ClockGen
import VexRiscv (JtagIn (..), JtagOut (..))

import qualified Bittide.Cpus.Riscv32imc as Riscv32imc
import qualified Bittide.Transceiver.ResetManager as ResetManager
import qualified Clash.Cores.Xilinx.Gth as Gth
import qualified Data.List as L
import qualified Data.Map as Map
import qualified Protocols.Spi as Spi

#ifdef SIM_BAUD_RATE
type Baud = MaxBaudRate Basic125
import Clash.Cores.UART.Extra
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

peConfig :: PeConfig BootPeBusses
peConfig =
  PeConfig
    { cpu = Riscv32imc.vexRiscv0
    , depthI = SNat @(Div (4 * 1024) 4)
    , depthD = SNat @(Div (16 * 1024) 4)
    , initI = Nothing
    , initD = Nothing
    , iBusTimeout = d0
    , dBusTimeout = d0
    , includeIlaWb = False
    }

{- | Worker function for 'transceiversUpTest'. See module documentation for more
information.
-}
goTransceiversUpTest ::
  "SMA_MGT_REFCLK_C" ::: Clock Ext200 ->
  "SYSCLK" ::: Clock Basic125 ->
  "RST_LOCAL" ::: Reset Basic125 ->
  "GTH_RX_N" ::: Gth.SimWires GthRx LinkCount ->
  "GTH_RX_NS" ::: Gth.Wires GthRxS LinkCount ->
  "GTH_RX_PS" ::: Gth.Wires GthRxS LinkCount ->
  Signal Basic125 Spi.S2M ->
  "JTAG" ::: Signal Basic125 JtagIn ->
  ( "GTH_TX_S" ::: Gth.SimWires GthTx LinkCount
  , "GTH_TX_NS" ::: Gth.Wires GthTxS LinkCount
  , "GTH_TX_PS" ::: Gth.Wires GthTxS LinkCount
  , "allUp" ::: Signal Basic125 Bool
  , "anyErrors" ::: Signal Basic125 Bool
  , "stats" ::: Vec LinkCount (Signal Basic125 ResetManager.Statistics)
  , "spiDone" ::: Signal Basic125 Bool
  , "" ::: Signal Basic125 Spi.M2S
  , "JTAG" ::: Signal Basic125 JtagOut
  , "USB_UART_RXD" ::: Signal Basic125 Bit
  )
goTransceiversUpTest refClk sysClk rst rxs rxns rxps spiS2M jtagIn =
  ( txs
  , txns
  , txps
  , allUp
  , expectCounterErrorSys
  , tOutputs.stats
  , spiDone
  , spiM2S
  , jtagOut
  , uartTx
  )
 where
  allUp =
    fmap and (bundle tOutputs.rxDataInitDonesFree)
      .&&. fmap and (bundle tOutputs.txDataInitDonesFree)

  gths = (refClk, rxs, rxns, rxps, channelNames, clockPaths)

  ( (_memoryMap, jtagOut, (txs, txns, txps), ())
    , (uartTx, spiDone, spiM2S, tOutputs)
    ) =
      toSignals
        (withLittleEndian $ simpleBootPe peConfig baud sysClk rst)
        ( ((), jtagIn, gths, txDatas)
        , ((), (), spiS2M, ())
        )

  -- Transceiver setup
  txCounters = counter tOutputs.txClock . unsafeFromActiveLow <$> tOutputs.txDataInitDones

  txDatas :: Signal GthTx (Vec LinkCount (BitVector 64))
  txDatas = bundle txCounters

  rxDataResets = unsafeFromActiveLow <$> tOutputs.rxDataInitDones

  expectCounterError =
    zipWith3
      expectCounter
      tOutputs.rxClocks
      (zipWith orReset tOutputs.rxResets rxDataResets)
      tOutputs.rxDatas

  expectCounterErrorSys =
    fmap or
      $ bundle
      $ zipWith (`xpmCdcSingle` sysClk) tOutputs.rxClocks expectCounterError

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
  ( "GTH_TX_S" ::: Gth.SimWires GthTx LinkCount
  , "GTH_TX_NS" ::: Gth.Wires GthTxS LinkCount
  , "GTH_TX_PS" ::: Gth.Wires GthTxS LinkCount
  , "SYNC_OUT" ::: Signal Basic125 Bool
  , "" ::: Signal Basic125 Spi.M2S
  , "JTAG" ::: Signal Basic125 JtagOut
  , "USB_UART_RXD" ::: Signal Basic125 Bit
  )
transceiversUpTest refClkDiff sysClkDiff syncIn rxs rxns rxps spiS2M jtagIn =
  (txs, txns, txps, syncOut, spiM2S, jtagOut, uartTx)
 where
  (refClk, _) = Gth.ibufds_gte3 refClkDiff

  (sysClk, sysRst) = clockWizardDifferential sysClkDiff noReset

  testRst = sysRst `orReset` unsafeFromActiveLow startTest `orReset` syncInRst
  syncOut = startTest
  syncInRst =
    resetGlitchFilter (SNat @1024) sysClk
      $ unsafeFromActiveLow
      $ xpmCdcSingle sysClk sysClk syncIn

  (txs, txns, txps, allUp, anyErrors, _stats, _spiDone, spiM2S, jtagOut, uartTx) =
    goTransceiversUpTest refClk sysClk testRst rxs rxns rxps spiS2M jtagIn

  failAfterUp = isFalling sysClk testRst enableGen False allUp
  failAfterUpSticky = stickyE sysClk testRst failAfterUp

  startTest = isJust <$> maybeFpgaIndex

  maybeFpgaIndex :: Signal Basic125 (Maybe (Index FpgaCount))
  maybeFpgaIndex =
    hitlVio
      0
      sysClk
      -- Consider test done if links have been up consistently for 40 seconds. This
      -- is just below the test timeout of 60 seconds, so transceivers have ~20
      -- seconds to come online reliably. This should be plenty.
      (trueFor (SNat @(Seconds 40)) sysClk testRst allUp .||. failAfterUpSticky .||. anyErrors)
      -- Success?
      (fmap not failAfterUpSticky .&&. fmap not anyErrors)

makeTopEntity 'transceiversUpTest

tests :: HitlTestGroup
tests =
  HitlTestGroup
    { topEntity = 'transceiversUpTest
    , externalHdl = []
    , targetXdcs =
        [ "wireDemoTest.xdc"
        , "si539x" </> "spi.xdc"
        ]
    , testCases = iters
    , mDriverProc = Nothing
    , mPostProc = Nothing
    }
 where
  fpgaIndices = [0 ..] :: [Index FpgaCount]
  nIters = 1
  iterNames = ["I" <> show n | n <- [(0 :: Int) .. nIters - 1]]
  iters =
    [ HitlTestCase
        { name = nm
        , parameters =
            Map.fromList (L.zip (HwTargetByIndex . fromIntegral <$> fpgaIndices) fpgaIndices)
        , postProcData = ()
        }
    | nm <- iterNames
    ]
