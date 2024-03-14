{-# OPTIONS -fplugin=Protocols.Plugin #-}
module Bittide.Instances.Pnr.Ethernet where

import Clash.Explicit.Prelude
import Clash.Prelude (withClockResetEnable)
import Clash.Annotations.TH
import Clash.Xilinx.ClockGen
import Language.Haskell.TH
import Protocols
import Protocols.Axi4.Stream
import System.FilePath

import Bittide.DoubleBufferedRam
import Bittide.Ethernet.Gmii
import Bittide.Ethernet.Mac
import Bittide.Instances.Domains
import Bittide.ProcessingElement
import Bittide.ProcessingElement.Util
import Bittide.SharedTypes
import Bittide.Wishbone
import Project.FilePath
import Data.Proxy
import Bittide.Axi4
import qualified Protocols.DfConv as DfConv

-- | A simple instance containing just VexRisc and UART as peripheral.
-- Runs the `hello` binary from `firmware-binaries`.
vexRiscGmii ::
  "SYSCLK_300" ::: DiffClock Ext300 ->
  "CPU_RESET" ::: Reset Basic200 ->
  "rxClk" ::: Clock Basic125A ->
  "rxRst" ::: Reset Basic125A ->
  "txClk" ::: Clock Basic125B ->
  "txRst" ::: Reset Basic125B ->
    ( "USB_UART_TX" ::: Signal Basic200 Bit
    , "gmiiRx" ::: Signal Basic125A GmiiRx
    ) ->
    ( "USB_UART_RX" ::: Signal Basic200 Bit
    , "gmiiTx" ::: Signal Basic125B GmiiTx
    )
vexRiscGmii diffClk cpuRst rxClk rxRst txClk txRst fwd = snd $ toSignals (
  circuit $ \(uartTx, gmiiRx) -> do
    [uartBus, timeBus, wbAxiRx, wbAxiTx] <- pe -< ()
    (uartRx, _uartStatus) <- uart -< (uartBus, uartTx)
    time -< timeBus

    (axiRx0, gmiiTx, _ethStatus) <- mac -< (axiTx1, gmiiRx)
    axiRx1 <- axiRxPipe -< axiRx0
    axiTx0 <- wbToAxiTx' -< wbAxiTx
    axiTx1 <- axiTxPipe -< axiTx0
    _rxBufStatus <- wbAxiRxBuffer -< (wbAxiRx, axiRx1)

    idC -< (uartRx, gmiiTx)
  ) (fwd, (pure (), pure ()))

 where
  time = withClockResetEnable sysClk sysRst enableGen timeWb
  mac = ethMac1GFifoC d32 d32 sysClk sysRst txClk txRst rxClk rxRst miiSel txClkEna rxClkEna
  uart = withClockResetEnable sysClk sysRst enableGen uartWb d16 d16 (SNat @921600)
  pe = withClockResetEnable sysClk sysRst enableGen processingElement peConfig
  wbToAxiTx' = withClockResetEnable sysClk sysRst enableGen wbToAxiTx
  wbAxiRxBuffer = withClockResetEnable sysClk sysRst enableGen wbAxisRxBufferCircuit (SNat @512)
  axiTxPipe = withClockResetEnable sysClk sysRst enableGen (addUser <| axiStreamPacketFifo (SNat @2048) <| axisToByteStream)
  axiRxPipe = withClockResetEnable sysClk sysRst enableGen (axisFromByteStream <| dropUser)

  addUser = withClockResetEnable sysClk sysRst enableGen DfConv.map proxyB proxyA (\axi -> axi{_tuser = False})
  proxyA = Proxy @(Axi4Stream Basic200 ('Axi4StreamConfig 1 0 0) Bool)
  proxyB = Proxy @(Axi4Stream Basic200 ('Axi4StreamConfig 1 0 0) ())
  dropUser = withClockResetEnable sysClk sysRst enableGen DfConv.map proxyA proxyB (\axi -> axi{_tuser = ()})

  miiSel = pure False
  rxClkEna = pure True
  txClkEna = pure True

  (sysClk, pllLock) = clockWizardDifferential diffClk noReset
  sysRst = pllLock `orReset` cpuRst

  ( (_iStart, _iSize, iMem)
    , (_dStart, _dSize, dMem)) = $(do
      root <- runIO $ findParentContaining "cabal.project"
      let
        elfDir = root </> firmwareBinariesDir "riscv32imc-unknown-none-elf" True
        elfPath = elfDir </> "hello"
      memBlobsFromElf BigEndian elfPath Nothing)

  peConfig =
    PeConfig (indices d6)
    (Reloadable $ Blob iMem)
    (Reloadable $ Blob dMem)

makeTopEntity 'vexRiscGmii
