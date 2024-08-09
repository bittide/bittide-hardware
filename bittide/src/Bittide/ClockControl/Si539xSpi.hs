{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}
-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# OPTIONS_GHC -fconstraint-solver-iterations=15 #-}

module Bittide.ClockControl.Si539xSpi where

import Clash.Cores.SPI
import Clash.Prelude hiding (PeriodToCycles)

import Data.Maybe

import Bittide.Arithmetic.Time
import Bittide.ClockControl
import Bittide.Extra.Maybe
import Bittide.SharedTypes

import Clash.Cores.Xilinx.DcFifo

-- | The Si539X chips use "Page"s to increase their address space.
type Page = Byte

-- | Different memory location depending on the current 'Page'.
type Address = Byte

-- | Indicates that the interface producing this is currently Busy and will not respond to inputs.
type Busy = Bool

-- | Indicates tgat the interface producing this value has captured the input.
type Acknowledge = Bool

-- | A Si539X register entry consists of a 'Page', and 'Address' and a 'Byte' value.
type RegisterEntry = (Page, Address, Byte)

-- | Used to read from or write to a register on a Si539x chip via SPI.
data RegisterOperation = RegisterOperation
  { regPage :: Page
  -- ^ Page at which to perform the read or write.
  , regAddress :: Address
  -- ^ Address at which to perform the read or write
  , regWrite :: Maybe Byte
  -- ^ @Nothing@ for a read operation, @Just byte@ to write @byte@ to this 'Page' and 'Address'.
  }
  deriving (Show, Generic, NFDataX)

{- | Contains the configuration for an Si539x chip, explicitly differentiates between
the configuration preamble, configuration and configuration postamble.
-}
data Si539xRegisterMap preambleEntries configEntries postambleEntries = Si539xRegisterMap
  { configPreamble :: Vec preambleEntries RegisterEntry
  -- ^ Configuration preamble
  , config :: Vec configEntries RegisterEntry
  -- ^ Configuration
  , configPostamble :: Vec postambleEntries RegisterEntry
  -- ^ Configuration postamble
  }

{- | Operations supported by the Si539x chip, @BurstWrite@ is omitted because the
current SPI core does not support writing a variable number of bytes while slave select
is low.
-}
data SpiCommand
  = -- | Sets the selected register 'Address' on the Si539x chip.
    SetAddress Address
  | -- | Writes data to the selected 'Address' on the selected 'Page'.
    WriteData Byte
  | -- | Reads data from the selected 'Address' on the selected 'Page'.
    ReadData
  | -- | Writes data to the selected 'Address' on the selected 'Page' and increments the 'Address'.
    WriteDataInc Byte
  | -- | Reads data from the selected 'Address' on the selected 'Page' and increments the 'Address'.
    ReadDataInc
  deriving (Eq)

-- | Converts an 'SpiCommand' to the corresponding bytes to be sent over SPI.
spiCommandToBytes :: SpiCommand -> Bytes 2
spiCommandToBytes = \case
  SetAddress bv -> pack (0b0000_0000 :: Byte, bv)
  WriteData bv -> pack (0b0100_0000 :: Byte, bv)
  ReadData -> pack (0b1000_0000 :: Byte, 0 :: Byte)
  WriteDataInc bv -> pack (0b0110_0000 :: Byte, bv)
  ReadDataInc -> pack (0b1000_0000 :: Byte, 0 :: Byte)

-- BurstWrite bv   -> pack (0b1110_0000 :: Byte, bv) BurstWrite is not supported by the current SPI core.

-- | State of the configuration circuit in 'si539xSpi'.
data ConfigState dom entries
  = -- | Continuously read from 'Address' 0xFE at any 'Page', if this operations returns
    -- 0x0F twice in a row, the device is considered to be ready for operation.
    WaitForReady Bool
  | -- | Always after a @WaitForReady False@ state, we reset the SPI driver to make sure
    -- it first sets the page and address again.
    ResetDriver Bool
  | -- | Fetches the 'RegisterEntry' at the 'Index' to be written to the @Si539x@ chip.
    FetchReg (Index entries)
  | -- | Writes the 'RegisterEntry' at the 'Index' to the @Si539x@ chip.
    WriteEntry (Index entries)
  | -- | Checks if the 'RegisterEntry' at the 'Index' was correctly written to the @Si539x@ chip.
    ReadEntry (Index entries)
  | -- | The 'RegisterEntry' at the 'Index' was not correctly written to the @Si539x@ chip.
    Error (Index entries)
  | -- | Continuously read from 'Address' 0x0C at 'Page' 0x00 until it returns bit 3 is 0.
    WaitForLock
  | -- | All entries in the 'Si539xRegisterMap' were correctly written to the @Si539x@ chip.
    Finished
  | -- | Waits for the Si539X to be calibrated after writing the configuration preamble from 'Si539xRegisterMap'.
    Wait (Index (PeriodToCycles dom (Milliseconds 300))) (Index entries)
  deriving (Show, Generic, NFDataX, Eq)

instance
  ( 1 <= entries
  , KnownNat (DomainPeriod dom)
  , KnownNat entries
  ) =>
  BitPack (ConfigState dom entries)

-- | Utility function to retrieve the entry 'Index' from the 'ConfigState'.
getStateAddress :: (KnownNat entries) => ConfigState dom entries -> Index entries
getStateAddress = \case
  WaitForReady _ -> 0
  ResetDriver _ -> 0
  FetchReg i -> i
  WriteEntry i -> i
  ReadEntry i -> i
  Error i -> i
  WaitForLock -> maxBound
  Finished -> maxBound
  Wait _ i -> i

{- | SPI interface for a @Si539x@ clock generator chip with an initial configuration.
This component will first write and verify the initial configuration before becoming
available for external circuitry. For an interface that does not initially configure the
chip, see 'si539xDriver'.
-}
si539xSpi ::
  forall dom preambleEntries configEntries postambleEntries minTargetPeriodPs.
  ( HiddenClockResetEnable dom
  , KnownNat preambleEntries
  , 1 <= preambleEntries
  , KnownNat configEntries
  , KnownNat postambleEntries
  , 1 <= (preambleEntries + configEntries + postambleEntries)
  ) =>
  -- | Initial configuration for the @Si539x@ chip.
  Si539xRegisterMap preambleEntries configEntries postambleEntries ->
  -- | Minimum period of the SPI clock frequency for the SPI clock divider.
  SNat minTargetPeriodPs ->
  -- | Read or write operation for the @Si539X@ registers.
  Signal dom (Maybe RegisterOperation) ->
  -- | MISO
  "MISO" ::: Signal dom Bit ->
  -- |
  -- 1. Byte returned by read / write operation.
  -- 2. The SPI interface is 'Busy' and does not accept new operations.
  -- 3. Outgoing SPI signals: (SCK, MOSI, SS)
  ( Signal dom (Maybe Byte)
  , Signal dom Busy
  , Signal dom (ConfigState dom (preambleEntries + configEntries + postambleEntries))
  , ( "SCK" ::: Signal dom Bool
    , "MOSI" ::: Signal dom Bit
    , "SS" ::: Signal dom Bool
    )
  )
si539xSpi Si539xRegisterMap{..} minTargetPs@SNat externalOperation miso =
  (configByte, configBusy, configState, spiOut)
 where
  (driverByte, driverBusy, spiOut) = withReset driverReset si539xSpiDriver minTargetPs spiOperation miso
  driverReset = forceReset $ holdTrue d3 $ flip fmap configState $ \case
    ResetDriver _ -> True
    _ -> False

  romOut = rom (configPreamble ++ config ++ configPostamble) romAddress
  romAddress = bitCoerce . getStateAddress <$> configState

  (configState, spiOperation, configBusy, configByte) =
    mealyB go (WaitForReady False) (romOut, externalOperation, driverByte, driverBusy)

  go currentState ((regPage, regAddress, byte), extSpi, spiByte, spiBusy) =
    (nextState, (currentState, spiOp, busy, returnedByte))
   where
    isConfigEntry i =
      (natToNum @preambleEntries) <= i && i < (natToNum @(preambleEntries + configEntries))
    nextState = case (currentState, spiByte) of
      (WaitForReady False, Just 0x0F) -> ResetDriver True
      (WaitForReady True, Just 0x0F) -> FetchReg 0
      (WaitForReady _, Just _) -> ResetDriver False
      (ResetDriver b, _) -> WaitForReady b
      (FetchReg i, _) -> WriteEntry i
      (WriteEntry i, Just _)
        | i == maxBound -> WaitForLock
        | i == (natToNum @preambleEntries - 1) -> Wait @dom 0 i
        | isConfigEntry i -> ReadEntry i
        | otherwise -> FetchReg (succ i)
      (ReadEntry i, Just b)
        | b == byte -> FetchReg (succ i)
        | otherwise -> Error i
      (Wait ((== maxBound) -> True) i, _) -> FetchReg (succ i)
      (Wait j i, _) -> Wait (succ j) i
      (WaitForLock, Just 0) -> Finished
      (WaitForReady _, _) -> currentState
      (WaitForLock, _) -> currentState
      (WriteEntry _, _) -> currentState
      (ReadEntry _, _) -> currentState
      (Finished, _) -> currentState
      (Error _, _) -> currentState

    spiOp = case currentState of
      WaitForReady _ -> Just RegisterOperation{regPage = 0x00, regAddress = 0xFE, regWrite = Nothing}
      ResetDriver _ -> Nothing
      FetchReg _ -> Nothing
      WriteEntry _ -> Just RegisterOperation{regPage, regAddress, regWrite = Just byte}
      ReadEntry _ -> Just RegisterOperation{regPage, regAddress, regWrite = Nothing}
      Wait _ _ -> Nothing
      WaitForLock -> Just RegisterOperation{regPage = 0, regAddress = 0xC0, regWrite = Nothing}
      Finished -> extSpi
      Error _ -> extSpi

    (busy, returnedByte) = case currentState of
      Finished -> (spiBusy, spiByte)
      Error _ -> (spiBusy, spiByte)
      _ -> (True, Nothing)

{- | Keeps track of the current 'Page' and 'Address' of the @Si539x@ chip as well as
the current communication cycle.
-}
data DriverState dom = DriverState
  { currentPage :: Maybe Page
  -- ^ Current 'Page' of the @Si539x@ chip.
  , currentAddress :: Maybe Address
  -- ^ Current 'Address' of the @Si539x@ chip.
  , currentOp :: Maybe RegisterOperation
  -- ^ Current communication transaction.
  , commandAcknowledged :: Acknowledge
  -- ^ Whether or not the current transaction has already been acknowledged.
  , storedByte :: Maybe Byte
  -- ^ Data we have received from the SPI interface.
  , idleCycles :: Index (PeriodToCycles dom (Nanoseconds 95))
  -- ^ After communication, slave select must be high for at least 95ns.
  }
  deriving (Generic, NFDataX)

{- | Circuitry that controls an SPI core based on a state machine that ensures communication
transactions with an @Si539x@ chip are executed correctly. It makes sure communication
operations target the right register and communication operations are spaced correctly.
-}
si539xSpiDriver ::
  forall dom minTargetPeriodPs.
  (HiddenClockResetEnable dom) =>
  -- | Minimum period of the SPI clock frequency for the SPI clock divider.
  SNat minTargetPeriodPs ->
  -- | Read or write operation for the @Si539X@ registers.
  Signal dom (Maybe RegisterOperation) ->
  -- | MISO
  "MISO" ::: Signal dom Bit ->
  -- |
  -- 1. Byte returned by read / write operation.
  -- 2. The SPI interface is 'Busy' and does not accept new operations.
  -- 3. Outgoing SPI signals: (SCK, MOSI, SS)
  ( Signal dom (Maybe Byte)
  , Signal dom Busy
  , ( "SCK" ::: Signal dom Bool
    , "MOSI" ::: Signal dom Bit
    , "SS" ::: Signal dom Bool
    )
  )
si539xSpiDriver SNat incomingOpS miso = (fromSlave, decoderBusy, spiOut)
 where
  spiOut = (sck, mosi, ss)
  (sck, mosi, ss, spiBusyS, acknowledge, receivedData) =
    spiMaster
      SPIMode0
      (SNat @(Max 1 (DivRU (PeriodToCycles dom minTargetPeriodPs) 2)))
      d1
      spiWrite
      miso
  (spiWrite, decoderBusy, fromSlave) =
    mealyB go defDriverState (incomingOpS, spiBusyS, acknowledge, receivedData)
  defDriverState =
    DriverState
      { currentPage = Nothing
      , currentAddress = Nothing
      , currentOp = Nothing
      , commandAcknowledged = False
      , storedByte = Nothing
      , idleCycles = maxBound
      }

  go ::
    DriverState dom ->
    (Maybe RegisterOperation, Busy, Acknowledge, Maybe (Bytes 2)) ->
    (DriverState dom, (Maybe (Bytes 2), Busy, Maybe Byte))

  go currentState@(currentOp -> Nothing) (incomingOp, _, _, _) =
    (currentState{currentOp = incomingOp}, (Nothing, False, storedByte currentState))
  go currentState@DriverState{..} (_, spiBusy, spiAck, receivedBytes) =
    (nextState, (output, True, storedByte))
   where
    RegisterOperation{..} = fromJust currentOp
    samePage = currentPage == Just regPage
    sameAddr = currentAddress == Just regAddress

    (spiCommand, nextOp, outBytes) = case (samePage, sameAddr, regWrite) of
      (True, True, Just byte) -> (WriteData byte, Nothing, receivedBytes)
      (True, True, Nothing) -> (ReadData, Nothing, receivedBytes)
      (True, False, _) -> (SetAddress regAddress, currentOp, Nothing)
      (False, _, _)
        | currentAddress == Just 1 -> (WriteData regPage, currentOp, Nothing)
        | otherwise -> (SetAddress 1, currentOp, Nothing)

    (nextPage, nextAddress) = case (currentPage, currentAddress, spiCommand) of
      (_, Just 1, WriteData newPage) -> (Just newPage, currentAddress)
      (_, _, SetAddress newAddr) -> (currentPage, Just newAddr)
      _ -> (currentPage, currentAddress)

    updateIdleCycles
      | spiBusy = idleCycles
      | otherwise = satPred SatZero idleCycles

    nextState
      | commandAcknowledged && not spiBusy && isNothing storedByte =
          DriverState nextPage nextAddress nextOp False (fmap resize outBytes) maxBound
      | otherwise =
          currentState
            { commandAcknowledged = spiAck || commandAcknowledged
            , idleCycles = updateIdleCycles
            , storedByte = fmap resize outBytes
            }

    spiBytes = spiCommandToBytes spiCommand
    output = orNothing (not commandAcknowledged && idleCycles == 0) spiBytes
{-# NOINLINE si539xSpiDriver #-}

-- TODO: Look into replacing dcFifo with XPM_CDC_Handshake.

{- | Consumes 'SpeedChange's produced by a clock control algorithm and produces a
'RegisterOperation' for the 'si539xSpi' core. Consumption rate of 'SpeedUp's and
'SlowDown' depends on the availability of the SPI core. Uses 'dcFifo' with a depth
of 16 elements for clock domain crossing. This is an alternative to controlling the
FINC / FDEC pins directly, the advantages are that we already have to use SPI
to configure the chips, so we require less wiring / IO, and we don´t have to concern
ourselves with the timing requirements for controlling FINC / FDEC directly. The only
downside is that it  is not as instantaneous as controlling the pins.
-}
spiFrequencyController ::
  forall domCallisto domSpi freqIncrementRange freqDecrementRange.
  (KnownDomain domCallisto, KnownDomain domSpi) =>
  -- | The number of times we can increment the frequency from its initial value.
  SNat freqIncrementRange ->
  -- | The number of times we can decrement the frequency from its initial value.
  SNat freqDecrementRange ->
  -- | Callisto domain's clock.
  Clock domCallisto ->
  -- | Callisto domain's reset.
  Reset domCallisto ->
  -- | Callisto domain's enable.
  Enable domCallisto ->
  -- | SPI domain's clock.
  Clock domSpi ->
  -- | SPI domain's reset.
  Reset domSpi ->
  -- | SPI domain's enable.
  Enable domSpi ->
  -- | Requested 'SpeedChange'.
  Signal domCallisto SpeedChange ->
  -- | Incoming 'Busy' signal from the 'si539xSpi' component.
  Signal domSpi Busy ->
  -- | Outgoing 'RegisterOperation'.
  Signal domSpi (Maybe RegisterOperation)
spiFrequencyController
  SNat
  SNat
  clkCallisto
  rstCallisto
  enCallisto
  clkSpi
  rstSpi
  enSpi
  speedChange
  spiBusy = spiOp
   where
    fifoIn =
      mux
        (speedChange .==. pure NoChange .||. not <$> fromEnable enCallisto)
        (pure Nothing)
        (Just <$> speedChange)

    FifoOut{..} =
      dcFifo (defConfig @4) clkCallisto rstCallisto clkSpi rstSpi fifoIn readEnable

    (spiOp, readEnable) =
      withClockResetEnable
        clkSpi
        rstSpi
        enSpi
        mealyB
        go
        initState
        (spiBusy, isEmpty, fifoData)

    initState :: (Bool, Index (1 + freqIncrementRange + freqDecrementRange))
    initState = (False, natToNum @freqIncrementRange)

    go (fifoValid, stepCount) (spiBusyGo, isEmptyGo, fifoDataGo) =
      ((readEnableGo, stepCountNext), (spiOpGo, readEnableGo))
     where
      readEnableGo = not (isEmptyGo || spiBusyGo)
      stepCountNext = case (fifoValid, spiBusyGo, fifoDataGo) of
        (True, False, SpeedUp) -> satSucc SatBound stepCount
        (True, False, SlowDown) -> satPred SatBound stepCount
        _ -> stepCount

      spiOpGo = case (fifoValid, fifoDataGo, stepCount == maxBound, stepCount == minBound) of
        (True, SpeedUp, False, _) ->
          Just RegisterOperation{regPage = 0x00, regAddress = 0x1D, regWrite = Just 1}
        (True, SlowDown, _, False) ->
          Just RegisterOperation{regPage = 0x00, regAddress = 0x1D, regWrite = Just 2}
        _ -> Nothing
{-# NOINLINE spiFrequencyController #-}

{- | When this component receives @True@, it will hold it for @holdCycles@ number of
clock cycles. This implementation does not scale well to large values for @holdCycles@
because it uses 'Vec' internally.
-}
holdTrue ::
  forall dom holdCycles.
  (HiddenClockResetEnable dom, 1 <= holdCycles) =>
  SNat holdCycles ->
  Signal dom Bool ->
  Signal dom Bool
holdTrue SNat = mealy go (repeat False)
 where
  go :: (1 <= holdCycles) => Vec holdCycles Bool -> Bool -> (Vec holdCycles Bool, Bool)
  go state@(Cons _ _) input = (takeI $ input :> state, fold (||) state)
