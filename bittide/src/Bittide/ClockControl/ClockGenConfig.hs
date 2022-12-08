-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# OPTIONS_GHC -fconstraint-solver-iterations=7 #-}

{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}

module Bittide.ClockControl.ClockGenConfig where
import Clash.Prelude
import Bittide.SharedTypes
import Clash.Cores.SPI
import Data.Maybe
import Clash.Debug (traceShowId)
import Debug.Trace

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
  { regPage     :: Page
   -- ^ Page at which to perform the read or write.
  , regAddress  :: Address
   -- ^ Address at which to perform the read or write
  , regWrite    :: Maybe Byte
   -- ^ @Nothing@ for a read operation, @Just byte@ to write @byte@ to this page and address.
  } deriving (Show, Generic, NFDataX)

-- | Contains the configuration for an Si539x chip, explicitly differentiates between
-- the configuration preamble, configuration and configuration postamble.
data ClockGenRegisterMap preambleEntries configEntries postambleEntries = ClockGenRegisterMap
  { configPreamble :: Vec preambleEntries RegisterEntry
  -- ^ Configuration preamble
  , config :: Vec configEntries RegisterEntry
  -- ^ Configuration
  , configPostamble :: Vec postambleEntries RegisterEntry
  -- ^ Configuration postamble
  }

-- | Operations supported by the Si539x chip, @BurstWrite@ is omitted because the
-- current SPI core does not support writing a variable number of bytes while slave select
-- is low.
data SpiCommand
  = SetAddress Address
  -- ^ Sets the selected register 'Address' on the Si539x chip.
  | WriteData Byte
  -- ^ Writes data to the selected 'Address' on the selected 'Page'.
  | ReadData
  -- ^ Reads data from the selected 'Address' on the selected 'Page'.
  | WriteDataInc Byte
  -- ^ Writes data to the selected 'Address' on the selected 'Page' and increments the address.
  | ReadDataInc
  -- ^ Reads data from the selected 'Address' on the selected 'Page' and increments the address.
  deriving Eq

-- | Converts an 'SpiCommand' to the corresponding bytes to be sent over Spi.
spiCommandToBytes :: SpiCommand -> Bytes 2
spiCommandToBytes = \case
  SetAddress bv   -> pack (0b0000_0000 :: Byte, bv)
  WriteData bv    -> pack (0b0100_0000 :: Byte, bv)
  ReadData        -> pack (0b1000_0000 :: Byte, 0 :: Byte)
  WriteDataInc bv -> pack (0b0110_0000 :: Byte, bv)
  ReadDataInc     -> pack (0b1000_0000 :: Byte, 0 :: Byte)
  -- BurstWrite bv   -> pack (0b1110_0000 :: Byte, bv) BurstWrite is not supported by the current spi core.

-- | State of the configuration circuit in 'si539xSpi'.
data ConfigState entries waitCycles
  = FetchReg (Index entries)
  -- ^ Fetches the 'RegisterEntry' at the 'Index' to be written to the @Si539x@ chip.
  | WriteEntry (Index entries)
  -- ^ Writes the 'RegisterEntry' at the 'Index' to the @Si539x@ chip.
  | ReadEntry (Index entries)
  -- ^ Checks if the 'RegisterEntry' at the 'Index' was correctly written to the @Si539x@ chip.
  | Error (Index entries)
  -- ^ The 'RegisterEntry' at the 'Index' was not correctly written to the @Si539x@ chip.
  | Finished
  -- ^ All entries in the 'ClockGenRegisterMap' were correctly written to the @Si539x@ chip.
  | Wait (Index waitCycles) (Index entries)
  -- ^ Waits for the Si539X to be calibrated after writing the configuration preamble from 'ClockGenRegisterMap'.
  deriving (Show, Generic, NFDataX)

-- | Utility function to retrieve the entry 'Index' from the 'ConfigState'.
getStateAddress :: ConfigState entries waitCycles-> Index entries
getStateAddress = \case
  FetchReg i -> i
  WriteEntry i -> i
  ReadEntry i -> i
  Error i -> i
  Finished -> deepErrorX "getStateAddress: ConfigState Finished does not contain an index"
  Wait _ i -> i

-- | Number of clock cycles required at the clock frequency of @dom@ before a minimum @period@ has passed.
-- Is always at least one.
type PeriodCycles dom period = Max 1 (DivRU period (Max 1 (DomainPeriod dom)))

-- | Number of clock cycles at the clock frequency of @dom@ per half period of @period@ to
-- reach a minimum period of @period. Will always be at least one, so the resulting period
-- is always at least twice the period of @dom@.
type HalfPeriodCycles dom period = Max 1 (DivRU period (Max 1 (2 * DomainPeriod dom)))

-- | Spi interface for a @Si539x@ clock generator chip with an initial configuration.
-- This component will first write and verify the initial configuration before becoming
-- available for external circuitry. For an interface that does not initially configure the
-- chip, see 'si539xDriver'.
si539xSpi ::
  forall dom preambleEntries configEntries postambleEntries minTargetPeriodPs .
  ( HiddenClockResetEnable dom, KnownNat (DomainPeriod dom)
  , KnownNat preambleEntries, 1 <= preambleEntries
  , KnownNat configEntries
  , KnownNat postambleEntries
  , 1 <= (preambleEntries + configEntries + postambleEntries)) =>
  -- | Initial configuration for the @Si539x@ chip.
  ClockGenRegisterMap preambleEntries configEntries postambleEntries ->
  -- | Minimum period of the spi clock frequency for the spi clock divider.
  SNat minTargetPeriodPs ->
  -- | Read or write operation for the @Si539X@ registers.
  Signal dom (Maybe RegisterOperation) ->
  -- | MISO
  Signal dom Bit ->
  -- |
  -- 1. Byte returned by read / write operation.
  -- 2. The spi interface is 'Busy' and does not accept new operations.
  -- 3. Outgoing SPI signals: (SCK, MOSI, SS)
  ( Signal dom (Maybe Byte)
  , Signal dom Busy
  , ( Signal dom Bool -- SCK
    , Signal dom Bit  -- MOSI
    , Signal dom Bool -- SS
    )
  )
si539xSpi ClockGenRegisterMap{..} minTargetPs@SNat externalOperation miso =
  (configByte, configBusy, spiOut)
 where
  (driverByte, driverBusy, spiOut) = si539xSpiDriver minTargetPs spiOperation miso
  romOut = rom (configPreamble ++ config ++ configPostamble) $ bitCoerce <$> readCounter

  readCounter :: Signal dom (Index (preambleEntries + configEntries + postambleEntries))
  (readCounter, spiOperation, configBusy, configByte) =
    mealyB go (FetchReg 0) (romOut, externalOperation, driverByte, driverBusy)

  go currentState ((regPage,regAddress,byte), extSpi, spiByte, spiBusy) =
    (nextState, (getStateAddress currentState, spiOp, busy, returnedByte))

   where
    nextState = case (currentState, spiByte) of
      (WriteEntry i, Just _)            -> ReadEntry i
      (ReadEntry i, Just b)
        | b == byte, i == natToNum @(preambleEntries - 1) -> Wait (0 :: Index (PeriodCycles dom (300*10^9))) i
        | b == byte -> FetchReg (succ i) --Error i
        | otherwise -> FetchReg (succ i) --Error i
      (FetchReg i, _)                   -> WriteEntry i
      (Wait ((==maxBound) -> True) i, _)-> FetchReg (succ i)
      (Wait j i, _)                     -> Wait (succ j) i
      (WriteEntry _, Nothing)           -> currentState
      (ReadEntry _, Nothing)            -> currentState
      (Finished , _)                    -> currentState
      (Error _ , _)                     -> currentState

    spiOp = case currentState of
      FetchReg _   -> Nothing
      WriteEntry _ -> Just RegisterOperation{regPage, regAddress, regWrite = Just byte}
      ReadEntry _  -> Just RegisterOperation{regPage, regAddress, regWrite = Nothing}
      Wait _ _     -> Nothing
      Finished     -> extSpi
      Error _      -> extSpi

    (busy, returnedByte) = case currentState of
      Finished -> (spiBusy, spiByte)
      Error _  -> (spiBusy, spiByte)
      _        -> (True, Nothing)

-- | Keeps track of the current 'Page' and 'Address' of the @Si539x@ chip.
-- Furthermore is used to track the current communication cycle.
data DriverState dom = DriverState
  { currentPage         :: Maybe Page
  -- ^ Current 'Page' of the @Si539x@ chip.
  , currentAddress      :: Maybe Address
  -- ^ Current 'Address' of the @Si539x@ chip.
  , currentOp           :: Maybe RegisterOperation
  -- ^ Current communication transaction.
  , commandAcknowledged :: Acknowledge
  -- ^ Whether or not the current transaction has already been acknowledged.
  , idleCycles          :: Index (PeriodCycles dom 95000)
  -- ^ After communication, slave select must be high for at least 95ns.
  }
  deriving (Generic, NFDataX)

-- | Circuitry that controls an spi core based on a state machine that ensures communication
-- transactions with an @Si539x@ chip are executed correctly. It makes sure communication
-- operations target the right register and communication operations are spaced correctly.
si539xSpiDriver ::
  forall dom minTargetPeriodPs .
  (HiddenClockResetEnable dom, KnownNat (DomainPeriod dom)) =>
  -- | Minimum period of the spi clock frequency for the spi clock divider.
  SNat minTargetPeriodPs ->
  -- | Read or write operation for the @Si539X@ registers.
  Signal dom (Maybe RegisterOperation) ->
  -- | MISO
  Signal dom Bit ->
  -- |
  -- 1. Byte returned by read / write operation.
  -- 2. The spi interface is 'Busy' and does not accept new operations.
  -- 3. Outgoing SPI signals: (SCK, MOSI, SS)
  ( Signal dom (Maybe Byte)
  , Signal dom Busy
  , ( Signal dom Bool -- SCK
    , Signal dom Bit  -- MOSI
    , Signal dom Bool -- SS
    )
  )
si539xSpiDriver SNat incomingOpS miso = (fromSlave, decoderBusy, spiOut)
 where
  spiOut = (sck, mosi, ss)
  (sck, mosi, ss, spiBusyS, acknowledge, receivedData) =
    spiMaster SPIMode0 (SNat @(HalfPeriodCycles dom minTargetPeriodPs)) d1 spiWrite miso
  (spiWrite, decoderBusy, fromSlave) = mealyB go (DriverState Nothing Nothing Nothing False 0) (incomingOpS, spiBusyS, acknowledge, receivedData)

  go ::
    DriverState dom ->
    (Maybe RegisterOperation , Busy, Acknowledge, Maybe (Bytes 2)) ->
    (DriverState dom, (Maybe (Bytes 2), Busy, Maybe Byte))

  go currentState@(currentOp -> Nothing) (incomingOp,_,_, _) =
    (currentState{currentOp = incomingOp}, (Nothing, False, Nothing))

  go currentState@DriverState{..} (_, spiBusy, spiAck, receivedBytes) =
   (nextState, (output, True,fmap resize outBytes))
   where
    Just (RegisterOperation{..}) = currentOp
    samePage = currentPage == Just regPage
    sameAddr = currentAddress == Just regAddress

    (spiCommand, nextOp, outBytes) = case (samePage, sameAddr, regWrite) of
      (True , True , Just byte)                             -> (WriteData byte,Nothing, receivedBytes)
      (True , True , Nothing  )                             -> (ReadData,Nothing, receivedBytes)
      (True , False, _        )                             -> (SetAddress regAddress,currentOp, Nothing)
      (False, _    , _        ) | currentAddress == Just 1  -> (WriteData regPage,currentOp, Nothing)
                                | otherwise                 -> (SetAddress 1,currentOp, Nothing)

    (nextPage,nextAddress) = case (currentPage, currentAddress, spiCommand) of
      (_,Just 1, WriteData newPage) -> (Just newPage, currentAddress)
      (_, _, SetAddress newAddr)    -> (currentPage, Just newAddr)
      _                             -> (currentPage, currentAddress)

    updateIdleCycles
      | spiBusy = idleCycles
      | otherwise = satPred SatZero idleCycles

    nextState
      | commandAcknowledged && not spiBusy = DriverState nextPage nextAddress nextOp False maxBound
      | otherwise                          = currentState{commandAcknowledged = spiAck || commandAcknowledged, idleCycles = updateIdleCycles}

    output
      | not commandAcknowledged && idleCycles == 0 = Just $ spiCommandToBytes spiCommand
      | otherwise = Nothing
