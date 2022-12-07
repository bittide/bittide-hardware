-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
module Bittide.ClockControl.ClockGenConfig where
import Clash.Prelude
import Bittide.SharedTypes
import Clash.Cores.SPI
import Data.Maybe

type Page = Byte
type Address = Byte
type Busy = Bool

type RegisterEntry = (Page, Address, Byte)
data RegisterOperation = RegisterOperation
  { regPage  :: Page
  , regAddr  :: Address
  , regWrite :: Maybe Byte
  } deriving (Show, Generic, NFDataX)

writeRegEntry :: RegisterEntry -> RegisterOperation
writeRegEntry (regPage, regAddr, Just -> regWrite) = RegisterOperation{regPage, regAddr, regWrite}

data ClockGenRegisterMap preambleEntries configEntries postambleEntries = ClockGenRegisterMap
  { configPreamble :: Vec preambleEntries RegisterEntry
  , config :: Vec configEntries RegisterEntry
  , configPostamble :: Vec postambleEntries RegisterEntry}

data SpiCommand
  = SetAddress Address
  | WriteData Byte
  | ReadData
  | WriteDataInc Byte
  | ReadDataInc

spiCommandToBytes :: SpiCommand -> Bytes 2
spiCommandToBytes = \case
  SetAddress bv   -> pack (0b0000_0000 :: Byte, bv)
  WriteData bv    -> pack (0b0100_0000 :: Byte, bv)
  ReadData        -> pack (0b1000_0000 :: Byte, 0 :: Byte)
  WriteDataInc bv -> pack (0b0110_0000 :: Byte, bv)
  ReadDataInc     -> pack (0b1000_0000 :: Byte, 0 :: Byte)
  -- BurstWrite bv   -> pack (0b1110_0000 :: Byte, bv)

regWriteToSpiBytes :: Maybe Byte -> Bytes 2
regWriteToSpiBytes = \case
  Just byte -> (spiCommandToBytes (WriteData byte))
  Nothing   -> (spiCommandToBytes ReadData)

data ConfigState entries
  = WriteEntry (Index entries)
  | ReadEntry (Index entries)
  | FetchReg (Index entries)
  | Error (Index entries)
  | Finished
  deriving (Show, Generic, NFDataX)

getStateAddress :: ConfigState entries -> Index entries
getStateAddress = \case
  WriteEntry i -> i
  ReadEntry i -> i
  FetchReg i -> i
  Error i -> i
  Finished -> deepErrorX "getStateAddress: ConfigState Finished does not contain an index"

si539xSpi ::
  forall dom preambleEntries configEntries postambleEntries .
  ( HiddenClockResetEnable dom
  , KnownNat preambleEntries
  , KnownNat configEntries
  , KnownNat postambleEntries
  , 1 <= (preambleEntries + configEntries + postambleEntries)) =>
  Maybe (ClockGenRegisterMap preambleEntries configEntries postambleEntries) ->
  Signal dom (Maybe RegisterOperation) ->
  Signal dom Bit ->
  ( Signal dom (Maybe Byte)
  , Signal dom Busy
  , ( Signal dom Bool -- SCK
    , Signal dom Bit  -- MOSI
    , Signal dom Bool -- SS
    )
  )
si539xSpi maybeConfig externalOperation miso = case maybeConfig of
  Just ClockGenRegisterMap{..} -> (configByte, configBusy, spiOut)
   where
    (driverByte, driverBusy, spiOut) = si539xSpiDriver spiOperation miso
    romOut = rom (configPreamble ++ config ++ configPostamble) $ bitCoerce <$> readCounter
    readCounter :: Signal dom (Index (preambleEntries + configEntries + postambleEntries))
    (readCounter, spiOperation, configBusy, configByte) = mealyB go (FetchReg 0) (romOut, externalOperation, driverByte, driverBusy)

    go currentState ((regPage,regAddr,byte), extSpi, spiByte, spiBusy) =
      (nextState, (getStateAddress currentState, spiOp, busy, returnedByte))
     where
      nextState = case (currentState, spiByte) of
        (WriteEntry i, Just _)            -> ReadEntry i
        (ReadEntry i, Just b) | b == byte -> FetchReg (succ i)
                              | otherwise -> FetchReg (succ i) --Error i
        (FetchReg i, _)                   -> WriteEntry i
        (state, _)                        -> state

      spiOp = case currentState of
        (FetchReg _)   -> Nothing
        (WriteEntry _) -> Just RegisterOperation{regPage, regAddr, regWrite = Just byte}
        (ReadEntry _)  -> Just RegisterOperation{regPage, regAddr, regWrite = Nothing}
        _              -> extSpi

      (busy, returnedByte) = case currentState of
        Finished -> (spiBusy, spiByte)
        Error _  -> (spiBusy, spiByte)
        _        -> (True, Nothing)

  Nothing -> si539xSpiDriver externalOperation miso

si539xSpiDriver ::
  (HiddenClockResetEnable dom) =>
  Signal dom (Maybe RegisterOperation) ->
  Signal dom Bit ->
  ( Signal dom (Maybe Byte)
  , Signal dom Busy
  , ( Signal dom Bool -- SCK
    , Signal dom Bit  -- MOSI
    , Signal dom Bool -- SS
    )
  )
si539xSpiDriver incomingEntry miso = (fromSlave, decoderBusy, spiOut)
 where
  spiOut = (sck, mosi, ss)
  fromSlave = mux routeSlaveBytes (fmap resize <$> receivedData) (pure Nothing)
  (sck, mosi, ss, spiBusy, _, receivedData) = spiMaster SPIMode0 d1 d1 spiWrite miso
  (spiWrite, decoderBusy, routeSlaveBytes) = mealyB go (Nothing, Nothing, Nothing) (incomingEntry, spiBusy)

  go ::
    (Maybe Page, Maybe Address, Maybe RegisterOperation) ->
    (Maybe RegisterOperation, Busy ) ->
    ((Maybe Page, Maybe Address, Maybe RegisterOperation), (Maybe (Bytes 2), Busy, Bool))
  go state@(_,_,storedOp) (_,True) = (state, (Nothing, True, isNothing storedOp ))

  go (maybeStoredPage, maybeStoredAddr, Nothing) (incomingOp,False) =
    ((maybeStoredPage, maybeStoredAddr, incomingOp), (Nothing, isJust incomingOp, True))

  go (maybeStoredPage, maybeStoredAddr, currentOp@(Just RegisterOperation{..})) (_, False) =
    (nextState, (output, True, False))
   where
    samePage = maybeStoredPage == Just regPage
    sameAddr = maybeStoredAddr == Just regAddr

    (spiCommand, nextStoredOp) = case (samePage, sameAddr, regWrite) of
      (True , True , Just byte)                             -> (WriteData byte,Nothing)
      (True , True , Nothing  )                             -> (ReadData,Nothing)
      (True , False, _        )                             -> (SetAddress regAddr,currentOp)
      (False, _    , _        ) | maybeStoredAddr == Just 1 -> (WriteData regPage,currentOp)
                                | otherwise                 -> (SetAddress 1,currentOp)

    (nextStoredPage,nextStoredAddr) = case (maybeStoredPage, maybeStoredAddr, spiCommand) of
      (_,Just 1, WriteData newPage) -> (Just newPage, maybeStoredAddr)
      (_, _, SetAddress newAddr)    -> (maybeStoredPage, Just newAddr)
      _                             -> (maybeStoredPage, maybeStoredAddr)

    nextState = (nextStoredPage, nextStoredAddr, nextStoredOp)
    output = Just $ spiCommandToBytes spiCommand
