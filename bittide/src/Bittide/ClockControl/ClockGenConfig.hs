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
  | Finished
  | Error

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
  Just ClockGenRegisterMap{..} -> _
   where
    (spiReadByte, spiBusy, spiOut) = si539xSpiDriver spiOperation miso
    romOut = rom (configPreamble ++ config ++ configPostamble) $ bitCoerce <$> readCounter
    go oldCnt input = (newCnt, output)
     where

  Nothing -> si539xSpiDriver externalOperation miso



 where
  withConfig ClockGenRegisterMap{..} = _
   where
    spiOperation =
      mux (register True (pure False)) (pure Nothing) $
      mux configDone externalOperation (Just . writeRegEntry <$> romOut)

    (spiReadByte, spiBusy, spiOut) = si539xSpiDriver spiOperation miso


    readCounter = register (0 :: Index (preambleEntries + configEntries + postambleEntries))
      $ mux (configDone .||. spiBusy) readCounter (satSucc SatBound <$> readCounter)

    configDone = register False (doneIndicator .||. configDone)
    doneIndicator = readCounter .==. maxBound .&&. not <$> spiBusy


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
