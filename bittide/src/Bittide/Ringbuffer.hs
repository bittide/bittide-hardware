-- SPDX-FileCopyrightText: 2026 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Bittide.Ringbuffer (
  transmitRingbufferWb,
  transmitRingbufferWbC,
  receiveRingbufferWb,
  receiveRingbufferWbC,
) where

import Clash.Prelude

import Bittide.Extra.Maybe
import Bittide.SharedTypes

import Clash.Class.BitPackC
import Protocols
import Protocols.Wishbone

import Data.Constraint.Nat.Lemmas
import Data.Type.Equality ((:~:) (Refl))
import GHC.Stack (HasCallStack)
import Protocols.MemoryMap

{- | Wishbone interface helper for ringbuffers. It makes ringbuffers,
which operate on 64 bit frames, addressable via a 32 bit wishbone bus.
-}
wbInterface ::
  forall nBytes addrW addresses.
  ( KnownNat nBytes
  , KnownNat addresses
  , 1 <= addresses
  , KnownNat addrW
  ) =>
  -- | Wishbone (master -> slave) data.
  WishboneM2S addrW nBytes (Bytes nBytes) ->
  -- | Read data to be sent over the (slave -> master) port.
  Bytes nBytes ->
  -- | (slave -> master data, write address memory element, write data memory element)
  (WishboneS2M (Bytes nBytes), Index addresses, Maybe (Bytes nBytes))
wbInterface WishboneM2S{..} readData =
  ( (emptyWishboneS2M @(Bytes nBytes)){readData, acknowledge, err}
  , wbAddr
  , writeOp
  )
 where
  masterActive = strobe && busCycle
  maxAddress = resize $ pack (maxBound :: Index addresses)
  err = masterActive && (addr > maxAddress)
  acknowledge = masterActive && not err
  wbAddr = unpack . resize $ pack addr
  writeOp = orNothing (strobe && writeEnable && not err) writeData

{-# OPAQUE transmitRingbufferWb #-}

transmitRingbufferWbC ::
  forall dom awTx memDepth.
  ( HasCallStack
  , HiddenClockResetEnable dom
  , KnownNat awTx
  , 1 <= awTx
  , 1 <= memDepth
  , ?busByteOrder :: ByteOrder
  , ?regByteOrder :: ByteOrder
  ) =>
  -- | Configuration for the ringbuffer.
  SNat memDepth ->
  Circuit
    (BitboneMm dom awTx)
    (CSignal dom (BitVector 64))
transmitRingbufferWbC memDepthSnat@SNat = Circuit go
 where
  go (((), wbM2S), _) = ((SimOnly memoryMap, wbS2M), txOut)
   where
    (txOut, wbS2M, _) = transmitRingbufferWb memDepthSnat wbM2S

  memoryMap =
    let
      deviceDef :: DeviceDefinition
      deviceDef =
        DeviceDefinition
          { registers =
              [ NamedLoc
                  { name = Name "transmitBuffer" ""
                  , loc = locHere
                  , value =
                      Register
                        { fieldType = regType @(Vec memDepth (Bytes 8))
                        , address = 0
                        , access = ReadWrite
                        , reset = Nothing
                        , tags = []
                        }
                  }
              ]
          , deviceName =
              Name
                { name = "TransmitRingbuffer"
                , description = ""
                }
          , definitionLoc = locHere
          , tags = []
          }
     in
      MemoryMap
        { tree = DeviceInstance locCaller "TransmitRingbuffer"
        , deviceDefs = deviceSingleton deviceDef
        }

{- | Wishbone addressable 'transmitRingbuffer', the wishbone port can read/write the data
from/to this memory element as if it has a 32 bit port by selecting the upper 32 or lower
32 bits of the data.
-}
transmitRingbufferWb ::
  forall dom awTx memDepth.
  ( HiddenClockResetEnable dom
  , KnownNat awTx
  , 1 <= memDepth
  , ?busByteOrder :: ByteOrder
  , ?regByteOrder :: ByteOrder
  ) =>
  -- | Configuration for the ringbuffer.
  SNat memDepth ->
  -- | Wishbone (master -> slave) port for CPU access.
  Signal dom (WishboneM2S awTx 4 (Bytes 4)) ->
  -- | 1. Transmitted data to link
  --   2. Wishbone (slave -> master) port
  --   3. Memory map
  ( Signal dom (BitVector 64)
  , Signal dom (WishboneS2M (Bytes 4))
  , Mm
  )
transmitRingbufferWb memDepthSnat@SNat wbIn = (txFrame, delayControls wbOut, SimOnly memoryMap)
 where
  (wbOut, memAddr, writeOp) =
    unbundle $ wbInterface <$> wbIn <*> txData
  (readAddr, upperSelected) = unbundle $ div2Index <$> memAddr

  -- Create write operation for 64-bit memory
  ramWriteOp = mkWrite <$> readAddr <*> writeOp <*> upperSelected

  -- Auto-incrementing read counter
  readCounter = register (0 :: Index memDepth) ((+ 1) <$> readCounter)

  -- Block RAM with dual port: CPU writes, counter reads
  txFrame = blockRamU NoClearOnReset memDepthSnat readCounter ramWriteOp

  -- Split 64-bit frame into upper and lower 32-bit words
  (upper, lower) = unbundle $ split <$> txFrame
  selected = register (errorX "transmitRingbufferWb: Initial selection undefined") upperSelected
  txData = mux selected upper lower

  -- Convert 32-bit writes to 64-bit writes
  mkWrite address (Just write) True = Just (address, write ++# 0)
  mkWrite address (Just write) False = Just (address, 0 ++# write)
  mkWrite _ _ _ = Nothing

  memoryMap =
    MemoryMap
      { tree = DeviceInstance locCaller "TransmitRingbuffer"
      , deviceDefs = deviceSingleton deviceDef
      }

  deviceDef :: DeviceDefinition
  deviceDef =
    DeviceDefinition
      { registers =
          [ NamedLoc
              { name = Name "transmitBuffer" ""
              , loc = locHere
              , value =
                  Register
                    { fieldType = regType @(Vec memDepth (Bytes 8))
                    , address = 0
                    , access = ReadWrite
                    , reset = Nothing
                    , tags = []
                    }
              }
          ]
      , deviceName = Name{name = "TransmitRingbuffer", description = ""}
      , definitionLoc = locHere
      , tags = []
      }

{-# OPAQUE receiveRingbufferWb #-}

receiveRingbufferWbC ::
  forall dom awRx memDepth.
  ( HasCallStack
  , HiddenClockResetEnable dom
  , KnownNat awRx
  , 1 <= memDepth
  , ?busByteOrder :: ByteOrder
  , ?regByteOrder :: ByteOrder
  ) =>
  -- | Configuration for the ringbuffer.
  SNat memDepth ->
  Circuit
    ( (BitboneMm dom awRx)
    , CSignal dom (BitVector 64)
    )
    ()
receiveRingbufferWbC memDepth@SNat = Circuit go
 where
  go ((((), wbM2S), linkIn), _) = (((SimOnly memoryMap, wbS2M), ()), ())
   where
    (wbS2M, _) = receiveRingbufferWb memDepth wbM2S linkIn
  memoryMap =
    let
      deviceDef =
        DeviceDefinition
          { registers =
              [ NamedLoc
                  { name = Name "receiveBuffer" ""
                  , loc = locHere
                  , value =
                      Register
                        { fieldType = regType @(Vec memDepth (Bytes 8))
                        , address = 0
                        , access = ReadOnly
                        , reset = Nothing
                        , tags = []
                        }
                  }
              ]
          , deviceName =
              Name
                { name = "ReceiveRingbuffer"
                , description = ""
                }
          , definitionLoc = locHere
          , tags = []
          }
     in
      MemoryMap
        { tree = DeviceInstance locCaller "ReceiveRingbuffer"
        , deviceDefs = deviceSingleton deviceDef
        }

{- | Wishbone addressable 'receiveRingbuffer', the wishbone port can read the data from
this memory element as if it has a 32 bit port by selecting the upper 32 or lower 32
bits of the read data.
-}
receiveRingbufferWb ::
  forall dom awRx memDepth.
  ( HiddenClockResetEnable dom
  , KnownNat awRx
  , 1 <= memDepth
  , ?busByteOrder :: ByteOrder
  , ?regByteOrder :: ByteOrder
  ) =>
  -- | Configuration for the ringbuffer.
  SNat memDepth ->
  -- | Wishbone (master -> slave) port for CPU access.
  Signal dom (WishboneM2S awRx 4 (Bytes 4)) ->
  -- | Incoming frame from Bittide link.
  Signal dom (BitVector 64) ->
  -- | 1. Wishbone (slave -> master) port
  --   2. Memory map
  (Signal dom (WishboneS2M (Bytes 4)), Mm)
receiveRingbufferWb memDepthSnat@SNat wbIn linkIn =
  case clogProductRule @memDepth of
    Refl ->
      (delayControls wbOut, SimOnly memoryMap)
     where
      (wbOut, memAddr, _) =
        unbundle $ wbInterface <$> wbIn <*> rxData
      (readAddr, upperSelected) = unbundle $ div2Index <$> memAddr

      -- Auto-incrementing write counter for incoming frames
      writeCounter = register (0 :: Index memDepth) ((+ 1) <$> writeCounter)

      -- Link writes to auto-incrementing address
      linkWriteOp = curry Just <$> writeCounter <*> linkIn

      -- Block RAM: Link writes, CPU reads
      rxFrame = blockRamU NoClearOnReset memDepthSnat readAddr linkWriteOp

      -- Split 64-bit frame into upper and lower 32-bit words for CPU access
      (upper, lower) = unbundle $ split <$> rxFrame
      selected = register (errorX "receiveRingbufferWb: Initial selection undefined") upperSelected
      rxData = mux selected upper lower

      memoryMap =
        MemoryMap
          { tree = DeviceInstance locCaller "ReceiveRingbuffer"
          , deviceDefs = deviceSingleton deviceDef
          }

      deviceDef :: DeviceDefinition
      deviceDef =
        DeviceDefinition
          { registers =
              [ NamedLoc
                  { name = Name "receiveBuffer" ""
                  , loc = locHere
                  , value =
                      Register
                        { fieldType = regType @(Vec memDepth (Bytes 8))
                        , address = 0
                        , access = ReadOnly
                        , reset = Nothing
                        , tags = []
                        }
                  }
              ]
          , deviceName = Name{name = "ReceiveRingbuffer", description = ""}
          , definitionLoc = locHere
          , tags = []
          }
