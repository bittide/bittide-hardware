-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE RecordWildCards #-}

module Bittide.ScatterGather (
  scatterUnitWb,
  scatterUnitWbC,
  ScatterConfig (..),
  gatherUnitWb,
  gatherUnitWbC,
  GatherConfig (..),
) where

import Clash.Prelude

import Bittide.Calendar
import Bittide.DoubleBufferedRam
import Bittide.Extra.Maybe
import Bittide.SharedTypes

import Clash.Class.BitPackC
import Protocols
import Protocols.Wishbone

import Data.Constraint.Nat.Extra
import Data.Constraint.Nat.Lemmas
import GHC.Stack (HasCallStack)
import Protocols.MemoryMap

{- | Existential type to explicitly differentiate between a configuration for
the 'scatterUnitWb' and 'gatherUnitWb' at type level and hide the memory depth from
higher level APIs.
-}
data ScatterConfig nBytes addrW where
  ScatterConfig ::
    (KnownNat memDepth, 1 <= memDepth) =>
    { memDepth :: SNat memDepth
    , calendarConfig :: CalendarConfig addrW (Index memDepth)
    } ->
    ScatterConfig nBytes addrW

{- | Existential type to explicitly differentiate between a configuration for
the 'scatterUnitWb' and 'gatherUnitWb' at type level and hide the memory depth from
higher level APIs.
-}
data GatherConfig nBytes addrW where
  GatherConfig ::
    (KnownNat memDepth, 1 <= memDepth) =>
    { memDepth :: SNat memDepth
    , calendarConfig :: CalendarConfig addrW (Index memDepth)
    } ->
    GatherConfig nBytes addrW

{- | Double buffered memory component that can be written to by a Bittide link. The write
address of the incoming frame is determined by the incorporated 'calendar'. The buffers
are swapped at the beginning of each metacycle. Reading the buffer is done by supplying
a read address. Furthermore this component offers ports to control the incorporated 'calendar'.
-}
scatterUnit ::
  ( HiddenClockResetEnable dom
  , KnownNat memDepth
  , 1 <= memDepth
  , KnownNat frameWidth
  , KnownNat nBytes
  , 1 <= nBytes
  , KnownNat addrW
  , ?busByteOrder :: ByteOrder
  , ?regByteOrder :: ByteOrder
  ) =>
  -- | Configuration for the 'calendar'.
  CalendarConfig addrW (Index memDepth) ->
  -- | Wishbone (master -> slave) port for the 'calendar'.
  Signal dom (WishboneM2S addrW nBytes (Bytes nBytes)) ->
  -- | Incoming frame from Bittide link.
  Signal dom (BitVector frameWidth) ->
  -- | Read address.
  Signal dom (Index memDepth) ->
  -- | 1. Data at read address delayed 1 cycle
  --   2. Wishbone (slave -> master) from 'calendar')
  --   3. End of metacycle.
  ( Signal dom (BitVector frameWidth)
  , Signal dom (WishboneS2M (Bytes nBytes))
  , Signal dom Bool
  , Signal dom (Unsigned 32)
  , MM
  )
scatterUnit calConfig wbIn linkIn readAddr = (readOut, wbOut, endOfMetacycle, metacycleCount, mm)
 where
  (writeAddr, endOfMetacycle, wbOut, metacycleCount, mm) = mkCalendar "scatter" calConfig wbIn
  writeOp = curry Just <$> writeAddr <*> linkIn
  readOut = doubleBufferedRamU bufSelect readAddr writeOp
  bufSelect = regEn A endOfMetacycle (swapAorB <$> bufSelect)

{- | Double buffered memory component that can be written to by a generic write operation. The
write address of the incoming frame is determined by the incorporated 'calendar'. The
buffers are swapped at the beginning of each metacycle. Reading the buffer is done by
supplying a read address. Furthermore this component offers ports to control the
incorporated 'calendar'.
-}
gatherUnit ::
  ( HiddenClockResetEnable dom
  , KnownNat memDepth
  , 1 <= memDepth
  , KnownNat frameWidth
  , 1 <= frameWidth
  , KnownNat (DivRU frameWidth 8)
  , 1 <= DivRU frameWidth 8
  , KnownNat nBytes
  , 1 <= nBytes
  , KnownNat addrW
  , ?busByteOrder :: ByteOrder
  , ?regByteOrder :: ByteOrder
  ) =>
  -- | Configuration for the 'calendar'.
  CalendarConfig addrW (Index memDepth) ->
  -- | Wishbone (master -> slave) port for the 'calendar'.
  Signal dom (WishboneM2S addrW nBytes (Bytes nBytes)) ->
  -- | Write operation writing a frame.
  Signal dom (Maybe (LocatedBits memDepth frameWidth)) ->
  -- | Byte enable for write operation.
  Signal dom (ByteEnable (BitVector frameWidth)) ->
  -- | 1. Frame to Bittide Link.
  --   2. Wishbone (slave -> master) from 'calendar')
  --   3. End of metacycle.
  ( Signal dom (BitVector frameWidth)
  , Signal dom (WishboneS2M (Bytes nBytes))
  , Signal dom Bool
  , Signal dom (Unsigned 32)
  , MM
  )
gatherUnit calConfig wbIn writeOp byteEnables = (bramOut, wbOut, endOfMetacycle, metacycleCount, mm)
 where
  (readAddr, endOfMetacycle, wbOut, metacycleCount, mm) = mkCalendar "gather" calConfig wbIn
  bramOut = doubleBufferedRamByteAddressableU bufSelect readAddr writeOp byteEnables
  bufSelect = regEn A endOfMetacycle (swapAorB <$> bufSelect)

{- | Wishbone interface for the 'scatterUnit' and 'gatherUnit'. It makes the scatter and gather
unit, which operate on 64 bit frames, addressable via a 32 bit wishbone bus.
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
  -- | Read data to be send to over the (slave -> master) port.
  Bytes nBytes ->
  -- | (slave - master data, read address memory element, write data memory element)
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

{- | Adds a stalling address to the 'wbInterface' by demanding an extra address on type level.
When this address is accessed, the outgoing 'WishboneS2M' bus' acknowledge is replaced
with the @endOfMetacycle@ signal to stall the wishbone master until the end of the metacycle.
-}
addStalling ::
  forall memAddresses wbData a.
  (KnownNat memAddresses, 1 <= memAddresses, NumConvert (Unsigned 32) wbData) =>
  -- | Controls the 'acknowledge' of the returned 'WishboneS2M' when the incoming address
  -- is 'maxBound'.
  Bool ->
  -- | The current metacycle count.
  Unsigned 32 ->
  -- |
  --  1. Incoming 'WishboneS2M' bus.
  --  2. Incoming wishbone address (stalling address in range).
  --  3. Incoming write operation.
  ( WishboneS2M wbData
  , Index (memAddresses + 2)
  , Maybe a
  ) ->
  -- |
  --  1. Outgoing 'WishboneS2M' bus (@acknowledge@ replaced with @endOfMetacycle@ when @wbAddr == maxBound@).
  --  2. Outgoing wishbone address (stalling address not in range).
  --  3. Outgoing write operation (set to @Nothing@ when @wbAddr == maxBound@).
  ( WishboneS2M wbData
  , Index memAddresses
  , Maybe a
  )
addStalling endOfMetacycle metacycleCount (incomingBus@WishboneS2M{..}, wbAddr, writeOp0) =
  (slaveToMaster1, memAddr, writeOp1)
 where
  stalledBus = incomingBus{acknowledge = endOfMetacycle}
  (slaveToMaster1, writeOp1)
    | acknowledge && (wbAddr == maxBound) = (stalledBus, Nothing)
    | acknowledge && (wbAddr == maxBound - 1) =
        (incomingBus{readData = numConvert metacycleCount}, Nothing)
    | otherwise = (incomingBus, writeOp0)
  memAddr = bitCoerce $ resize wbAddr

{-# NOINLINE scatterUnitWb #-}

scatterUnitWbC ::
  forall dom awSu nBytesCal awCal.
  ( HasCallStack
  , HiddenClockResetEnable dom
  , KnownNat awSu
  , KnownNat nBytesCal
  , 1 <= nBytesCal
  , KnownNat awCal
  , ?busByteOrder :: ByteOrder
  , ?regByteOrder :: ByteOrder
  ) =>
  -- | Configuration for the 'calendar'.
  ScatterConfig nBytesCal awCal ->
  Signal dom (BitVector 64) ->
  Circuit
    ( (ConstBwd MM, Wishbone dom 'Standard awSu (Bytes 4))
    , (ConstBwd MM, Wishbone dom 'Standard awCal (Bytes nBytesCal))
    )
    ()
scatterUnitWbC conf@(ScatterConfig memDepthSnat _) linkIn = case cancelMulDiv @nBytesCal @8 of
  Dict -> Circuit go
   where
    go ((((), wbM2SSu), ((), wbM2SCal)), _) =
      (
        ( (SimOnly memoryMapScatterMem, wbS2MSu)
        , (memoryMapCal, wbS2MCal)
        )
      , ()
      )
     where
      (wbS2MSu, wbS2MCal, memoryMapCal) = scatterUnitWb conf wbM2SCal linkIn wbM2SSu

    memoryMapScatterMem =
      let
        deviceDef :: forall memDepth. SNat memDepth -> DeviceDefinition
        deviceDef SNat =
          DeviceDefinition
            { registers =
                [ NamedLoc
                    { name = Name "scatterMemory" ""
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
                , NamedLoc
                    { name = Name "metacycleCount" ""
                    , loc = locHere
                    , value =
                        Register
                          { fieldType = regType @(Bytes 4)
                          , address = afterMemory
                          , access = ReadOnly
                          , reset = Nothing
                          , tags = []
                          }
                    }
                , NamedLoc
                    { name = Name "metacycleRegister" ""
                    , loc = locHere
                    , value =
                        Register
                          { fieldType = regType @(Bytes 4)
                          , address = afterMemory + snatToInteger (SNat @(ByteSizeC (Bytes 4)))
                          , access = ReadOnly
                          , reset = Nothing
                          , tags = []
                          }
                    }
                ]
            , deviceName =
                Name
                  { name = "ScatterUnit"
                  , description = ""
                  }
            , definitionLoc = locHere
            , tags = []
            }
         where
          afterMemory = snatToInteger (SNat @(ByteSizeC (Vec memDepth (Bytes 8))))
       in
        MemoryMap
          { tree = DeviceInstance locCaller "ScatterUnit"
          , deviceDefs = deviceSingleton (deviceDef memDepthSnat)
          }

{- | Wishbone addressable 'scatterUnit', the wishbone port can read the data from this
memory element as if it has a 32 bit port by selecting the upper 32 or lower 32 bits
of the read data.
-}
scatterUnitWb ::
  forall dom awSu nBytesCal awCal.
  ( HiddenClockResetEnable dom
  , KnownNat awSu
  , KnownNat nBytesCal
  , 1 <= nBytesCal
  , KnownNat awCal
  , ?busByteOrder :: ByteOrder
  , ?regByteOrder :: ByteOrder
  ) =>
  -- | Configuration for the 'calendar'.
  ScatterConfig nBytesCal awCal ->
  -- | Wishbone (master -> slave) port 'calendar'.
  Signal dom (WishboneM2S awCal nBytesCal (Bytes nBytesCal)) ->
  -- | Incoming frame from Bittide link.
  Signal dom (BitVector 64) ->
  -- | Wishbone (master -> slave) port scatter memory.
  Signal dom (WishboneM2S awSu 4 (Bytes 4)) ->
  -- |
  -- 1. Wishbone (slave -> master) port scatter memory
  -- 2. Wishbone (slave -> master) port 'calendar'
  (Signal dom (WishboneS2M (Bytes 4)), Signal dom (WishboneS2M (Bytes nBytesCal)), MM)
scatterUnitWb (ScatterConfig _memDepth calConfig) wbInCal linkIn wbInSu =
  (delayControls wbOutSu, wbOutCal, mm)
 where
  (wbOutSu, memAddr, _) =
    unbundle
      $ addStalling
      <$> endOfMetacycle
      <*> metacycleCount
      <*> (wbInterface <$> wbInSu <*> scatteredData)
  (readAddr, upperSelected) = unbundle $ div2Index <$> memAddr
  (scatterUnitRead, wbOutCal, endOfMetacycle, metacycleCount, mm) =
    scatterUnit calConfig wbInCal linkIn readAddr
  (lower, upper) = unbundle $ split <$> scatterUnitRead
  selected = register (errorX "scatterUnitWb: Initial selection undefined") upperSelected
  scatteredData = mux selected upper lower

{-# NOINLINE gatherUnitWb #-}

gatherUnitWbC ::
  forall dom awGu nBytesCal awCal.
  ( HasCallStack
  , HiddenClockResetEnable dom
  , KnownNat awGu
  , KnownNat nBytesCal
  , 1 <= nBytesCal
  , KnownNat awCal
  , ?busByteOrder :: ByteOrder
  , ?regByteOrder :: ByteOrder
  ) =>
  -- | Configuration for the 'calendar'.
  GatherConfig nBytesCal awCal ->
  Circuit
    ( (ConstBwd MM, Wishbone dom 'Standard awGu (Bytes 4))
    , (ConstBwd MM, Wishbone dom 'Standard awCal (Bytes nBytesCal))
    )
    (CSignal dom (BitVector 64))
gatherUnitWbC conf@(GatherConfig memDepthSnat _) = case (cancelMulDiv @nBytesCal @8) of
  Dict -> Circuit go
   where
    go ::
      ( ( ((), Signal dom (WishboneM2S awGu 4 (BitVector 32)))
        , ( ()
          , Signal
              dom
              (WishboneM2S awCal nBytesCal (BitVector (nBytesCal * 8)))
          )
        )
      , Signal dom ()
      ) ->
      ( ( (SimOnly MemoryMap, Signal dom (WishboneS2M (BitVector 32)))
        , ( SimOnly MemoryMap
          , Signal dom (WishboneS2M (BitVector (nBytesCal * 8)))
          )
        )
      , Signal dom (BitVector 64)
      )
    go ((((), wbInGu), ((), wbInCal)), _) =
      (
        ( (SimOnly memMapGu, wbOutGu)
        , (memMapCal, wbOutCal)
        )
      , linkOut
      )
     where
      (linkOut, wbOutGu, wbOutCal, memMapCal) = gatherUnitWb conf wbInCal wbInGu

    memMapGu =
      MemoryMap
        { tree = DeviceInstance locCaller "GatherUnit"
        , deviceDefs = deviceSingleton (deviceDef memDepthSnat)
        }

    deviceDef :: forall memDepth. SNat memDepth -> DeviceDefinition
    deviceDef SNat =
      DeviceDefinition
        { registers =
            [ NamedLoc
                { name = Name "gatherMemory" ""
                , loc = locHere
                , value =
                    Register
                      { fieldType = regType @(Vec memDepth (Bytes 8))
                      , address = 0
                      , access = WriteOnly
                      , reset = Nothing
                      , tags = []
                      }
                }
            , NamedLoc
                { name = Name "metacycleCount" ""
                , loc = locHere
                , value =
                    Register
                      { fieldType = regType @(Bytes 4)
                      , address = afterMemory
                      , access = ReadOnly
                      , reset = Nothing
                      , tags = []
                      }
                }
            , NamedLoc
                { name = Name "metacycleRegister" ""
                , loc = locHere
                , value =
                    Register
                      { fieldType = regType @(Bytes 4)
                      , address = afterMemory + snatToInteger (SNat @(ByteSizeC (Bytes 4)))
                      , access = ReadOnly
                      , reset = Nothing
                      , tags = []
                      }
                }
            ]
        , deviceName =
            Name
              { name = "GatherUnit"
              , description = ""
              }
        , definitionLoc = locHere
        , tags = []
        }
     where
      afterMemory = snatToInteger (SNat @(ByteSizeC (Vec memDepth (Bytes 8))))

{- | Wishbone addressable 'gatherUnit', the wishbone port can write data to this
memory element as if it has a 32 bit port by controlling the byte enables of the
'gatherUnit' based on the third bit.
-}
gatherUnitWb ::
  forall dom awGu nBytesCal awCal.
  ( HiddenClockResetEnable dom
  , KnownNat awGu
  , KnownNat nBytesCal
  , 1 <= nBytesCal
  , KnownNat awCal
  , ?busByteOrder :: ByteOrder
  , ?regByteOrder :: ByteOrder
  ) =>
  -- | Configuration for the 'calendar'.
  GatherConfig nBytesCal awCal ->
  -- | Wishbone (master -> slave) data 'calendar'.
  Signal dom (WishboneM2S awCal nBytesCal (Bytes nBytesCal)) ->
  -- | Wishbone (master -> slave) port gather memory.
  Signal dom (WishboneM2S awGu 4 (Bytes 4)) ->
  -- |
  -- 1. Wishbone (slave -> master) port gather memory
  -- 2. Wishbone (slave -> master) port 'calendar'
  ( Signal dom (BitVector 64)
  , Signal dom (WishboneS2M (Bytes 4))
  , Signal dom (WishboneS2M (Bytes nBytesCal))
  , MM
  )
gatherUnitWb (GatherConfig _memDepth calConfig) wbInCal wbInGu =
  (linkOut, delayControls wbOutGu, wbOutCal, mm)
 where
  (wbOutGu, memAddr, writeOp) =
    unbundle
      $ addStalling
      <$> endOfMetacycle
      <*> metacycleCount
      <*> (wbInterface <$> wbInGu <*> pure 0b0)
  (writeAddr, upperSelected) = unbundle $ div2Index <$> memAddr
  (linkOut, wbOutCal, endOfMetacycle, metacycleCount, mm) =
    gatherUnit calConfig wbInCal gatherWrite gatherByteEnables
  gatherWrite = mkWrite <$> writeAddr <*> writeOp
  gatherByteEnables = mkEnables <$> upperSelected <*> (busSelect <$> wbInGu)

  -- We update the 64 bit entry of the 'gatherUnit' in chunks of 32 bits. Thus we repeat
  -- the writeData of the wishbone bus twice in the write operation to the 'gatherUnit' and
  -- use the byte enables to either update the upper or lower
  mkWrite address (Just write) = Just (address, write ++# write)
  mkWrite _ _ = Nothing
  mkEnables selected byteEnables
    | selected = 0 ++# byteEnables
    | otherwise = byteEnables ++# 0
