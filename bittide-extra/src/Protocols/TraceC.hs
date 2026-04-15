-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- | Many of these functions should be added to `clash-protocols`, there is a PR
for this: https://github.com/clash-lang/clash-protocols/pull/116
And a bittide-hardware issue:
https://github.com/bittide/bittide-hardware/issues/645
-}
module Protocols.TraceC (TraceC (..)) where

import Clash.Prelude

import Data.Data (Proxy (..))
import Data.Either (isLeft)
import Data.Maybe (isJust)
import Data.String.Interpolate (i)
import Data.Typeable (Typeable)
import Protocols
import Protocols.Axi4.ReadAddress (
  Axi4ReadAddress,
  KnownAxi4ReadAddressConfig,
  M2S_ReadAddress (..),
 )
import Protocols.Axi4.ReadData (Axi4ReadData, KnownAxi4ReadDataConfig, S2M_ReadData (..))
import Protocols.Axi4.Stream (Axi4Stream, Axi4StreamM2S, Axi4StreamS2M, KnownAxi4StreamConfig)
import Protocols.Axi4.WriteAddress (
  Axi4WriteAddress,
  KnownAxi4WriteAddressConfig,
  M2S_WriteAddress (..),
 )
import Protocols.Axi4.WriteData (
  Axi4WriteData,
  KnownAxi4WriteDataConfig,
  M2S_WriteData (..),
  S2M_WriteData,
 )
import Protocols.Axi4.WriteResponse (
  Axi4WriteResponse,
  KnownAxi4WriteResponseConfig,
  S2M_WriteResponse (..),
 )
import Protocols.Extra.TH
import Protocols.Internal (reverseCircuit)
import Protocols.Internal.Types.Extra (TraceC)
import Protocols.PacketStream (PacketStream, PacketStreamM2S, PacketStreamS2M)
import Protocols.ReqResp (ReqResp)
import Protocols.Spi (Spi)
import Protocols.Vec (vecCircuits)
import Protocols.Wishbone

import qualified Clash.Prelude as C
import qualified Clash.Shockwaves as Shock
import qualified Clash.Shockwaves.Trace.CRE as Shock
import qualified Debug.Trace as Debug
import qualified Protocols.Axi4.Common as Axi4
import qualified Protocols.Axi4.ReadAddress as Axi4
import qualified Protocols.Axi4.ReadData as Axi4
import qualified Protocols.Axi4.WriteAddress as Axi4
import qualified Protocols.Axi4.WriteData as Axi4
import qualified Protocols.Axi4.WriteResponse as Axi4
import qualified Protocols.Internal.Types.Extra as Trace (TraceC (..))

instance TraceC () where
  shock _ = idC
  signal _ = idC
  names _ _ = []
  trace _ = idC

instance (TraceC a, TraceC b) => TraceC (a, b) where
  shock name = circuit $ \(a, b) -> do
    a' <- Trace.shock (name <> "_0") -< a
    b' <- Trace.shock (name <> "_1") -< b
    idC -< (a', b')
  signal name = circuit $ \(a, b) -> do
    a' <- Trace.signal (name <> "_0") -< a
    b' <- Trace.signal (name <> "_1") -< b
    idC -< (a', b')
  names _ name =
    (Trace.names (Proxy @a) (name <> "_0"))
      <> (Trace.names (Proxy @b) (name <> "_1"))
  trace name = circuit $ \(a, b) -> do
    a' <- Trace.trace (name <> "_0") -< a
    b' <- Trace.trace (name <> "_1") -< b
    idC -< (a', b')

$(traceCTupleInstances 3 3)

instance (KnownNat n, TraceC p) => TraceC (Vec n p) where
  shock name = vecCircuits (fmap (\n -> Trace.shock (name <> "_" <> show n)) indicesI)
  signal name = vecCircuits (fmap (\n -> Trace.signal (name <> "_" <> show n)) indicesI)
  names _ name = fmap (<> "n") (Trace.names (Proxy @p) name)
  trace name = vecCircuits (fmap (\n -> Trace.trace (name <> "_" <> show n)) indicesI)

instance (KnownDomain dom, NFDataX a, Shock.Waveform a) => TraceC (CSignal dom a) where
  shock name = applyC (Shock.traceSignal name) id
  signal name = applyC (C.traceSignal name) id
  names _ name = [name]
  trace name = applyC (Debug.trace name) id

instance (KnownDomain dom) => TraceC (Clock dom) where
  shock name = applyC (Shock.traceClock name) id
  signal name = error (name <> ": Tracing clocks with Trace.signal is not supported, use Trace.shock instead.")
  names _ name = [name]
  trace name = error (name <> ": Clocks are only used for wiring and can not be traced with Debug.Trace.trace")

instance (KnownDomain dom) => TraceC (Reset dom) where
  shock name = applyC (Shock.traceReset name) id
  signal name = applyC (C.unsafeToReset . C.traceSignal name . C.unsafeFromReset) id
  names _ name = [name]
  trace name = applyC (C.unsafeFromActiveHigh . fmap printMsg . C.unsafeToActiveHigh) id
   where
    printMsg r = Debug.trace [i|#{name}: #{resolveReset r}|] r
    resolveReset r
      | isLeft $ hasX r = "Undefined"
      | r = "Asserted"
      | otherwise = "Deasserted"

instance (KnownDomain dom) => TraceC (Enable dom) where
  shock name = applyC (Shock.traceEnable name) id
  signal name = applyC (C.toEnable . C.traceSignal name . C.fromEnable) id
  names _ name = [name]
  trace name = applyC (C.toEnable . fmap printMsg . C.fromEnable) id
   where
    printMsg e = Debug.trace [i|#{name}: #{resolveEnable e}|] e
    resolveEnable e
      | isLeft $ hasX e = "Undefined"
      | e = "Enabled"
      | otherwise = "Disabled"

instance (TraceC a) => TraceC (Reverse a) where
  shock name = reverseCircuit (Trace.shock name)
  signal name = reverseCircuit (Trace.signal name)
  names _ name = Trace.names (Proxy @a) name
  trace name = reverseCircuit (Trace.trace name)

-- Df derivations
deriving newtype instance Shock.Waveform Ack

-- Wishbone derivations
deriving anyclass instance Shock.Waveform (CycleTypeIdentifier)
deriving anyclass instance Shock.Waveform (BurstTypeExtension)
deriving anyclass instance (KnownNat nBytes, KnownNat aw) => Shock.Waveform (WishboneM2S aw nBytes)
deriving anyclass instance (KnownNat nBytes) => Shock.Waveform (WishboneS2M nBytes)

-- Packetstream derivations
deriving anyclass instance (KnownNat w, BitPack meta) => BitPack (PacketStreamM2S w meta)
deriving anyclass instance BitPack PacketStreamS2M
deriving anyclass instance
  (KnownNat w, BitPack meta, Shock.Waveform meta) => Shock.Waveform (PacketStreamM2S w meta)
deriving anyclass instance Shock.Waveform (PacketStreamS2M)

-- AXI4 derivations
deriving anyclass instance
  (KnownAxi4StreamConfig conf, BitPack userType) => BitPack (Axi4StreamM2S conf userType)
deriving anyclass instance
  ( KnownAxi4StreamConfig conf
  , Typeable conf
  , BitPack userType
  , Shock.Waveform userType
  , Typeable userType
  ) =>
  Shock.Waveform (Axi4StreamM2S conf userType)
deriving anyclass instance BitPack Axi4StreamS2M
deriving anyclass instance Shock.Waveform Axi4StreamS2M
deriving anyclass instance
  (KnownAxi4ReadDataConfig conf, BitPack dataType, BitPack userType) =>
  BitPack (S2M_ReadData conf userType dataType)
deriving anyclass instance ShowX (S2M_WriteData)
deriving instance (Typeable (Proxy a), BitPack (Proxy a)) => Shock.Waveform (Proxy a)
deriving anyclass instance Shock.Waveform Axi4.S2M_ReadAddress
deriving anyclass instance Shock.Waveform Axi4.S2M_WriteAddress
deriving anyclass instance Shock.Waveform Axi4.M2S_ReadData
deriving anyclass instance Shock.Waveform Axi4.S2M_WriteData
deriving anyclass instance Shock.Waveform Axi4.M2S_WriteResponse

instance
  ( KnownDomain dom
  , NFDataX a
  , ShowX a
  , Shock.Waveform a
  , Shock.Waveform (Maybe a)
  , Typeable a
  , BitPack a
  ) =>
  TraceC (Df dom a)
  where
  shock name =
    Circuit $ \(fwd, bwd) ->
      (Shock.traceSignal (name <> "_ack") bwd, Shock.traceSignal (name <> "_dat") fwd)
  signal name =
    Circuit $ \(fwd, bwd) ->
      (C.traceSignal (name <> "_ack") bwd, C.traceSignal (name <> "_dat") fwd)
  names _ name = [name <> "_dat", name <> "_ack"]
  trace name = Circuit (unbundle . fmap go . bundle)
   where
    go ~(m2s, s2m0)
      | isJust m2s =
          let msg = [i| #{name}: #{showX m2s}, #{showX s2m0}|]
              s2m1 = Debug.trace msg s2m0
           in (s2m1, m2s)
      | otherwise = (s2m0, m2s)

instance (KnownDomain dom, KnownNat nBytes, KnownNat aw) => TraceC (Wishbone dom mode aw nBytes) where
  shock name = applyC (Shock.traceSignal (name <> "_m2s")) (Shock.traceSignal (name <> "_s2m"))
  signal name = applyC (C.traceSignal (name <> "_m2s")) (C.traceSignal (name <> "_s2m"))
  names _ name = [name <> "_m2s", name <> "_s2m"]
  trace name = Circuit (unbundle . fmap go . bundle)
   where
    go ~(m2s, s2m)
      | m2s.busCycle && m2s.strobe =
          (Debug.trace ([i| #{name}: #{showX m2s}, #{showX s2m} |]) s2m, m2s)
      | otherwise = (s2m, m2s)

instance
  ( KnownDomain dom
  , NFDataX req
  , NFDataX resp
  , ShowX req
  , ShowX resp
  , Shock.Waveform (Maybe req)
  , Shock.Waveform (Maybe resp)
  , Typeable req
  , Typeable resp
  , BitPack req
  , BitPack resp
  ) =>
  TraceC (ReqResp dom req resp)
  where
  shock name =
    Circuit $ \(fwd, bwd) ->
      (Shock.traceSignal (name <> "_resp") bwd, Shock.traceSignal (name <> "_req") fwd)
  signal name =
    Circuit $ \(fwd, bwd) ->
      (C.traceSignal (name <> "_resp") bwd, C.traceSignal (name <> "_req") fwd)
  names _ name = [name <> "_req", name <> "_resp"]
  trace name = Circuit (unbundle . fmap go . bundle)
   where
    go ~(m2s0, s2m)
      | isJust m2s0 =
          let msg = [i| #{name}: req = #{showX m2s0}, resp = #{showX s2m}|]
              m2s1 = Debug.trace msg m2s0
           in (s2m, m2s1)
      | otherwise = (s2m, m2s0)

instance (KnownDomain dom) => TraceC (Spi dom) where
  -- Unique names for request and response signals are needed to distinguish them for the
  -- tracing infastructure.
  shock name =
    Circuit $ \(fwd, bwd) ->
      (Shock.traceSignal (name <> "_s2m") bwd, Shock.traceSignal (name <> "_m2s") fwd)
  signal name =
    Circuit $ \(fwd, bwd) ->
      (C.traceSignal (name <> "_s2m") bwd, C.traceSignal (name <> "_m2s") fwd)
  names _ name = [name <> "_m2s", name <> "_s2m"]
  trace name = Circuit (unbundle . fmap go . bundle)
   where
    go ~(m2s, s2m) = (s2m, Debug.trace ([i| #{name}: #{showX m2s}, #{showX s2m} |]) m2s)

instance
  ( KnownDomain dom
  , KnownNat w
  , Shock.Waveform meta
  , BitPack meta
  , NFDataX meta
  , Typeable meta
  , ShowX meta
  ) =>
  TraceC (PacketStream dom w meta)
  where
  shock name =
    Circuit $ \(fwd, bwd) ->
      (Shock.traceSignal (name <> "_bwd") bwd, Shock.traceSignal (name <> "_fwd") fwd)
  signal name =
    Circuit $ \(fwd, bwd) ->
      (C.traceSignal (name <> "_bwd") bwd, C.traceSignal (name <> "_fwd") fwd)
  names _ name = [name <> "_fwd", name <> "_bwd"]
  trace name = Circuit (unbundle . fmap go . bundle)
   where
    go ~(m2s0, s2m)
      | isJust m2s0 =
          let msg = [i| #{name}: #{showX m2s0}, #{showX s2m}|]
              m2s1 = Debug.trace msg m2s0
           in (s2m, m2s1)
      | otherwise = (s2m, m2s0)

-- Axi4Stream
instance
  ( KnownDomain dom
  , KnownAxi4StreamConfig conf
  , Typeable conf
  , Shock.Waveform userType
  , BitPack userType
  , NFDataX userType
  , Typeable userType
  , ShowX userType
  ) =>
  TraceC (Axi4Stream dom conf userType)
  where
  shock name =
    Circuit $ \(fwd, bwd) ->
      (Shock.traceSignal (name <> "_bwd") bwd, Shock.traceSignal (name <> "_fwd") fwd)
  signal name =
    Circuit $ \(fwd, bwd) ->
      (C.traceSignal (name <> "_bwd") bwd, C.traceSignal (name <> "_fwd") fwd)
  names _ name = [name <> "_fwd", name <> "_bwd"]
  trace name = Circuit (unbundle . fmap go . bundle)
   where
    go ~(m2s0, s2m)
      | isJust m2s0 =
          let msg = [i| #{name}: #{showX m2s0}, #{showX s2m}|]
              m2s1 = Debug.trace msg m2s0
           in (s2m, m2s1)
      | otherwise = (s2m, m2s0)

deriving instance
  ( KnownAxi4ReadAddressConfig conf
  , Typeable conf
  , Shock.Waveform userType
  , Shock.Waveform (Axi4.RegionType (Axi4.ARKeepRegion conf))
  , Shock.Waveform (Axi4.BurstLengthType (Axi4.ARKeepBurstLength conf))
  , Shock.Waveform (Axi4.SizeType (Axi4.ARKeepSize conf))
  , Shock.Waveform (Axi4.BurstType (Axi4.ARKeepBurst conf))
  , Shock.Waveform (Axi4.LockType (Axi4.ARKeepLock conf))
  , Shock.Waveform (Axi4.ArCacheType (Axi4.ARKeepCache conf))
  , Shock.Waveform (Axi4.PermissionsType (Axi4.ARKeepPermissions conf))
  , Shock.Waveform (Axi4.QosType (Axi4.ARKeepQos conf))
  ) =>
  Shock.Waveform (M2S_ReadAddress conf userType)

deriving instance
  ( KnownAxi4WriteAddressConfig conf
  , Typeable conf
  , Shock.Waveform userType
  , Shock.Waveform (Axi4.RegionType (Axi4.AWKeepRegion conf))
  , Shock.Waveform (Axi4.BurstLengthType (Axi4.AWKeepBurstLength conf))
  , Shock.Waveform (Axi4.SizeType (Axi4.AWKeepSize conf))
  , Shock.Waveform (Axi4.BurstType (Axi4.AWKeepBurst conf))
  , Shock.Waveform (Axi4.LockType (Axi4.AWKeepLock conf))
  , Shock.Waveform (Axi4.AwCacheType (Axi4.AWKeepCache conf))
  , Shock.Waveform (Axi4.PermissionsType (Axi4.AWKeepPermissions conf))
  , Shock.Waveform (Axi4.QosType (Axi4.AWKeepQos conf))
  ) =>
  Shock.Waveform (M2S_WriteAddress conf userType)

deriving instance
  ( KnownAxi4ReadDataConfig conf
  , Typeable conf
  , Shock.Waveform userType
  , Shock.Waveform dataType
  , Shock.Waveform (Axi4.ResponseType (Axi4.RKeepResponse conf))
  ) =>
  Shock.Waveform (S2M_ReadData conf userType dataType)

deriving instance
  ( KnownAxi4WriteDataConfig conf
  , Typeable conf
  , Shock.Waveform userType
  , Shock.Waveform (Axi4.StrobeDataType (Axi4.WKeepStrobe conf))
  ) =>
  Shock.Waveform (M2S_WriteData conf userType)

deriving instance
  ( KnownAxi4WriteResponseConfig conf
  , Typeable conf
  , Shock.Waveform userType
  , Shock.Waveform (Axi4.ResponseType (Axi4.BKeepResponse conf))
  ) =>
  Shock.Waveform (S2M_WriteResponse conf userType)

-- Axi4ReadAddress
instance
  ( KnownDomain dom
  , KnownAxi4ReadAddressConfig conf
  , NFDataX userType
  , ShowX userType
  , BitPack userType
  , Typeable userType
  , Typeable conf
  , Shock.Waveform userType
  , Shock.Waveform (Axi4.RegionType (Axi4.ARKeepRegion conf))
  , Shock.Waveform (Axi4.BurstLengthType (Axi4.ARKeepBurstLength conf))
  , Shock.Waveform (Axi4.SizeType (Axi4.ARKeepSize conf))
  , Shock.Waveform (Axi4.BurstType (Axi4.ARKeepBurst conf))
  , Shock.Waveform (Axi4.LockType (Axi4.ARKeepLock conf))
  , Shock.Waveform (Axi4.ArCacheType (Axi4.ARKeepCache conf))
  , Shock.Waveform (Axi4.PermissionsType (Axi4.ARKeepPermissions conf))
  , Shock.Waveform (Axi4.QosType (Axi4.ARKeepQos conf))
  ) =>
  TraceC (Axi4ReadAddress dom conf userType)
  where
  shock name =
    Circuit $ \(fwd, bwd) ->
      (Shock.traceSignal (name <> "_bwd") bwd, Shock.traceSignal (name <> "_fwd") fwd)
  signal name =
    Circuit $ \(fwd, bwd) ->
      (C.traceSignal (name <> "_bwd") bwd, C.traceSignal (name <> "_fwd") fwd)
  names _ name = [name <> "_fwd", name <> "_bwd"]
  trace name = Circuit (unbundle . fmap go . bundle)
   where
    go ~(m2s, s2m) = case m2s of
      M2S_NoReadAddress -> (s2m, m2s)
      M2S_ReadAddress{} ->
        (Debug.trace ([i| #{name}: #{showX m2s}, #{showX s2m}|]) s2m, m2s)

-- Axi4WriteAddress
instance
  ( KnownDomain dom
  , KnownAxi4WriteAddressConfig conf
  , NFDataX userType
  , ShowX userType
  , BitPack userType
  , Typeable userType
  , Typeable conf
  , Shock.Waveform userType
  , Shock.Waveform (Axi4.RegionType (Axi4.AWKeepRegion conf))
  , Shock.Waveform (Axi4.BurstLengthType (Axi4.AWKeepBurstLength conf))
  , Shock.Waveform (Axi4.SizeType (Axi4.AWKeepSize conf))
  , Shock.Waveform (Axi4.BurstType (Axi4.AWKeepBurst conf))
  , Shock.Waveform (Axi4.LockType (Axi4.AWKeepLock conf))
  , Shock.Waveform (Axi4.AwCacheType (Axi4.AWKeepCache conf))
  , Shock.Waveform (Axi4.PermissionsType (Axi4.AWKeepPermissions conf))
  , Shock.Waveform (Axi4.QosType (Axi4.AWKeepQos conf))
  ) =>
  TraceC (Axi4WriteAddress dom conf userType)
  where
  shock name =
    Circuit $ \(fwd, bwd) ->
      (Shock.traceSignal (name <> "_bwd") bwd, Shock.traceSignal (name <> "_fwd") fwd)
  signal name =
    Circuit $ \(fwd, bwd) ->
      (C.traceSignal (name <> "_bwd") bwd, C.traceSignal (name <> "_fwd") fwd)
  names _ name = [name <> "_fwd", name <> "_bwd"]
  trace name = Circuit (unbundle . fmap go . bundle)
   where
    go ~(m2s, s2m) = case m2s of
      M2S_NoWriteAddress -> (s2m, m2s)
      M2S_WriteAddress{} ->
        (Debug.trace ([i| #{name}: #{showX m2s}, #{showX s2m}|]) s2m, m2s)

-- Axi4ReadData (reversed: Fwd = S2M_ReadData, Bwd = M2S_ReadData)
instance
  ( KnownDomain dom
  , KnownAxi4ReadDataConfig conf
  , NFDataX userType
  , NFDataX dataType
  , ShowX userType
  , ShowX dataType
  , BitPack dataType
  , BitPack userType
  , Typeable userType
  , Typeable dataType
  , Typeable conf
  , Shock.Waveform userType
  , Shock.Waveform dataType
  , Shock.Waveform (Axi4.ResponseType (Axi4.RKeepResponse conf))
  ) =>
  TraceC (Axi4ReadData dom conf userType dataType)
  where
  shock name =
    Circuit $ \(fwd, bwd) ->
      (Shock.traceSignal (name <> "_bwd") bwd, Shock.traceSignal (name <> "_fwd") fwd)
  signal name =
    Circuit $ \(fwd, bwd) ->
      (C.traceSignal (name <> "_bwd") bwd, C.traceSignal (name <> "_fwd") fwd)
  names _ name = [name <> "_fwd", name <> "_bwd"]
  trace name = Circuit (unbundle . fmap go . bundle)
   where
    go ~(s2m, m2s) = case s2m of
      S2M_NoReadData -> (m2s, s2m)
      S2M_ReadData{} ->
        (Debug.trace ([i| #{name}: #{showX s2m}, #{showX m2s}|]) m2s, s2m)

-- Axi4WriteData
instance
  ( KnownDomain dom
  , KnownAxi4WriteDataConfig conf
  , NFDataX userType
  , ShowX userType
  , BitPack userType
  , Typeable userType
  , Typeable conf
  , Shock.Waveform userType
  , Shock.Waveform (Axi4.StrobeDataType (Axi4.WKeepStrobe conf))
  ) =>
  TraceC (Axi4WriteData dom conf userType)
  where
  shock name =
    Circuit $ \(fwd, bwd) ->
      (Shock.traceSignal (name <> "_bwd") bwd, Shock.traceSignal (name <> "_fwd") fwd)
  signal name =
    Circuit $ \(fwd, bwd) ->
      (C.traceSignal (name <> "_bwd") bwd, C.traceSignal (name <> "_fwd") fwd)
  names _ name = [name <> "_fwd", name <> "_bwd"]
  trace name = Circuit (unbundle . fmap go . bundle)
   where
    go ~(m2s, s2m) = case m2s of
      M2S_NoWriteData -> (s2m, m2s)
      M2S_WriteData{} ->
        (Debug.trace ([i| #{name}: #{showX m2s}, #{showX s2m}|]) s2m, m2s)

-- Axi4WriteResponse (reversed: Fwd = S2M_WriteResponse, Bwd = M2S_WriteResponse)
instance
  ( KnownDomain dom
  , KnownAxi4WriteResponseConfig conf
  , NFDataX userType
  , ShowX userType
  , BitPack userType
  , Typeable userType
  , Typeable conf
  , Shock.Waveform userType
  , Shock.Waveform (Axi4.ResponseType (Axi4.BKeepResponse conf))
  ) =>
  TraceC (Axi4WriteResponse dom conf userType)
  where
  shock name =
    Circuit $ \(fwd, bwd) ->
      (Shock.traceSignal (name <> "_bwd") bwd, Shock.traceSignal (name <> "_fwd") fwd)
  signal name =
    Circuit $ \(fwd, bwd) ->
      (C.traceSignal (name <> "_bwd") bwd, C.traceSignal (name <> "_fwd") fwd)
  names _ name = [name <> "_fwd", name <> "_bwd"]
  trace name = Circuit (unbundle . fmap go . bundle)
   where
    go ~(s2m, m2s) = case s2m of
      S2M_NoWriteResponse -> (m2s, s2m)
      S2M_WriteResponse{} ->
        (Debug.trace ([i| #{name}: #{showX s2m}, #{showX m2s}|]) m2s, s2m)
