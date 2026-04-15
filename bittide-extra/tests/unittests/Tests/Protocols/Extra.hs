-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Tests.Protocols.Extra (tests) where

import Clash.Prelude

import Data.Data (Proxy (..))
import Data.Either (isRight)
import Data.List (isInfixOf)
import Protocols
import Protocols.Extra ()
import Test.Tasty (DependencyType (..), TestTree, dependentTestGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

import qualified Clash.Prelude as C
import qualified Clash.Shockwaves.Trace as Shock
import qualified Clash.Signal.Trace as Trace
import qualified Data.Text as Text
import qualified Protocols.TraceC as Trace
import qualified Prelude as P

import Protocols.Axi4.ReadAddress (
  Axi4ReadAddress,
  Axi4ReadAddressConfig (..),
  M2S_ReadAddress (..),
  S2M_ReadAddress (..),
 )
import Protocols.Axi4.ReadData (
  Axi4ReadData,
  Axi4ReadDataConfig (..),
  M2S_ReadData (..),
  S2M_ReadData (..),
 )
import Protocols.Axi4.Stream (
  Axi4Stream,
  Axi4StreamConfig (..),
  Axi4StreamM2S (..),
  Axi4StreamS2M (..),
 )
import Protocols.Axi4.WriteAddress (
  Axi4WriteAddress,
  Axi4WriteAddressConfig (..),
  M2S_WriteAddress (..),
  S2M_WriteAddress (..),
 )
import Protocols.Axi4.WriteData (
  Axi4WriteData,
  Axi4WriteDataConfig (..),
  M2S_WriteData (..),
  S2M_WriteData (..),
 )
import Protocols.Axi4.WriteResponse (
  Axi4WriteResponse,
  Axi4WriteResponseConfig (..),
  M2S_WriteResponse (..),
  S2M_WriteResponse (..),
 )
import Protocols.BiDf (BiDf)
import Protocols.Df ()
import Protocols.PacketStream (PacketStream, PacketStreamM2S (..), PacketStreamS2M (..))
import Protocols.Spi (Spi)
import qualified Protocols.Spi as Spi
import Protocols.Wishbone

-- * Helpers

nCycles :: Int
nCycles = 100

-- | Check that a Shock VCD text contains all the given signal names.
assertVcdContains :: Either String (Text.Text, a) -> [String] -> IO ()
assertVcdContains result names = do
  assertBool "dumpVCD succeeded" (isRight result)
  case result of
    Right (vcdText, _) ->
      let vcd = Text.unpack vcdText
       in P.mapM_ (\n -> assertBool ("VCD contains " <> n) (n `isInfixOf` vcd)) names
    Left _ -> error "Expected Right from dumpVCD"

-- | Check that a Clash VCD text contains all the given signal names.
assertClashVcdContains :: Either String Text.Text -> [String] -> IO ()
assertClashVcdContains result names = do
  assertBool "dumpVCD succeeded" (isRight result)
  case result of
    Right vcdText ->
      let vcd = Text.unpack vcdText
       in P.mapM_ (\n -> assertBool ("VCD contains " <> n) (n `isInfixOf` vcd)) names
    Left _ -> error "Expected Right from dumpVCD"

-- * Type aliases for concrete AXI4 configs

type SimpleAxi4StreamConf = 'Axi4StreamConfig 1 0 0

type SimpleAxi4ReadAddrConf =
  'Axi4ReadAddressConfig
    'False
    'False
    0
    8
    'False
    'False
    'False
    'False
    'False
    'False

type SimpleAxi4WriteAddrConf =
  'Axi4WriteAddressConfig
    'False
    'False
    0
    8
    'False
    'False
    'False
    'False
    'False
    'False

type SimpleAxi4ReadDataConf = 'Axi4ReadDataConfig 'False 0
type SimpleAxi4WriteDataConf = 'Axi4WriteDataConfig 'False 1
type SimpleAxi4WriteRespConf = 'Axi4WriteResponseConfig 'False 0

type TestBiDf = BiDf System (Unsigned 8) (Unsigned 8)
type TestTriple = (Df System (Unsigned 8), Spi System, PacketStream System 1 ())
-- * Tests

-- ** Trace.names tests

case_names :: IO ()
case_names = do
  Trace.names (Proxy @(Wishbone System Standard 8 4)) "wb" @?= ["wb_m2s", "wb_s2m"]
  Trace.names (Proxy @(Df System (Unsigned 8))) "df" @?= ["df_dat", "df_ack"]
  Trace.names (Proxy @(Spi System)) "spi" @?= ["spi_m2s", "spi_s2m"]
  Trace.names (Proxy @(PacketStream System 1 ())) "ps" @?= ["ps_fwd", "ps_bwd"]
  Trace.names (Proxy @(Axi4Stream System SimpleAxi4StreamConf ())) "as" @?= ["as_fwd", "as_bwd"]
  Trace.names (Proxy @(Axi4ReadAddress System SimpleAxi4ReadAddrConf ())) "ar"
    @?= ["ar_fwd", "ar_bwd"]
  Trace.names (Proxy @(Axi4WriteAddress System SimpleAxi4WriteAddrConf ())) "aw"
    @?= ["aw_fwd", "aw_bwd"]
  Trace.names (Proxy @(Axi4ReadData System SimpleAxi4ReadDataConf () (BitVector 8))) "rd"
    @?= ["rd_fwd", "rd_bwd"]
  Trace.names (Proxy @(Axi4WriteData System SimpleAxi4WriteDataConf ())) "wd" @?= ["wd_fwd", "wd_bwd"]
  Trace.names (Proxy @(Axi4WriteResponse System SimpleAxi4WriteRespConf ())) "wr"
    @?= ["wr_fwd", "wr_bwd"]
  Trace.names (Proxy @TestBiDf) "bd"
    @?= ["bd_0_dat", "bd_0_ack", "bd_1_dat", "bd_1_ack"]
  Trace.names (Proxy @TestTriple) "t3"
    @?= [ "t3_0_dat"
        , "t3_0_ack"
        , "t3_1_m2s"
        , "t3_1_s2m"
        , "t3_2_fwd"
        , "t3_2_bwd"
        ]

-- ** Trace.shock tests

-- For protocols that support Trace.shock: simulate, dump VCD, check signal names appear.

case_shock_wishbone :: IO ()
case_shock_wishbone = do
  let names = Trace.names (Proxy @(Wishbone System Standard 8 4)) "wb"
      fwd = C.fromList (P.repeat (emptyWishboneM2S @8 @4))
      bwd = C.fromList (P.repeat emptyWishboneS2M)
      ckt = Trace.shock @(Wishbone System Standard 8 4) "wb"
      (bwdOut, fwdOut) = toSignals ckt (fwd, bwd)
      combined = C.bundle (fwdOut, bwdOut)
  result <- Shock.dumpVCD (0, nCycles) combined names
  assertVcdContains result names

case_shock_df :: IO ()
case_shock_df = do
  let names = Trace.names (Proxy @(Df System (Unsigned 8))) "df"
      fwd = C.fromList (P.repeat (Just (1 :: Unsigned 8)))
      bwd = C.fromList (P.repeat (Ack True))
      ckt = Trace.shock @(Df System (Unsigned 8)) "df"
      (bwdOut, fwdOut) = toSignals ckt (fwd, bwd)
      combined = C.bundle (fwdOut, bwdOut)
  result <- Shock.dumpVCD (0, nCycles) combined names
  assertVcdContains result names

case_shock_spi :: IO ()
case_shock_spi = do
  let names = Trace.names (Proxy @(Spi System)) "spi"
      fwd = C.fromList (P.repeat (Spi.M2S False 0 True))
      bwd = C.fromList (P.repeat (Spi.S2M 0))
      ckt = Trace.shock @(Spi System) "spi"
      (bwdOut, fwdOut) = toSignals ckt (fwd, bwd)
      combined = C.bundle (fwdOut, bwdOut)
  result <- Shock.dumpVCD (0, nCycles) combined names
  assertVcdContains result names

case_shock_packetstream :: IO ()
case_shock_packetstream = do
  let names = Trace.names (Proxy @(PacketStream System 1 ())) "ps"
      fwd = C.fromList (P.repeat (Just (PacketStreamM2S (0 :> Nil) Nothing () False)))
      bwd = C.fromList (P.repeat (PacketStreamS2M True))
      ckt = Trace.shock @(PacketStream System 1 ()) "ps"
      (bwdOut, fwdOut) = toSignals ckt (fwd, bwd)
      combined = C.bundle (fwdOut, bwdOut)
  result <- Shock.dumpVCD (0, nCycles) combined names
  assertVcdContains result names

case_shock_axi4stream :: IO ()
case_shock_axi4stream = do
  let names = Trace.names (Proxy @(Axi4Stream System SimpleAxi4StreamConf ())) "as"
      m2s = Axi4StreamM2S (0 :> Nil) (True :> Nil) (True :> Nil) False 0 0 ()
      fwd = C.fromList (P.repeat (Just m2s))
      bwd = C.fromList (P.repeat (Axi4StreamS2M True))
      ckt = Trace.shock @(Axi4Stream System SimpleAxi4StreamConf ()) "as"
      (bwdOut, fwdOut) = toSignals ckt (fwd, bwd)
      combined = C.bundle (fwdOut, bwdOut)
  result <- Shock.dumpVCD (0, nCycles) combined names
  assertVcdContains result names

case_shock_axi4readaddress :: IO ()
case_shock_axi4readaddress = do
  let names = Trace.names (Proxy @(Axi4ReadAddress System SimpleAxi4ReadAddrConf ())) "ar"
      fwd = C.fromList (P.repeat M2S_NoReadAddress)
      bwd = C.fromList (P.repeat (S2M_ReadAddress True))
      ckt = Trace.shock @(Axi4ReadAddress System SimpleAxi4ReadAddrConf ()) "ar"
      (bwdOut, fwdOut) = toSignals ckt (fwd, bwd)
      combined = C.bundle (fwdOut, bwdOut)
  result <- Shock.dumpVCD (0, nCycles) combined names
  assertVcdContains result names

case_shock_axi4writeaddress :: IO ()
case_shock_axi4writeaddress = do
  let names = Trace.names (Proxy @(Axi4WriteAddress System SimpleAxi4WriteAddrConf ())) "aw"
      fwd = C.fromList (P.repeat M2S_NoWriteAddress)
      bwd = C.fromList (P.repeat (S2M_WriteAddress True))
      ckt = Trace.shock @(Axi4WriteAddress System SimpleAxi4WriteAddrConf ()) "aw"
      (bwdOut, fwdOut) = toSignals ckt (fwd, bwd)
      combined = C.bundle (fwdOut, bwdOut)
  result <- Shock.dumpVCD (0, nCycles) combined names
  assertVcdContains result names

case_shock_axi4readdata :: IO ()
case_shock_axi4readdata = do
  let names = Trace.names (Proxy @(Axi4ReadData System SimpleAxi4ReadDataConf () (BitVector 8))) "rd"
      fwd = C.fromList (P.repeat (S2M_NoReadData @SimpleAxi4ReadDataConf @() @(BitVector 8)))
      bwd = C.fromList (P.repeat (M2S_ReadData True))
      ckt = Trace.shock @(Axi4ReadData System SimpleAxi4ReadDataConf () (BitVector 8)) "rd"
      (bwdOut, fwdOut) = toSignals ckt (fwd, bwd)
      combined = C.bundle (fwdOut, bwdOut)
  result <- Shock.dumpVCD (0, nCycles) combined names
  assertVcdContains result names

case_shock_axi4writedata :: IO ()
case_shock_axi4writedata = do
  let names = Trace.names (Proxy @(Axi4WriteData System SimpleAxi4WriteDataConf ())) "wd"
      fwd = C.fromList (P.repeat (M2S_NoWriteData @SimpleAxi4WriteDataConf @()))
      bwd = C.fromList (P.repeat (S2M_WriteData True))
      ckt = Trace.shock @(Axi4WriteData System SimpleAxi4WriteDataConf ()) "wd"
      (bwdOut, fwdOut) = toSignals ckt (fwd, bwd)
      combined = C.bundle (fwdOut, bwdOut)
  result <- Shock.dumpVCD (0, nCycles) combined names
  assertVcdContains result names

case_shock_axi4writeresponse :: IO ()
case_shock_axi4writeresponse = do
  let names = Trace.names (Proxy @(Axi4WriteResponse System SimpleAxi4WriteRespConf ())) "wr"
      fwd = C.fromList (P.repeat (S2M_NoWriteResponse @SimpleAxi4WriteRespConf @()))
      bwd = C.fromList (P.repeat (M2S_WriteResponse True))
      ckt = Trace.shock @(Axi4WriteResponse System SimpleAxi4WriteRespConf ()) "wr"
      (bwdOut, fwdOut) = toSignals ckt (fwd, bwd)
      combined = C.bundle (fwdOut, bwdOut)
  result <- Shock.dumpVCD (0, nCycles) combined names
  assertVcdContains result names

-- ** traceSignal tests

-- Same pattern as Trace.shock but using Clash's built-in VCD dump.

case_traceSignal_wishbone :: IO ()
case_traceSignal_wishbone = do
  let names = Trace.names (Proxy @(Wishbone System Standard 8 4)) "wb_s"
      fwd = C.fromList (P.repeat (emptyWishboneM2S @8 @4))
      bwd = C.fromList (P.repeat emptyWishboneS2M)
      ckt = Trace.signal @(Wishbone System Standard 8 4) "wb_s"
      (bwdOut, fwdOut) = toSignals ckt (fwd, bwd)
      combined = C.bundle (fwdOut, bwdOut)
  result <- Trace.dumpVCD (0, nCycles) combined names
  assertClashVcdContains result names

case_traceSignal_df :: IO ()
case_traceSignal_df = do
  let names = Trace.names (Proxy @(Df System (Unsigned 8))) "df_s"
      fwd = C.fromList (P.repeat (Just (1 :: Unsigned 8)))
      bwd = C.fromList (P.repeat (Ack True))
      ckt = Trace.signal @(Df System (Unsigned 8)) "df_s"
      (bwdOut, fwdOut) = toSignals ckt (fwd, bwd)
      combined = C.bundle (fwdOut, bwdOut)
  result <- Trace.dumpVCD (0, nCycles) combined names
  assertClashVcdContains result names

case_traceSignal_spi :: IO ()
case_traceSignal_spi = do
  let names = Trace.names (Proxy @(Spi System)) "spi_s"
      fwd = C.fromList (P.repeat (Spi.M2S False 0 True))
      bwd = C.fromList (P.repeat (Spi.S2M 0))
      ckt = Trace.signal @(Spi System) "spi_s"
      (bwdOut, fwdOut) = toSignals ckt (fwd, bwd)
      combined = C.bundle (fwdOut, bwdOut)
  result <- Trace.dumpVCD (0, nCycles) combined names
  assertClashVcdContains result names

case_traceSignal_packetstream :: IO ()
case_traceSignal_packetstream = do
  let names = Trace.names (Proxy @(PacketStream System 1 ())) "ps_s"
      fwd = C.fromList (P.repeat (Just (PacketStreamM2S (0 :> Nil) Nothing () False)))
      bwd = C.fromList (P.repeat (PacketStreamS2M True))
      ckt = Trace.signal @(PacketStream System 1 ()) "ps_s"
      (bwdOut, fwdOut) = toSignals ckt (fwd, bwd)
      combined = C.bundle (fwdOut, bwdOut)
  result <- Trace.dumpVCD (0, nCycles) combined names
  assertClashVcdContains result names

case_traceSignal_axi4stream :: IO ()
case_traceSignal_axi4stream = do
  let names = Trace.names (Proxy @(Axi4Stream System SimpleAxi4StreamConf ())) "as_s"
      m2s = Axi4StreamM2S (0 :> Nil) (True :> Nil) (True :> Nil) False 0 0 ()
      fwd = C.fromList (P.repeat (Just m2s))
      bwd = C.fromList (P.repeat (Axi4StreamS2M True))
      ckt = Trace.signal @(Axi4Stream System SimpleAxi4StreamConf ()) "as_s"
      (bwdOut, fwdOut) = toSignals ckt (fwd, bwd)
      combined = C.bundle (fwdOut, bwdOut)
  result <- Trace.dumpVCD (0, nCycles) combined names
  assertClashVcdContains result names

case_traceSignal_axi4readaddress :: IO ()
case_traceSignal_axi4readaddress = do
  let names = Trace.names (Proxy @(Axi4ReadAddress System SimpleAxi4ReadAddrConf ())) "ar_s"
      fwd = C.fromList (P.repeat M2S_NoReadAddress)
      bwd = C.fromList (P.repeat (S2M_ReadAddress True))
      ckt = Trace.signal @(Axi4ReadAddress System SimpleAxi4ReadAddrConf ()) "ar_s"
      (bwdOut, fwdOut) = toSignals ckt (fwd, bwd)
      combined = C.bundle (fwdOut, bwdOut)
  result <- Trace.dumpVCD (0, nCycles) combined names
  assertClashVcdContains result names

case_traceSignal_axi4writeaddress :: IO ()
case_traceSignal_axi4writeaddress = do
  let names = Trace.names (Proxy @(Axi4WriteAddress System SimpleAxi4WriteAddrConf ())) "aw_s"
      fwd = C.fromList (P.repeat M2S_NoWriteAddress)
      bwd = C.fromList (P.repeat (S2M_WriteAddress True))
      ckt = Trace.signal @(Axi4WriteAddress System SimpleAxi4WriteAddrConf ()) "aw_s"
      (bwdOut, fwdOut) = toSignals ckt (fwd, bwd)
      combined = C.bundle (fwdOut, bwdOut)
  result <- Trace.dumpVCD (0, nCycles) combined names
  assertClashVcdContains result names

case_traceSignal_axi4readdata :: IO ()
case_traceSignal_axi4readdata = do
  let names = Trace.names (Proxy @(Axi4ReadData System SimpleAxi4ReadDataConf () (BitVector 8))) "rd_s"
      fwd = C.fromList (P.repeat (S2M_NoReadData @SimpleAxi4ReadDataConf @() @(BitVector 8)))
      bwd = C.fromList (P.repeat (M2S_ReadData True))
      ckt = Trace.signal @(Axi4ReadData System SimpleAxi4ReadDataConf () (BitVector 8)) "rd_s"
      (bwdOut, fwdOut) = toSignals ckt (fwd, bwd)
      combined = C.bundle (fwdOut, bwdOut)
  result <- Trace.dumpVCD (0, nCycles) combined names
  assertClashVcdContains result names

case_traceSignal_axi4writedata :: IO ()
case_traceSignal_axi4writedata = do
  let names = Trace.names (Proxy @(Axi4WriteData System SimpleAxi4WriteDataConf ())) "wd_s"
      fwd = C.fromList (P.repeat (M2S_NoWriteData @SimpleAxi4WriteDataConf @()))
      bwd = C.fromList (P.repeat (S2M_WriteData True))
      ckt = Trace.signal @(Axi4WriteData System SimpleAxi4WriteDataConf ()) "wd_s"
      (bwdOut, fwdOut) = toSignals ckt (fwd, bwd)
      combined = C.bundle (fwdOut, bwdOut)
  result <- Trace.dumpVCD (0, nCycles) combined names
  assertClashVcdContains result names

case_traceSignal_axi4writeresponse :: IO ()
case_traceSignal_axi4writeresponse = do
  let names = Trace.names (Proxy @(Axi4WriteResponse System SimpleAxi4WriteRespConf ())) "wr_s"
      fwd = C.fromList (P.repeat (S2M_NoWriteResponse @SimpleAxi4WriteRespConf @()))
      bwd = C.fromList (P.repeat (M2S_WriteResponse True))
      ckt = Trace.signal @(Axi4WriteResponse System SimpleAxi4WriteRespConf ()) "wr_s"
      (bwdOut, fwdOut) = toSignals ckt (fwd, bwd)
      combined = C.bundle (fwdOut, bwdOut)
  result <- Trace.dumpVCD (0, nCycles) combined names
  assertClashVcdContains result names

-- ** BiDf (2-tuple) tests

case_shock_bidf :: IO ()
case_shock_bidf = do
  let names = Trace.names (Proxy @TestBiDf) "bd"
      fwd =
        ( C.fromList (P.repeat (Just (1 :: Unsigned 8)))
        , C.fromList (P.repeat (Ack True))
        )
      bwd =
        ( C.fromList (P.repeat (Ack True))
        , C.fromList (P.repeat (Just (2 :: Unsigned 8)))
        )
      ckt = Trace.shock @TestBiDf "bd"
      (bwdOut, fwdOut) = toSignals ckt (fwd, bwd)
      combined = C.bundle (C.bundle fwdOut, C.bundle bwdOut)
  result <- Shock.dumpVCD (0, nCycles) combined ["bd_0_dat"]
  assertVcdContains result names

case_traceSignal_bidf :: IO ()
case_traceSignal_bidf = do
  let names = Trace.names (Proxy @TestBiDf) "bd_s"
      fwd =
        ( C.fromList (P.repeat (Just (1 :: Unsigned 8)))
        , C.fromList (P.repeat (Ack True))
        )
      bwd =
        ( C.fromList (P.repeat (Ack True))
        , C.fromList (P.repeat (Just (2 :: Unsigned 8)))
        )
      ckt = Trace.signal @TestBiDf "bd_s"
      (bwdOut, fwdOut) = toSignals ckt (fwd, bwd)
      combined = C.bundle (C.bundle fwdOut, C.bundle bwdOut)
  result <- Trace.dumpVCD (0, nCycles) combined names
  assertClashVcdContains result names

-- ** 3-tuple tests (exercises TH-generated instances)

case_shock_triple :: IO ()
case_shock_triple = do
  let names = Trace.names (Proxy @TestTriple) "t3"
      fwd =
        ( C.fromList (P.repeat (Just (1 :: Unsigned 8)))
        , C.fromList (P.repeat (Spi.M2S False 0 True))
        , C.fromList (P.repeat (Just (PacketStreamM2S (0 :> Nil) Nothing () False)))
        )
      bwd =
        ( C.fromList (P.repeat (Ack True))
        , C.fromList (P.repeat (Spi.S2M 0))
        , C.fromList (P.repeat (PacketStreamS2M True))
        )
      ckt = Trace.shock @TestTriple "t3"
      (bwdOut, fwdOut) = toSignals ckt (fwd, bwd)
      combined = C.bundle (C.bundle fwdOut, C.bundle bwdOut)
  result <- Shock.dumpVCD (0, nCycles) combined names
  assertVcdContains result names

case_traceSignal_triple :: IO ()
case_traceSignal_triple = do
  let names = Trace.names (Proxy @TestTriple) "t3_s"
      fwd =
        ( C.fromList (P.repeat (Just (1 :: Unsigned 8)))
        , C.fromList (P.repeat (Spi.M2S False 0 True))
        , C.fromList (P.repeat (Just (PacketStreamM2S (0 :> Nil) Nothing () False)))
        )
      bwd =
        ( C.fromList (P.repeat (Ack True))
        , C.fromList (P.repeat (Spi.S2M 0))
        , C.fromList (P.repeat (PacketStreamS2M True))
        )
      ckt = Trace.signal @TestTriple "t3_s"
      (bwdOut, fwdOut) = toSignals ckt (fwd, bwd)
      combined = C.bundle (C.bundle fwdOut, C.bundle bwdOut)
  result <- Trace.dumpVCD (0, nCycles) combined names
  assertClashVcdContains result names

-- * Test tree

tests :: TestTree
tests =
  dependentTestGroup
    "Protocols.Extra"
    AllSucceed
    [ dependentTestGroup
        "Trace.names"
        AllSucceed
        [ testCase "Trace.names" case_names
        ]
    , dependentTestGroup
        "Trace.shock"
        AllSucceed
        [ testCase "Wishbone" case_shock_wishbone
        , testCase "Df" case_shock_df
        , testCase "Spi" case_shock_spi
        , testCase "PacketStream" case_shock_packetstream
        , testCase "Axi4Stream" case_shock_axi4stream
        , testCase "Axi4ReadAddress" case_shock_axi4readaddress
        , testCase "Axi4WriteAddress" case_shock_axi4writeaddress
        , testCase "Axi4ReadData" case_shock_axi4readdata
        , testCase "Axi4WriteData" case_shock_axi4writedata
        , testCase "Axi4WriteResponse" case_shock_axi4writeresponse
        , testCase "BiDf" case_shock_bidf
        , testCase "3-tuple" case_shock_triple
        ]
    , dependentTestGroup
        "traceSignal"
        AllSucceed
        [ testCase "Wishbone" case_traceSignal_wishbone
        , testCase "Df" case_traceSignal_df
        , testCase "Spi" case_traceSignal_spi
        , testCase "PacketStream" case_traceSignal_packetstream
        , testCase "Axi4Stream" case_traceSignal_axi4stream
        , testCase "Axi4ReadAddress" case_traceSignal_axi4readaddress
        , testCase "Axi4WriteAddress" case_traceSignal_axi4writeaddress
        , testCase "Axi4ReadData" case_traceSignal_axi4readdata
        , testCase "Axi4WriteData" case_traceSignal_axi4writedata
        , testCase "Axi4WriteResponse" case_traceSignal_axi4writeresponse
        , testCase "BiDf" case_traceSignal_bidf
        , testCase "3-tuple" case_traceSignal_triple
        ]
    ]
