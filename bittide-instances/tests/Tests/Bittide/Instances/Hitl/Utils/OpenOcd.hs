-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
module Tests.Bittide.Instances.Hitl.Utils.OpenOcd where

import Prelude

import Bittide.Instances.Hitl.Utils.OpenOcd (
  TapInfo (..),
  mergeGdbJtag,
  parseGdbPorts,
  parseJtagIds,
 )
import Data.String.Interpolate (i)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.TH

testInput :: [String]
testInput =
  lines
    [i|
Info : set servers polling period to 50ms
Info : clock speed 1000 kHz
Info : JTAG tap: riscv.tap0 tap/device found: 0x2514c001 (mfg: 0x000 (<invalid>), part: 0x514c, ver: 0x2)
Info : JTAG tap: riscv.tap1 tap/device found: 0x1514c001 (mfg: 0x000 (<invalid>), part: 0x514c, ver: 0x1)
Info : JTAG tap: riscv.tap2 tap/device found: 0x0514c001 (mfg: 0x000 (<invalid>), part: 0x514c, ver: 0x0)
Info : [riscv.tap0] datacount=1 progbufsize=2
Info : [riscv.tap0] Disabling abstract command reads from CSRs.
Warn : [riscv.tap0] 'misa' register is read as zero.OpenOCD will not be able to determine some hart's capabilities.
Info : [riscv.tap0] Disabling abstract command writes to CSRs.
Info : [riscv.tap0] Vector support with vlenb=0
Info : [riscv.tap0] S?aia detected with IMSIC
Info : [riscv.tap0] Examined RISC-V core
Info : [riscv.tap0]  XLEN=32, misa=0x0
[riscv.tap0] Target successfully examined.
Info : [riscv.tap0] Examination succeed
Info : [riscv.tap1] datacount=1 progbufsize=2
Info : [riscv.tap1] Disabling abstract command reads from CSRs.
Warn : [riscv.tap1] 'misa' register is read as zero.OpenOCD will not be able to determine some hart's capabilities.
Info : [riscv.tap1] Disabling abstract command writes to CSRs.
Info : [riscv.tap1] Vector support with vlenb=0
Info : [riscv.tap1] S?aia detected with IMSIC
Info : [riscv.tap1] Examined RISC-V core
Info : [riscv.tap1]  XLEN=32, misa=0x0
[riscv.tap1] Target successfully examined.
Info : [riscv.tap1] Examination succeed
Info : [riscv.tap2] datacount=1 progbufsize=2
Info : [riscv.tap2] Disabling abstract command reads from CSRs.
Warn : [riscv.tap2] 'misa' register is read as zero.OpenOCD will not be able to determine some hart's capabilities.
Info : [riscv.tap2] Disabling abstract command writes to CSRs.
Info : [riscv.tap2] Vector support with vlenb=0
Info : [riscv.tap2] S?aia detected with IMSIC
Info : [riscv.tap2] Examined RISC-V core
Info : [riscv.tap2]  XLEN=32, misa=0x0
[riscv.tap2] Target successfully examined.
Info : [riscv.tap2] Examination succeed
Info : [riscv.tap0] starting gdb server on 3333
Info : Listening on port 3333 for gdb connections
Info : [riscv.tap1] starting gdb server on 3334
Info : Listening on port 3334 for gdb connections
Info : [riscv.tap2] starting gdb server on 3335
Info : Listening on port 3335 for gdb connections
JTAG_ID: riscv.tap0: 0x2514C001
JTAG_ID: riscv.tap1: 0x1514C001
JTAG_ID: riscv.tap2: 0x0514C001
|]

-- Test case: parse testInput successfully
case_parse_gdb_ports_success :: Assertion
case_parse_gdb_ports_success =
  case parseGdbPorts testInput of
    Left err -> assertFailure $ "Unexpected parse error: " ++ err
    Right result -> do
      assertEqual "Should have 3 entries" 3 (length result)
      assertEqual "First entry" (0, 3333) (result !! 0)
      assertEqual "Second entry" (1, 3334) (result !! 1)
      assertEqual "Third entry" (2, 3335) (result !! 2)

-- Test case: parse empty list
case_parse_gdb_ports_empty :: Assertion
case_parse_gdb_ports_empty =
  case parseGdbPorts [] of
    Left err -> assertFailure $ "Should not error on empty input: " ++ err
    Right result -> assertEqual "Should return empty list" [] result

-- Test case: parse error on malformed tap number
case_parse_gdb_ports_bad_tap :: Assertion
case_parse_gdb_ports_bad_tap =
  case parseGdbPorts ["Info : [riscv.tapX] starting gdb server on 3333"] of
    Left _ -> return () -- Expected to fail
    Right _ -> assertFailure "Should have failed on non-numeric tap number"

-- Test case: parse error on malformed port number
case_parse_gdb_ports_bad_port :: Assertion
case_parse_gdb_ports_bad_port =
  case parseGdbPorts ["Info : [riscv.tap0] starting gdb server on ABCD"] of
    Left _ -> return () -- Expected to fail
    Right _ -> assertFailure "Should have failed on non-numeric port number"

-- Test case: parse JTAG IDs successfully
case_parse_jtag_ids_success :: Assertion
case_parse_jtag_ids_success =
  case parseJtagIds testInput of
    Left err -> assertFailure $ "Unexpected parse error: " ++ err
    Right result -> do
      assertEqual "Should have 3 entries" 3 (length result)
      assertEqual "First entry" (0, 0x2514C001) (result !! 0)
      assertEqual "Second entry" (1, 0x1514C001) (result !! 1)
      assertEqual "Third entry" (2, 0x0514C001) (result !! 2)

-- Test case: parse empty list
case_parse_jtag_ids_empty :: Assertion
case_parse_jtag_ids_empty =
  case parseJtagIds [] of
    Left err -> assertFailure $ "Should not error on empty input: " ++ err
    Right result -> assertEqual "Should return empty list" [] result

-- Test case: parse error on malformed tap number
case_parse_jtag_ids_bad_tap :: Assertion
case_parse_jtag_ids_bad_tap =
  case parseJtagIds ["JTAG_ID: riscv.tapX: 0x2514C001"] of
    Left _ -> return () -- Expected to fail
    Right _ -> assertFailure "Should have failed on non-numeric tap number"

-- Test case: parse error on missing JTAG ID
case_parse_jtag_ids_no_id :: Assertion
case_parse_jtag_ids_no_id =
  case parseJtagIds ["JTAG_ID: riscv.tap0:"] of
    Left _ -> return () -- Expected to fail
    Right _ -> assertFailure "Should have failed on missing JTAG ID"

-- Test case: merge GDB ports and JTAG IDs successfully
case_merge_gdb_jtag_success :: Assertion
case_merge_gdb_jtag_success =
  case (parseGdbPorts testInput, parseJtagIds testInput) of
    (Right gdbPorts, Right jtagIds) ->
      case mergeGdbJtag gdbPorts jtagIds of
        Left err -> assertFailure $ "Unexpected merge error: " ++ err
        Right result -> do
          assertEqual "Should have 3 entries" 3 (length result)
          let TapInfo{jtagId = jtagId0, tapId = tapId0, gdbPort = port0} = result !! 0
          let TapInfo{jtagId = jtagId1, tapId = tapId1, gdbPort = port1} = result !! 1
          let TapInfo{jtagId = jtagId2, tapId = tapId2, gdbPort = port2} = result !! 2
          assertEqual "First JTAG ID" 0x0514C001 jtagId0
          assertEqual "First tap ID" 2 tapId0
          assertEqual "First port" 3335 port0
          assertEqual "Second JTAG ID" 0x1514C001 jtagId1
          assertEqual "Second tap ID" 1 tapId1
          assertEqual "Second port" 3334 port1
          assertEqual "Third JTAG ID" 0x2514C001 jtagId2
          assertEqual "Third tap ID" 0 tapId2
          assertEqual "Third port" 3333 port2
    (Left err, _) -> assertFailure $ "GDB parse error: " ++ err
    (_, Left err) -> assertFailure $ "JTAG parse error: " ++ err

-- Test case: merge with mismatched tap numbers
case_merge_gdb_jtag_mismatch :: Assertion
case_merge_gdb_jtag_mismatch =
  let gdbPorts = [(0, 3333), (1, 3334)]
      jtagIds = [(0, 0x2514C001), (2, 0x0514C001)] -- tap 2 instead of 1
   in case mergeGdbJtag gdbPorts jtagIds of
        Left _ -> return () -- Expected to fail
        Right _ -> assertFailure "Should have failed on mismatched tap numbers"

-- Test case: merge with different number of entries
case_merge_gdb_jtag_different_lengths :: Assertion
case_merge_gdb_jtag_different_lengths =
  let gdbPorts = [(0, 3333), (1, 3334), (2, 3335)]
      jtagIds = [(0, 0x2514C001), (1, 0x1514C001)] -- Missing tap 2
   in case mergeGdbJtag gdbPorts jtagIds of
        Left _ -> return () -- Expected to fail
        Right _ -> assertFailure "Should have failed on different list lengths"

-- Test case: merge with invalid hex in JTAG ID
case_merge_gdb_jtag_bad_hex :: Assertion
case_merge_gdb_jtag_bad_hex =
  case parseJtagIds ["JTAG_ID: riscv.tap0: 0xGGGGGGGG"] of
    Left _ -> return () -- Expected to fail during parsing
    Right _ -> assertFailure "Should have failed on invalid hex JTAG ID"

tests :: TestTree
tests = $(testGroupGenerator)
