-- SPDX-FileCopyrightText: 2022-2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE CPP #-}
-- {-# OPTIONS -fplugin-opt=Protocols.Plugin:debug #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fplugin Protocols.Plugin #-}

import Clash.Prelude

import Control.Monad (forM_)
import Internal.HdlTest.UartMock (someCircuit)
import Protocols.MemoryMap
import Text.Printf (printf)

import qualified Data.Aeson as Ae
import qualified Data.ByteString.Lazy as BS
import Protocols.MemoryMap.Check (
  CheckConfiguration (..),
  MemoryMapValid (..),
  MemoryMapValidationErrors (..),
  check,
  checkCircuitTH,
 )
import Protocols.MemoryMap.Check.AbsAddress (AbsAddressValidateError (..))
import Protocols.MemoryMap.Check.Overlap (OverlapError (..))
import Protocols.MemoryMap.Json (memoryMapJson)
import System.Exit (exitFailure)

checkConfig :: CheckConfiguration
checkConfig = CheckConfiguration{startAddr = 0x0000_0000, endAddr = 0xFFFF_FFFF}

checkCircuitTH @System
  (CheckConfiguration{startAddr = 0x0000_0000, endAddr = 0xFFFF_FFFF})
  someCircuit

main :: IO ()
main = do
  let
    SimOnly ann = annotation @System someCircuit
  -- ann' = makeAbsolute 0x0 ann
  -- validation = validateAbsAddresses Root ann' ann

  mmValid@MemoryMapValid{..} <- case check checkConfig ann of
    Left MemoryMapValidationErrors{..} -> do
      forM_ absAddrErrors $ \AbsAddressValidateError{..} -> do
        let path' = prettyPrintPath path
        let component = case componentName of
              Just name -> name
              Nothing -> "interconnect " <> path'
        printf "Expected component %s at %08X but found %08X\n" component expected got

      forM_ overlapErrors $ \case
        OverlapError{..} -> do
          printf
            "Component %s (%08X + %08X) overlaps with %s at address (%08X)"
            (prettyPrintPath path)
            startAddr
            componentSize
            (prettyPrintPath overlapsWith)
            overlapsAt
        SizeExceedsError{..} -> do
          printf
            "Component %s (%08X + %08X) exceeds available size %08X"
            (prettyPrintPath path)
            startAddr
            requestedSize
            availableSize

      exitFailure
    Right res -> pure res

  print validTypes

  putStrLn "\n"

  let json = memoryMapJson mmValid
  BS.putStr (Ae.encode json)

prettyPrintPath :: ComponentPath -> String
prettyPrintPath Root = "root"
prettyPrintPath (InterconnectComponent idx path) = prettyPrintPath path <> "." <> show idx
