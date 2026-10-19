-- SPDX-FileCopyrightText: 2026 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE MultiWayIf #-}

{- | Driver for 'Bittide.Instances.Hitl.Transceivers.transceiversUpTest'. Does
the same as the default driver, but also prints the status of every link
while the test runs, so a failing run shows which links did not come up.
-}
module Bittide.Instances.Hitl.Driver.Transceivers where

import Prelude

import Bittide.Hitl
import Bittide.Instances.Hitl.Setup (knownLinkConfigs)
import Bittide.Instances.Hitl.Utils.Driver (assertProbe, getTargetIndex)
import Clash.Prelude (toList)
import Control.Concurrent (threadDelay)
import Control.Monad (forM, forM_, unless, when)
import Control.Monad.IO.Class (liftIO)
import Data.Bits (shiftR, testBit, (.&.))
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Numeric (readHex)
import System.Exit
import Vivado.Tcl (HwTarget)
import Vivado.VivadoM

import qualified Data.List as L

-- | Names of the link ports, in the order of 'knownLinkConfigs'
portNames :: [String]
portNames = ["SFP0", "SFP1", "J4", "J5", "J6", "J7", "SMA"]

data LinkStatus = LinkStatus
  { rxUp :: Bool
  , txUp :: Bool
  , anyError :: Bool
  , failAfterUps :: Integer
  , rxFullRetries :: Integer
  , txRetries :: Integer
  }
  deriving (Eq, Show)

data NodeStatus = NodeStatus
  { done :: Bool
  , success :: Bool
  , links :: [LinkStatus]
  }
  deriving (Eq, Show)

parseHex :: String -> Integer
parseHex s = case readHex s of
  [(n, "")] -> n
  _ -> error $ "Could not parse VIO value: " <> show s

readNodeStatus :: HwTarget -> VivadoM NodeStatus
readNodeStatus hwT = do
  openHardwareTarget hwT
  hitl <- readVio "vioHitlt" ["probe_test_done", "probe_test_success"]
  let probes =
        [ "probe_rx_up"
        , "probe_tx_up"
        , "probe_errors"
        , "probe_fail_after_ups"
        , "probe_rx_full_retries"
        , "probe_tx_retries"
        ]
  forM_ probes $ \p ->
    setProp
      ("[get_hw_probes -of_objects [get_hw_vios] *vioLinkStatus/" <> p <> "]")
      "INPUT_VALUE_RADIX"
      "HEX"
  vals <- fmap (parseHex . snd) <$> readVio "vioLinkStatus" probes
  case (fmap snd hitl, vals) of
    ([done, success], [rx, tx, err, fails, rxRetries, txRetries]) ->
      pure
        NodeStatus
          { done = done == "1"
          , success = success == "1"
          , links =
              [ LinkStatus
                  { rxUp = testBit rx i
                  , txUp = testBit tx i
                  , anyError = testBit err i
                  , failAfterUps = counter fails i
                  , rxFullRetries = counter rxRetries i
                  , txRetries = counter txRetries i
                  }
              | i <- [0 .. length portNames - 1]
              ]
          }
    _ -> error $ "Unexpected VIO values: " <> show (hitl, vals)
 where
  counter n i = (n `shiftR` (32 * i)) .&. 0xFFFF_FFFF

showNode :: Int -> NodeStatus -> [String]
showNode node s =
  ( "node "
      <> show node
      <> ": done="
      <> showBool s.done
      <> " success="
      <> showBool s.success
  )
    : L.zipWith3 showLink portNames peers s.links
 where
  peers = toList (toList <$> knownLinkConfigs) L.!! node
  showLink port peer l =
    "  "
      <> port
      <> " -> node "
      <> show peer
      <> ": rx_up="
      <> showBool l.rxUp
      <> " tx_up="
      <> showBool l.txUp
      <> " error="
      <> showBool l.anyError
      <> " fail_after_ups="
      <> show l.failAfterUps
      <> " rx_full_retries="
      <> show l.rxFullRetries
      <> " tx_retries="
      <> show l.txRetries
  showBool b = if b then "1" else "0"

-- | Links that are not up, or have seen errors
problems :: Int -> NodeStatus -> [String]
problems node s =
  [ "node " <> show node <> " " <> port <> " -> node " <> show peer <> ": " <> L.intercalate ", " issues
  | (port, peer, l) <- L.zip3 portNames peers s.links
  , let issues =
          ["rx down" | not l.rxUp]
            <> ["tx down" | not l.txUp]
            <> ["data errors" | l.anyError]
            <> ["went down " <> show l.failAfterUps <> " times after up" | l.failAfterUps > 0]
  , not (null issues)
  ]
 where
  peers = toList (toList <$> knownLinkConfigs) L.!! node

driver :: String -> [(HwTarget, DeviceInfo)] -> VivadoM ExitCode
driver _name targets = do
  forM_ targets (assertProbe "probe_test_start")
  start <- liftIO getCurrentTime

  let
    nodes = getTargetIndex . fst <$> targets

    poll previous = do
      liftIO $ threadDelay 5_000_000
      now <- liftIO getCurrentTime
      statuses <- forM targets (readNodeStatus . fst)
      let elapsed = realToFrac (diffUTCTime now start) :: Double
      when (Just statuses /= previous) $ liftIO $ do
        putStrLn $ "Link status after " <> show (round elapsed :: Int) <> " s:"
        forM_ (L.zip nodes statuses) $ \(node, s) -> mapM_ putStrLn (showNode node s)
      if all (.done) statuses || elapsed > 70
        then pure statuses
        else poll (Just statuses)

  statuses <- poll Nothing

  liftIO $ do
    putStrLn "Summary:"
    forM_ (L.zip nodes statuses) $ \(node, s) ->
      putStrLn $
        "node "
          <> show node
          <> ": "
          <> if
            | not s.done -> "timeout"
            | s.success -> "passed"
            | otherwise -> "failed"
    let allProblems = L.concat (L.zipWith problems nodes statuses)
    unless (null allProblems) $ do
      putStrLn "Links with problems:"
      mapM_ (putStrLn . ("  " <>)) allProblems

  pure $
    if all (\s -> s.done && s.success) statuses
      then ExitSuccess
      else ExitFailure 1
