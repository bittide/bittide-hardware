-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE OverloadedStrings #-}

module Bittide.Instances.Hitl.Post.TcpServer where

import System.IO
import Prelude

import qualified Network.Simple.TCP as NS

withServer :: ((NS.Socket, NS.SockAddr) -> IO a) -> IO a
withServer = NS.listen NS.HostAny "1234"

waitForClients :: Int -> NS.Socket -> IO [(NS.Socket, NS.SockAddr)]
waitForClients numberOfClients serverSock = do
  mapM
    ( \i ->
        NS.accept
          serverSock
          ( \(clientSock, clientAddr) -> do
              putStrLn $ show i <> " | Connection established from: " ++ show clientAddr
              pure (clientSock, clientAddr)
          )
    )
    [1 .. numberOfClients]
