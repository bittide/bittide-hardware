-- SPDX-FileCopyrightText: 2023 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module VexRiscv.JtagTcpBridge where

import Clash.Prelude

import Clash.Signal.Internal
import Network.Socket
import Protocols
import Protocols.Internal (CSignal(..))
import VexRiscv (JtagIn(..), JtagOut(..), Jtag)
import Control.Concurrent (MVar, forkIO, newEmptyMVar, putMVar, takeMVar, tryTakeMVar)
import Control.Monad (when)
import Network.Socket.ByteString (sendAll, recv)

import qualified Data.ByteString as BS
import System.IO.Unsafe (unsafePerformIO)

data NetworkThreadToMainMsg
  = Connected
  | Disconnected
  | DataReceived [BitVector 8]

data MainToNetworkThreadMsg
  = ReadMore
  | Send (BitVector 8)

data NetworkThreadState
  = NoClient
  | PerformRead Socket
  | WaitForNextRead Socket
  deriving (Show)

data MainThreadState
  = MDisconnected
  | MWaitForRead
  | MProcessing (BitVector 2) [BitVector 8]
  deriving (Show)

jtagTcpBridge ::
  (HiddenClockResetEnable dom) =>
  PortNumber ->
  Circuit
    (Jtag dom)
    (CSignal dom Bit)
jtagTcpBridge port = 
  Circuit $ \(jtagOut, _) -> unsafePerformIO $ do
    (unbundle -> (jtagIn, debugReset)) <- jtagTcpBridge' port hasReset jtagOut
    pure (jtagIn, CSignal debugReset)

jtagTcpBridge' ::
  (KnownDomain dom) =>
  PortNumber ->
  Reset dom ->
  Signal dom JtagOut ->
  IO (Signal dom (JtagIn, Bit))
  -- ^ JTAG and debugReset
jtagTcpBridge' port rst jtagOut = do

  {-
  let resets = boolToBit <$> unsafeToHighPolarity rst
  pure $ bundle (pure defaultIn, resets)
  -}

  -- {-
  (n2m, m2n) <- server port

  let resets = boolToBit <$> unsafeToHighPolarity rst
  
  jtagIn <- client n2m m2n MDisconnected jtagOut

  pure $ bundle (jtagIn, resets)
  -- -}
{-# NOINLINE jtagTcpBridge' #-}

server :: PortNumber -> IO (MVar NetworkThreadToMainMsg, MVar MainToNetworkThreadMsg)
server port = withSocketsDo $ do
  sock <- setup
  
  threadToMainChan <- newEmptyMVar
  mainToThreadChan <- newEmptyMVar
    
  let
      thread NoClient = do
        (clientSock, _) <- accept sock
        putMVar threadToMainChan Connected
        thread (PerformRead clientSock)
      thread (PerformRead clientSock) = do
        buf <- recv clientSock 100
        if BS.null buf then do
          putMVar threadToMainChan Disconnected
          thread NoClient
        else do
          let dat = pack <$> BS.unpack buf
          putMVar threadToMainChan (DataReceived dat)
          thread (WaitForNextRead clientSock)
      
      thread (WaitForNextRead clientSock) = do
        msg <- takeMVar mainToThreadChan
        case msg of
          ReadMore -> thread (PerformRead clientSock)
          Send byte -> do
            sendAll clientSock (BS.singleton $ unpack byte)
            thread (WaitForNextRead clientSock)

  _ <- forkIO $ thread NoClient
      
  pure (threadToMainChan, mainToThreadChan)

  where
    setup = do
      sock <- socket AF_INET Stream 0

      setSocketOption sock NoDelay 0

      bind sock (SockAddrInet port (tupleToHostAddress (127, 0, 0, 1)))

      listen sock 1

      pure sock

defaultIn :: JtagIn
defaultIn = JtagIn { testModeSelect = low, testDataIn = low, testClock = low }

dbg :: Show a => a -> a
dbg x = 
  -- trace (show x)
  x

clientSleep :: BitVector 2
clientSleep = 4

client ::
  (KnownDomain dom) =>
  MVar NetworkThreadToMainMsg ->
  MVar MainToNetworkThreadMsg ->
  MainThreadState ->
  Signal dom JtagOut ->
  IO (Signal dom JtagIn)
client n2m m2n MDisconnected (_out :- outs) = do
  msg <- tryTakeMVar n2m
  case msg of
    Nothing ->
      pure $ _out `deepseqX` defaultIn :- unsafePerformIO (client n2m m2n MDisconnected outs)
    Just Connected -> do
      pure $ _out `deepseqX` defaultIn :- unsafePerformIO (client n2m m2n MWaitForRead outs)
    Just Disconnected -> do
      errorX "????"
    Just (DataReceived _xs) -> do
      errorX "????"
  
client n2m m2n MWaitForRead (out :- outs) = do
  msg <- tryTakeMVar n2m
  case msg of
    Nothing ->
      pure $ out `deepseqX` defaultIn :- unsafePerformIO (client n2m m2n MWaitForRead outs)
    Just Disconnected ->
      pure $ out `deepseqX` defaultIn :- unsafePerformIO (client n2m m2n MDisconnected outs)
    Just (DataReceived xs) ->
      client n2m m2n (MProcessing 0 xs) (out :- outs)
    Just Connected ->
      errorX "????"

client n2m m2n (MProcessing _ []) (out :- outs) = do
  putMVar m2n ReadMore
  pure $ out `deepseqX` defaultIn :- unsafePerformIO (client n2m m2n MWaitForRead outs)
client n2m m2n (MProcessing 0 (x:xs)) (out :- outs) = do
  let tms = x ! (0 :: Int)
      tdi = x ! (1 :: Int)
      tck = x ! (3 :: Int)

      sendTdo = bitToBool $ x ! (2 :: Int)
  
  when sendTdo $ do
    putMVar m2n $ Send $ boolToBV (bitToBool $ testDataOut out)
  
  let inDat = JtagIn { testModeSelect = tms, testDataIn = tdi, testClock = tck }
  pure $ inDat :- unsafePerformIO (client n2m m2n (MProcessing clientSleep xs) outs)
client n2m m2n (MProcessing n xs) (out :- outs) = do
  pure $ out `deepseqX` defaultIn :- unsafePerformIO (client n2m m2n (MProcessing (n - 1) xs) outs)


{-# NOINLINE client #-}