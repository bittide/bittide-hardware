-- SPDX-FileCopyrightText: 2026 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
module Clash.Class.Cdc.Handshake where

import Clash.Explicit.Prelude

import Bittide.Extra.Maybe (toMaybe)
import Data.Maybe (fromMaybe, isJust, isNothing)
import GHC.Stack (HasCallStack)
import Protocols

import qualified Clash.Class.Cdc as Cdc

-- | `Maybe a`-based version of `handshake`.
handshakeMaybe ::
  forall vendor a src dst.
  ( HasCallStack
  , KnownDomain src
  , KnownDomain dst
  , BitPack a
  , NFDataX a
  , Cdc.HiddenVendor vendor
  , Cdc.ValidHandshake vendor a src dst
  ) =>
  Clock src ->
  Clock dst ->
  Signal src (Maybe a) ->
  Signal dst Bool ->
  (Signal src Bool, Signal dst (Maybe a))
handshakeMaybe clkSrc clkDst srcIn dstAck = (srcRcv, toMaybe <$> dstReq <*> dstOut)
 where
  (dstOut, dstReq, srcRcv) =
    Cdc.handshake @vendor clkSrc clkDst (fromMaybe (unpack 0) <$> srcIn) (isJust <$> srcIn) dstAck

{- | Reliable CDC component based on 'handshakeMaybe' without backpressure and with limited
throughput. Data will be lost if the `src` domain provides more inputs than the circuit can handle.
Useful for low granularity synchronization.
-}
maybeLossy ::
  forall vendor a src dst.
  ( HasCallStack
  , KnownDomain src
  , KnownDomain dst
  , BitPack a
  , NFDataX a
  , Cdc.HiddenVendor vendor
  , Cdc.ValidHandshake vendor a src dst
  ) =>
  Clock src ->
  Clock dst ->
  Signal src (Maybe a) ->
  Signal dst (Maybe a)
maybeLossy clkSrc clkDst maybeInp =
  mux (isRising clkDst noReset enableGen False dstAck) dstOut (pure Nothing)
 where
  srcReg =
    regEn
      clkSrc
      noReset
      enableGen
      Nothing
      (srcRcv .||. srcRegEmpty)
      $ mux (srcRegEmpty .&&. fmap not srcRcv) maybeInp (pure Nothing)

  srcRegEmpty = isNothing <$> srcReg
  (srcRcv, dstOut) = handshakeMaybe clkSrc clkDst srcReg (isJust <$> dstOut)
  dstAck = isJust <$> dstOut

data SafeHandshakeState
  = InReset
  | WaitForRising
  | WaitForFalling
  deriving (Generic, NFDataX)

-- | State machine for the source side of a safe handshake, see 'safeXpmCdcHandshake'.
safeHandshakeSrcFsm ::
  forall a dom.
  (NFDataX a, KnownDomain dom) =>
  Clock dom ->
  Reset dom ->
  Signal dom (Maybe a) ->
  "src_rcv" ::: Signal dom Bool ->
  ( "src_send" ::: Signal dom Bool
  , "src_in" ::: Signal dom a
  , Signal dom Ack
  )
safeHandshakeSrcFsm clk rst ins srcRcvs =
  mealyB clk rst enableGen go InReset (ins, srcRcvs)
 where
  go :: SafeHandshakeState -> (Maybe a, Bool) -> (SafeHandshakeState, (Bool, a, Ack))
  go InReset _ = (WaitForRising, noInput nack)
  go WaitForRising (Nothing, _) = (WaitForRising, noInput nack)
  go s@WaitForRising (Just a, rcv) = (if rcv then WaitForFalling else s, input a (Ack rcv))
  go s@WaitForFalling (_, rcv) = (if rcv then s else WaitForRising, noInput nack)

  noInput :: Ack -> (Bool, a, Ack)
  noInput acknowledge = (False, deepErrorX "no input", acknowledge)

  input :: a -> Ack -> (Bool, a, Ack)
  input a acknowledge = (True, a, acknowledge)

  nack :: Ack
  nack = Ack False

{- | State machine for the destination side of a safe handshake, see
'safeXpmCdcHandshake'.
-}
safeHandshakeDstFsm ::
  (KnownDomain dom) =>
  Clock dom ->
  Reset dom ->
  Signal dom Ack ->
  "dest_out" ::: Signal dom a ->
  "dest_req" ::: Signal dom Bool ->
  ( "dest_ack" ::: Signal dom Bool
  , Signal dom (Maybe a)
  )
safeHandshakeDstFsm clk rst acks outs reqs =
  mealyB clk rst enableGen go InReset (acks, outs, reqs)
 where
  go :: SafeHandshakeState -> (Ack, a, Bool) -> (SafeHandshakeState, (Bool, Maybe a))
  go InReset _ = (WaitForRising, (False, Nothing))
  go s@WaitForRising (~(Ack acknowledge), a, request)
    | request = (if acknowledge then WaitForFalling else s, (acknowledge, Just a))
    | otherwise = (s, (False, Nothing))
  go s@WaitForFalling (_, _, request)
    | request = (s, (True, Nothing))
    | otherwise = (WaitForRising, (False, Nothing))

{- | A wrapper around 'xpmCdcHandshake' that implements a 'Df'-like interface:

  * If a 'Just' is given as an argument it should remain stable until an Ack is
    returned. This means that it can neither transition to 'Nothing', nor can the
    contents inside of the 'Just' change while @'Ack' 'False'@ is returned.

  * The input value (@'Maybe' a@) cannot depend combinationally on the returned
    'Ack'.

This implements the state machine necessary to safely do a handshake, as described in
the Xilinx documentation.

If either side is reset, the other should be too. Though it doesn't matter in which
order the resets are deasserted, the resets should be asserted for long enough for the
handshake pipeline to flush. See the Xilinx documentation for more information.
-}
safeHandshake ::
  forall vendor a src dst.
  ( HasCallStack
  , KnownDomain src
  , KnownDomain dst
  , BitPack a
  , NFDataX a
  , Cdc.HiddenVendor vendor
  , Cdc.ValidHandshake vendor a src dst
  ) =>
  Clock src ->
  Reset src ->
  Clock dst ->
  Reset dst ->
  Signal src (Maybe a) ->
  Signal dst Ack ->
  ( Signal src Ack
  , Signal dst (Maybe a)
  )
safeHandshake clkSrc rstSrc clkDst rstDst srcData dstAck = (srcAck, dstData)
 where
  (srcSend, srcIn, srcAck) = safeHandshakeSrcFsm clkSrc rstSrc srcData srcRcv
  (destAck, dstData) = safeHandshakeDstFsm clkDst rstDst dstAck destOut destReq
  (destOut, destReq, srcRcv) = Cdc.handshake @vendor clkSrc clkDst srcIn srcSend destAck
