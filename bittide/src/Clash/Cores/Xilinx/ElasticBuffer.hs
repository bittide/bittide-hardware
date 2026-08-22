-- SPDX-FileCopyrightText: 2026 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
module Clash.Cores.Xilinx.ElasticBuffer where

import Clash.Explicit.Prelude

import Bittide.ElasticBuffer
import Clash.Cores.Xilinx.DcFifo
import GHC.Stack (HasCallStack)
import Protocols

toElasticBufferData :: Bool -> Bool -> a -> ElasticBufferData a
toElasticBufferData requestedReadInPreviousCycle underflow a
  | not requestedReadInPreviousCycle = FillCycle
  | underflow = Empty
  | otherwise = Data a

type XilinxEBConstraints n = (4 <= n, n <= 17)

xilinxElasticBuffer ::
  forall n readDom writeDom a.
  (HasCallStack, XilinxEBConstraints n) =>
  Clock readDom ->
  Clock writeDom ->
  DcFifoC n readDom writeDom a
xilinxElasticBuffer clkRead clkWrite = Circuit go
 where
  go (input, _) = (dcFifoOutput, ())
   where
    dcFifoOutput =
      DcFifoOutput
        { dataCount = readCount
        , underflow = isUnderflow
        , overflow = isOverflow
        , fifoOut = fifoOut
        }

    FifoOut{readCount, isUnderflow, isOverflow, fifoData} =
      dcFifo @n @a @writeDom @readDom
        (defConfig @n){dcOverflow = True, dcUnderflow = True}
        clkWrite
        (unsafeFromActiveHigh (pure False))
        clkRead
        (unsafeFromActiveHigh (pure False))
        input.writeData
        input.readEnable

    readEnableDelayed =
      register clkRead (unsafeFromActiveHigh (pure False)) enableGen False input.readEnable
    fifoOut = toElasticBufferData <$> readEnableDelayed <*> isUnderflow <*> fifoData
