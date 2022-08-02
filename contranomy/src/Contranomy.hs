-- SPDX-FileCopyrightText: 2022 Google LLC
-- SPDX-FileCopyrightText: 2020 Christiaan Baaij
--
-- SPDX-License-Identifier: Apache-2.0

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards    #-}

module Contranomy where

import           Clash.Annotations.TH
import           Clash.Prelude

import           Data.IntMap                 (IntMap)
import qualified Data.List                   as L

import           Bittide.Extra.Wishbone
import           Contranomy.Core
import           Contranomy.Core.SharedTypes
import           Contranomy.RVFI
import           Contranomy.RegisterFile

createDomain vXilinxSystem{vName="Core", vPeriod=hzToPeriod 100e6}

-- | Contranomy RV32I core
contranomy ::
  "" ::: BitVector 32 ->
  "clk" ::: Clock Core ->
  "reset" ::: Reset Core ->
  ( "" ::: Signal Core CoreIn) ->
  ( "" ::: Signal Core CoreOut)
contranomy entry clk rst coreIn = withClockResetEnable clk rst enableGen $
  let (coreResult,regWrite,_) = core entry (coreIn,regOut)
      regOut = registerFile regWrite
   in coreResult

contranomyTE ::
  "clk" ::: Clock Core ->
  "reset" ::: Reset Core ->
  ( "" ::: Signal Core CoreIn) ->
  ( "" ::: Signal Core CoreOut)
contranomyTE = contranomy 0
makeTopEntity 'contranomyTE


-- | Contranomy RV32I core with RVFI interface
contranomyRVFI ::
  "" ::: BitVector 32 ->
  "clk" ::: Clock Core ->
  "reset" ::: Reset Core ->
  ( "" ::: Signal Core CoreIn) ->
  ( "" ::: Signal Core CoreOut
  , "" ::: Signal Core RVFI)
contranomyRVFI entry clk rst coreIn = withClockResetEnable clk rst enableGen $
  let (coreResult,regWrite,rvfiOut) = core entry (coreIn,regOut)
      regOut = registerFile regWrite
   in (coreResult,rvfiOut)

contranomyRVFITE ::
  "clk" ::: Clock Core ->
  "reset" ::: Reset Core ->
  ( "" ::: Signal Core CoreIn) ->
  ( "" ::: Signal Core CoreOut
  , "" ::: Signal Core RVFI)
contranomyRVFITE = contranomyRVFI 0
makeTopEntity 'contranomyRVFITE

contranomy'
  :: Clock Core
  -> Reset Core
  -> BitVector 32
  -> IntMap (BitVector 8)
  -> IntMap (BitVector 8)
  -> Signal Core (Bool, Bool, BitVector 32)
  -> Signal Core (Maybe (Unsigned 32, Signed 32), Maybe (Unsigned 32, Signed 32))
contranomy' clk rst entry iMem dMem (unbundle -> (tI, sI, eI)) =
  bundle (iWritten, dWritten)
 where
  tupToCoreIn (timerInterrupt, softwareInterrupt, externalInterrupt, iBusS2M, dBusS2M) =
    CoreIn {..}

  coreIn = tupToCoreIn <$> bundle (tI, sI, eI, iMemS2M, dMemS2M)
  coreOut1 = contranomy entry clk rst coreIn

  instructionM2S = iBusM2S <$> coreOut1
  dataM2S = dBusM2S <$> coreOut1
  iMemS2M = wishboneStorage "Instruction storage" iMem instructionM2S
  dMemS2M = wishboneStorage "Data storage" dMem dataM2S

  iWritten = checkWritten <$> instructionM2S <*> iMemS2M
  dWritten = checkWritten <$> dataM2S <*> dMemS2M


  checkWritten
    :: WishboneM2S Bytes AddressWidth
    -> WishboneS2M nBytes
    -> Maybe (Unsigned 32, Signed 32)
  checkWritten busM busS =
    if writeEnable busM && strobe busM && acknowledge busS
    then Just (unpack (addr busM), unpack (writeData busM))
    else Nothing

bytesToWords :: [BitVector 8] -> [BitVector 32]
bytesToWords (a:b:c:d:es) = (a ++# b ++# c ++# d) : bytesToWords es
bytesToWords [] = []
bytesToWords l  = error ("Length of given list not divisible by four: " <> show l)

wordToBytes :: BitVector 32 -> [BitVector 8]
wordToBytes bv =
  [ slice d7  d0  bv
  , slice d15 d8  bv
  , slice d23 d16 bv
  , slice d31 d24 bv ]

wordsToBytes :: [BitVector 32] -> [BitVector 8]
wordsToBytes = L.concatMap wordToBytes
