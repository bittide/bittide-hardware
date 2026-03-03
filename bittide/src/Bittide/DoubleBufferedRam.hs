-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE RecordWildCards #-}

module Bittide.DoubleBufferedRam where

import Clash.Prelude

import Protocols (Circuit, toSignals, (<|))
import Protocols.Wishbone

import Bittide.Extra.Maybe
import Bittide.SharedTypes hiding (delayControls)
import Clash.Class.BitPackC (ByteOrder (..))
import Data.Typeable
import GHC.Stack (HasCallStack)
import Protocols.MemoryMap (unMemmap)
import Protocols.MemoryMap.Registers.WishboneStandard (
  addressableBytesWb,
  deviceWb,
  matchEndianness,
  registerConfig,
 )

import qualified Protocols.ReqResp as ReqResp

data ContentType n a
  = Vec (Vec n a)
  | Blob (MemBlob n (BitSize a))
  | File FilePath

instance (Show a, KnownNat n, Typeable a) => Show (ContentType n a) where
  show = \case
    (Vec _) -> "Vec: " <> nAnda
    (Blob _) -> "Blob: " <> nAnda
    (File fp) -> "File: " <> nAnda <> ", filepath = " <> fp
   where
    nAnda = "(" <> show (natToNatural @n) <> " of type (" <> show (typeRep $ Proxy @a) <> "))"

{- | Accepts 'ContentType' and returns a 'blockRam' implementations initialized with
the corresponding content.
-}
initializedRam ::
  forall dom n a.
  ( HiddenClockResetEnable dom
  , KnownNat n
  , 1 <= n
  , Paddable a
  ) =>
  ContentType n a ->
  ( Signal dom (Index n) ->
    Signal dom (Maybe (Located n a)) ->
    Signal dom a
  )
initializedRam content rd wr = case content of
  Vec vec -> blockRam vec rd wr
  Blob blob -> bitCoerce <$> blockRamBlob blob rd (bitCoerce <$> wr)
  File fp -> bitCoerce <$> blockRamFile (SNat @n) fp rd (bitCoerce <$> wr)

{- | Wishbone storage element with 'Circuit' interface from "Protocols.Wishbone" that
allows for word aligned reads and writes.
-}
wbStorage ::
  forall dom depth aw.
  ( HasCallStack
  , HiddenClockResetEnable dom
  , KnownNat aw
  , 1 <= depth
  , ?busByteOrder :: ByteOrder
  ) =>
  String ->
  SNat depth ->
  Maybe (ContentType depth (Bytes 4)) ->
  Circuit (BitboneMm dom aw) ()
wbStorage memoryName SNat initContent =
  let ?regByteOrder = BigEndian
   in circuit $ \(mm, wbMaster0) -> do
        wbMaster1 <- matchEndianness -< wbMaster0
        [wb0] <- deviceWb memoryName -< (mm, wbMaster1)
        reqresp <- addressableBytesWb regConfig -< wb0
        (reads, writes0) <- ReqResp.partitionEithers -< reqresp
        writes1 <- ReqResp.requests <| ReqResp.dropResponse 0 -< writes0
        _vecUnit <- ram -< (reads, writes1)
        idC -< ()
 where
  regConfig = registerConfig "data"
  ram = ReqResp.fromBlockRamWithMask
    $ case initContent of
      Nothing ->
        blockRamByteAddressableU
      Just content ->
        blockRamByteAddressable @_ @depth content
{-# OPAQUE wbStorage #-}

-- | Storage element with a single wishbone port. Allows for word-aligned addresses.
wbStorage' ::
  forall dom depth aw.
  ( HiddenClockResetEnable dom
  , KnownNat aw
  , KnownNat depth
  , 1 <= depth
  ) =>
  SNat depth ->
  Maybe (ContentType depth (Bytes 4)) ->
  Signal dom (WishboneM2S aw 4) ->
  Signal dom (WishboneS2M 4)
wbStorage' depth initContent wbIn =
  let ?busByteOrder = BigEndian
   in fst $ toSignals (unMemmap $ wbStorage "" depth initContent) (wbIn, ())

{- | Blockram similar to 'blockRam' with the addition that it takes a byte select signal
that controls which nBytes at the write address are updated.
-}
blockRamByteAddressable ::
  forall dom memDepth a.
  (HiddenClockResetEnable dom, KnownNat memDepth, 1 <= memDepth, Paddable a, ShowX a) =>
  -- | Initial content.
  ContentType memDepth a ->
  -- | Read address.
  Signal dom (Index memDepth) ->
  -- | Write operation.
  Signal dom (Maybe (Located memDepth a)) ->
  -- | Byte enables that determine which nBytes get replaced.
  Signal dom (ByteEnable a) ->
  -- | Data at read address (1 cycle delay).
  Signal dom a
blockRamByteAddressable initContent readAddr newEntry byteSelect =
  getDataBe @8 . RegisterBank <$> case initContent of
    Blob _ -> clashCompileError "blockRamByteAddressable: Singular MemBlobs are not supported. "
    Vec vecOfA -> go (byteRam . Vec <$> transpose (fmap getBytes vecOfA))
    File _ ->
      clashCompileError
        "blockRamByteAddressable: Singular source files for initial content are not supported. "
 where
  go brams = readBytes
   where
    writeBytes = unbundle $ splitWriteInBytes <$> newEntry <*> byteSelect
    readBytes = bundle $ brams <*> writeBytes
  getBytes (getRegsBe -> RegisterBank (vec :: Vec (Regs a 8) Byte)) = vec
  byteRam = (`initializedRam` readAddr)

{- | Version of 'blockRamByteAddressable' with undefined initial contents. It is similar
to 'blockRam' with the addition that it takes a byte select signal that controls
which nBytes at the write address are updated.
-}
blockRamByteAddressableU ::
  forall dom memDepth a.
  (HiddenClockResetEnable dom, KnownNat memDepth, 1 <= memDepth, Paddable a, ShowX a) =>
  -- | Read address.
  Signal dom (Index memDepth) ->
  -- | Write operation.
  Signal dom (Maybe (Located memDepth a)) ->
  -- | Byte enables that determine which nBytes get replaced.
  Signal dom (ByteEnable a) ->
  -- | Data at read address (1 cycle delay).
  Signal dom a
blockRamByteAddressableU readAddr newEntry byteSelect =
  getDataBe @8 . RegisterBank <$> readBytes
 where
  writeBytes = unbundle $ splitWriteInBytes <$> newEntry <*> byteSelect
  readBytes = bundle $ ram readAddr <$> writeBytes
  ram = blockRamU NoClearOnReset (SNat @memDepth)

data RegisterWritePriority = CircuitPriority | WishbonePriority
  deriving (Eq)

{- | Takes singular write operation (Maybe (Index maxIndex, writeData)) and splits it up
according to a supplied byteselect bitvector into a vector of byte sized write operations
(Maybe (Index maxIndex, Byte)).
-}
splitWriteInBytes ::
  forall maxIndex writeData.
  (Paddable writeData) =>
  -- | Incoming write operation.
  Maybe (Located maxIndex writeData) ->
  -- | Incoming byte enables.
  ByteEnable writeData ->
  -- | Per byte write operation.
  Vec (Regs writeData 8) (Maybe (LocatedByte maxIndex))
splitWriteInBytes (Just (addr, writeData)) byteSelect =
  case getRegsBe writeData of
    RegisterBank vec -> orNothing <$> unpack byteSelect <*> fmap (addr,) vec
splitWriteInBytes Nothing _ = repeat Nothing

{- | Takes an address and write operation and 'bitCoerce's the addresses as follows:
'bitCoerce' (address, bool)
-}
updateAddrs ::
  (KnownNat n, 1 <= n, KnownNat m, 1 <= m) =>
  -- | An address.
  Index n ->
  -- | A write operation.
  Maybe (Index m, b) ->
  -- | A boolean that will be used for the addresses LSBs.
  AorB ->
  -- |
  --   1. Updated address
  --   2. Write operation with updated address.
  (Index (n * 2), Maybe (Index (m * 2), b))
updateAddrs rdAddr (Just (i, a)) bufSelect =
  (mul2Index rdAddr bufSelect, Just (mul2Index i (swapAorB bufSelect), a))
updateAddrs rdAddr Nothing bufSelect =
  (mul2Index rdAddr bufSelect, Nothing)
