-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# OPTIONS_GHC -fconstraint-solver-iterations=7#-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Bittide.DoubleBufferedRam where

import Clash.Prelude

import Data.Bifunctor
import Data.Constraint
import Data.Maybe
import Protocols (Circuit (Circuit))
import Protocols.Wishbone

import Bittide.SharedTypes hiding (delayControls)
import Data.Constraint.Nat.Extra
import Data.Typeable

data ContentType n a
  = Vec (Vec n a)
  | Blob (MemBlob n (BitSize a))
  | BlobVec (Vec (Regs a 8) (MemBlob n 8))
  | File FilePath
  | FileVec (Vec (Regs a 8) FilePath)

instance (Show a, KnownNat n, Typeable a) => Show (ContentType n a) where
  show = \case
    (Vec _) -> "Vec: " <> nAnda
    (Blob _) -> "Blob: " <> nAnda
    (BlobVec _) -> "BlobVec: " <> nAnda
    (File fp) -> "File: " <> nAnda <> ", filepath = " <> fp
    (FileVec fps) -> "File: " <> nAnda <> ", filepaths = " <> show fps
   where
    nAnda = "(" <> show (natToNatural @n) <> " of type (" <> show (typeRep $ Proxy @a) <> "))"
data InitialContent elements a where
  NonReloadable :: ContentType elements a -> InitialContent elements a
  Reloadable    :: ContentType elements a -> InitialContent elements a
  Undefined     :: (1 <= elements, KnownNat elements) => InitialContent elements a

deriving instance (Show a, KnownNat elements, Typeable a) => Show (InitialContent elements a)

-- | Accepts 'InitialContents' and returns a 'blockRam' implementations initialized with
-- the corresponding content.
initializedRam ::
  forall dom n a .
  ( HiddenClockResetEnable dom
  , KnownNat n, 1 <= n
  , Paddable a) =>
  ContentType n a ->
  ( Signal dom (Index n)->
    Signal dom (Maybe (Located n a)) ->
    Signal dom a)
initializedRam content rd wr = case content of
  Vec vec         -> blockRam vec rd wr
  Blob blob       -> bitCoerce <$> blockRamBlob blob rd (bitCoerce <$> wr)
  BlobVec blobVec -> getDataBe @8 . RegisterBank <$>
    bundle ((`blockRamBlob` rd) <$> blobVec <*> unbundle ((`splitWriteInBytes` maxBound) <$> wr))
  File fp -> bitCoerce <$> blockRamFile (SNat @n) fp rd (bitCoerce <$> wr)
  FileVec fpVec -> getDataBe @8 . RegisterBank <$>
    bundle ((\ fp -> blockRamFile (SNat @n) fp rd)
    <$> fpVec
    <*> unbundle ((`splitWriteInBytes` maxBound) <$> wr))

contentGenerator ::
  forall dom romSize targetSize a .
  ( HiddenClockResetEnable dom
  , KnownNat targetSize,  1 <= targetSize
  , KnownNat romSize, romSize <= targetSize
  , Paddable a) =>
  ContentType romSize a ->
  (Signal dom (Maybe (Located targetSize a)), Signal dom Bool)
contentGenerator content = case compareSNat d1 (SNat @romSize) of
  SNatLE -> (mux (running .&&. not <$> done) writeOp (pure Nothing), done)
   where
    running = register False $ pure True
    writeOp = curry Just . resize <$> romAddr0 <*> element
    done = register False (done .||. (running .&&. romAddr0 .==. pure maxBound))
    romAddr0 = register (maxBound :: Index romSize) romAddr1
    romAddr1 = satSucc SatWrap <$> romAddr0
    element = initializedRam content (bitCoerce <$> romAddr1) (pure Nothing)
  _ -> (pure Nothing, pure True)


-- | Dual-ported Wishbone storage element, essentially a wrapper for the single-ported version
-- which priorities port A over port B. Transactions are not aborted, but when two transactions
-- are initiated at the same time, port A will have priority.
wbStorageDP ::
  forall dom depth awA awB .
  ( HiddenClockResetEnable dom
  , KnownNat awA, 2 <= awA
  , KnownNat awB, 2 <= awB
  , KnownNat depth, 1 <= depth) =>
  InitialContent depth (Bytes 4) ->
  Signal dom (WishboneM2S awA 4 (Bytes 4)) ->
  Signal dom (WishboneM2S awB 4 (Bytes 4)) ->
  (Signal dom (WishboneS2M (Bytes 4)),Signal dom (WishboneS2M (Bytes 4)))
wbStorageDP initial aM2S bM2S = (aS2M, bS2M)
 where
  storageOut = case lessThanMax @awA @awB @2 of
    Dict -> wbStorage' @_ @depth @(Max awA awB) initial storageIn

  storageIn :: Signal dom (WishboneM2S (Max awA awB) 4 (Bytes 4))
  storageIn = mux (nowSelected .==. pure A) (resizeM2SAddr <$> aM2S) (resizeM2SAddr <$> bM2S)

  -- We keep track of ongoing transactions to respect Read-modify-write operations.
  lastSelected = register A nowSelected
  nowSelected = selectNow <$> aM2S <*> bM2S <*> lastSelected
  selectNow (busCycle -> cycA) (busCycle -> cycB) lastSel
    | lastSel == B && cycB = B
    | cycA && not cycB     = A
    | otherwise            = A

  (aS2M, bS2M) = unbundle $ mux (nowSelected .==. pure A)
    (bundle (storageOut, noTerminate <$> storageOut))
    (bundle (noTerminate <$> storageOut, storageOut))

  noTerminate wb = wb{acknowledge = False, err = False, retry = False, stall = False}

-- | Wishbone storage element with 'Circuit' interface from "Protocols.Wishbone" that
-- allows for half-word aligned reads and writes.
wbStorage ::
  forall dom depth aw .
  ( HiddenClockResetEnable dom
  , KnownNat aw, 2 <= aw
  , KnownNat depth, 1 <= depth) =>
  InitialContent depth (Bytes 4) ->
  Circuit (Wishbone dom 'Standard aw (Bytes 4)) ()
wbStorage initContent = Circuit $ \(m2s, ()) ->
  (wbStorage' initContent m2s, ())

-- | Storage element with a single wishbone port, it is half word addressable to work with
-- RISC-V's C extension. This storage element allows for half-word aligned accesses.
wbStorage' ::
  forall dom depth aw .
  ( HiddenClockResetEnable dom
  , KnownNat aw, 2 <= aw
  , KnownNat depth, 1 <= depth) =>
  InitialContent depth (Bytes 4) ->
  Signal dom (WishboneM2S aw 4 (Bytes 4)) ->
  Signal dom (WishboneS2M (Bytes 4))
wbStorage' initContent wbIn = delayControls wbIn wbOut
 where
  romOut = case initContent of
    Reloadable content-> bundle $ contentGenerator content
    other -> deepErrorX ("wbStorage': No contentgenerator for " <> show other)

  readDataA = ramA readAddrA writeEntryA byteSelectA
  readDataB = ramB readAddrB writeEntryB byteSelectB

  (ramA, ramB, isReloadable) = case initContent of
    Reloadable _ ->
      (blockRamByteAddressableU, blockRamByteAddressableU, True)
    Undefined ->
      (blockRamByteAddressableU, blockRamByteAddressableU, False)
    NonReloadable (BlobVec (splitAtI -> (b, a))) ->
      ( blockRamByteAddressable @_ @depth $ BlobVec a
      , blockRamByteAddressable @_ @depth $ BlobVec b
      , False)
    NonReloadable (FileVec (splitAtI -> (b, a))) ->
      ( blockRamByteAddressable @_ @depth $ FileVec a
      , blockRamByteAddressable @_ @depth $ FileVec b
      , False)
    (NonReloadable (Vec (unzip @depth . bitCoerce -> (b, a)))) ->
      ( blockRamByteAddressable @_ @depth $ Vec a
      , blockRamByteAddressable @_ @depth $ Vec b
      , False)
    _ -> error $ "wbStorage': " <> show initContent <> " not supported."

  (readAddrA, readAddrB, writeEntryA, writeEntryB, byteSelectA, byteSelectB, wbOut) =
    unbundle $ go <$> wbIn <*> readDataB <*> readDataA <*> romOut

  go WishboneM2S{..} rdB rdA romOut0 =
    ( addrA
    , addrB
    , writeEntryA0
    , writeEntryB0
    , byteSelectA1
    , byteSelectB1
    , (emptyWishboneS2M @(Bytes 4)){acknowledge,readData,err})
   where

    (bitCoerce . resize -> wbAddr :: Index depth, alignment) =  split @_ @(aw - 2) addr

    -- When the second lowest bit is not set, the address is considered word aligned.
    -- We don't care about the first bit to determine the address alignment because
    -- byte aligned addresses are considered illegal.
    (not -> wordAligned, byteAligned) = bimap unpack unpack $ split alignment

    -- The depth of the memory is defined as the number of words in the memory
    -- (words are 4 bytes wide). The wishbone interface addresses per byte, so we multiply
    -- the depth by 4 to get the number of bytes in the memory.
    addrLegal = addr < (natToNum @(4 * depth)) && not byteAligned

    masterActive = strobe && busCycle
    err = masterActive && not addrLegal
    acknowledge = masterActive && (not isReloadable || romDone) && addrLegal
    masterWriting = masterActive && writeEnable && not err

    (bsHigh,bsLow) = split busSelect
    (writeDataHigh, writeDataLow) = split writeData

    ((addrA, byteSelectA0, writeDataA), (addrB, byteSelectB0, writeDataB))
      | wordAligned =
        ( (wbAddr, bsLow, writeDataLow)
        , (wbAddr, bsHigh, writeDataHigh))
      | otherwise =
        ( (satSucc SatBound wbAddr, bsHigh, writeDataHigh)
        , (wbAddr, bsLow, writeDataLow))

    (romWrite, romDone) = romOut0
    (romWriteB, romWriteA) = splitWrite romWrite
    (writeEntryA0, writeEntryB0)
      | isReloadable && not romDone = (romWriteA, romWriteB)
      | masterWriting = (Just (addrA, writeDataA),Just (addrB, writeDataB))
      | otherwise = (Nothing,Nothing)
    readData
      | wordAligned = rdB ++# rdA
      | otherwise   = rdA ++# rdB

    (byteSelectA1, byteSelectB1)
      | isReloadable && not romDone = (maxBound,maxBound)
      | otherwise = (byteSelectA0, byteSelectB0)

  splitWrite ::
    KnownNat bits =>
    Maybe (LocatedBits n (2*bits)) ->
    (Maybe (LocatedBits n bits), Maybe (LocatedBits n bits))
  splitWrite (Just (i, a)) = (Just (i,upper), Just (i, lower))
   where
    (upper,lower) = split a
  splitWrite Nothing = (Nothing, Nothing)

  -- | Delays the output controls to align them with the actual read / write timing.
  delayControls ::
    NFDataX a =>
    Signal dom (WishboneM2S aw selWidth a) -> -- current M2S signal
    Signal dom (WishboneS2M a) ->
    Signal dom (WishboneS2M a)
  delayControls m2s s2m0 = mux inCycle s2m1 (pure emptyWishboneS2M)
   where
    inCycle = (busCycle <$> m2s) .&&. (strobe <$> m2s)

    -- It takes a single cycle to lookup elements in a block ram. We can therfore
    -- only process a request every other clock cycle.
    ack = (acknowledge <$> s2m0) .&&. (not <$> err1) .&&. (not <$> delayedAck) .&&. inCycle
    err1 = (err <$> s2m0) .&&. inCycle
    delayedAck = register False ack
    s2m1 = (\wb newAck newErr-> wb{acknowledge = newAck, err = newErr})
      <$> s2m0 <*> delayedAck <*> err1

-- | The double buffered Ram component is a memory component that contains two buffers
-- and enables the user to write to one buffer and read from the other. 'AorB'
-- selects which buffer is written to, while read operations read from the other buffer.
doubleBufferedRam ::
  forall dom memDepth a .
  (HiddenClockResetEnable dom, KnownNat memDepth, 1 <= memDepth, Paddable a, ShowX a) =>
  -- | Initial content.
  ContentType (2 * memDepth) a ->
  -- | Controls which buffers is written to, while the other buffer is read from.
  Signal dom AorB ->
  -- | Read address.
  Signal dom (Index memDepth) ->
  -- | Write operation.
  Signal dom (Maybe (Located memDepth a)) ->
  -- | Data at read address (1 cycle delay).
  Signal dom a
doubleBufferedRam initContent outputSelect rd0 wr0 =
  initializedRam initContent rd1 wr1
 where
  (rd1, wr1) = unbundle $ updateAddrs <$> rd0 <*> wr0 <*> outputSelect

-- | Version of 'doubleBufferedRam' with undefined initial contents. This component
-- contains two buffers and enables the user to write to one buffer and read from the
-- other. 'AorB' selects which buffer is written to, while read operations
-- read from the other buffer.
doubleBufferedRamU ::
  forall dom memDepth a .
  (HiddenClockResetEnable dom, KnownNat memDepth, 1 <= memDepth, NFDataX a) =>
  -- | Controls which buffers is written to, while the other buffer is read from.
  Signal dom AorB ->
  -- | Read address.
  Signal dom (Index memDepth) ->
  -- | Incoming data frame.
  Signal dom (Maybe (Index memDepth, a)) ->
  -- | Outgoing data
  Signal dom a
doubleBufferedRamU outputSelect readAddr0 writeFrame0 =
  blockRamU NoClearOnReset (SNat @(2 * memDepth)) rstFunc readAddr1 writeFrame1
 where
  (readAddr1, writeFrame1) =
    unbundle $ updateAddrs <$> readAddr0 <*> writeFrame0 <*> outputSelect
  rstFunc = const (errorX "doubleBufferedRamU: reset function undefined")

-- | The byte addressable double buffered Ram component is a memory component that
-- consists of two buffers and internally stores its elements as a multiple of 8 bits.
-- It contains a blockRam per byte and uses the one hot byte select signal to determine
-- which bytes will be overwritten during a write operation. This components writes to
-- one buffer and reads from the other. 'AorB' selects which buffer is
-- written to, while read operations read from the other buffer.
doubleBufferedRamByteAddressable ::
  forall dom memDepth a .
  (HiddenClockResetEnable dom, KnownNat memDepth, 1 <= memDepth, Paddable a, ShowX a) =>
  -- | Initial content
  ContentType (2 * memDepth) a ->
  -- | Controls which buffers is written to, while the other buffer is read from.
  Signal dom AorB ->
  -- | Read address.
  Signal dom (Index memDepth) ->
  -- | Write operation.
  Signal dom (Maybe (Located memDepth a)) ->
  -- | Byte enables that determine which nBytes get replaced.
  Signal dom (ByteEnable a) ->
  -- | Data at read address (1 cycle delay).
  Signal dom a
doubleBufferedRamByteAddressable initContent outputSelect rd0 wr0 =
  blockRamByteAddressable initContent rd1 wr1
 where
  (rd1, wr1) = unbundle $ updateAddrs <$> rd0 <*> wr0 <*> outputSelect


-- | Version of 'doubleBufferedRamByteAddressable' where the initial content is undefined.
-- This memory element consists of two buffers and internally stores its elements as a
-- multiple of 8 bits. It contains a blockRam per byte and uses the one hot byte select
-- signal to determine which nBytes will be overwritten during a write operation.
-- This components writes to one buffer and reads from the other. Which buffer is
-- used for reading while the other is used for writing is controlled by the 'AorB'.
doubleBufferedRamByteAddressableU ::
  forall dom memDepth a .
  ( KnownNat memDepth, 1 <= memDepth, HiddenClockResetEnable dom, Paddable a, ShowX a) =>
  -- | Controls which buffers is written to, while the other buffer is read from.
  Signal dom AorB ->
  -- | Read address.
  Signal dom (Index memDepth) ->
  -- | Incoming data frame.
  Signal dom (Maybe (Located  memDepth a)) ->
  -- | One hot byte select for writing only
  Signal dom (ByteEnable a) ->
  -- | Outgoing data
  Signal dom a
doubleBufferedRamByteAddressableU outputSelect readAddr0 writeFrame0 =
  blockRamByteAddressableU readAddr1 writeFrame1
 where
  (readAddr1, writeFrame1) =
    unbundle $ updateAddrs <$> readAddr0 <*> writeFrame0 <*> outputSelect

-- | Blockram similar to 'blockRam' with the addition that it takes a byte select signal
-- that controls which nBytes at the write address are updated.
blockRamByteAddressable ::
  forall dom memDepth a .
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
    Blob _  -> deepErrorX "blockRamByteAddressable: Singular MemBlobs are not supported. "
    Vec vecOfA -> go (byteRam . Vec <$> transpose (fmap getBytes vecOfA))
    BlobVec blobs -> go (fmap (byteRam . Blob) blobs)
    File _ -> deepErrorX "blockRamByteAddressable: Singular source files for initial content are not supported. "
    FileVec blobs -> go (fmap (byteRam . File) blobs)
 where
  go brams =  readBytes
   where
    writeBytes = unbundle $ splitWriteInBytes <$> newEntry <*> byteSelect
    readBytes = bundle $ brams <*> writeBytes
  getBytes (getRegsBe -> RegisterBank (vec :: Vec (Regs a 8) Byte)) = vec
  byteRam = (`initializedRam` readAddr)


-- | Version of 'blockRamByteAddressable' with undefined initial contents. It is similar
-- to 'blockRam' with the addition that it takes a byte select signal that controls
-- which nBytes at the write address are updated.
blockRamByteAddressableU ::
  forall dom memDepth a .
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
  ram = blockRamU NoClearOnReset (SNat @memDepth) rstFunc
  rstFunc = const (errorX "blockRamByteAddressableU: reset function undefined")

data RegisterWritePriority = CircuitPriority | WishbonePriority

-- | Register with additional wishbone interface, this component has a configurable
-- priority that determines which value gets stored in the register during a write conflict.
-- With 'CircuitPriority', the incoming value in the fourth argument gets stored on a
-- collision and the wishbone bus gets acknowledged, but the value is silently ignored.
-- With 'WishbonePriority', the incoming wishbone write gets accepted and the value in the
-- fourth argument gets ignored.
registerWb ::
  forall dom a nBytes addrW .
  ( HiddenClockResetEnable dom
  , Paddable a
  , KnownNat nBytes
  , 1 <= nBytes
  , KnownNat addrW
  , 2 <= addrW) =>
  -- | Determines the write priority on write collisions
  RegisterWritePriority ->
  -- | Initial value.
  a ->
  -- | Wishbone bus (master to slave)
  Signal dom (WishboneM2S addrW nBytes (Bytes nBytes)) ->
  -- | New circuit value.
  Signal dom (Maybe a) ->
  -- |
  -- 1. Outgoing stored value
  -- 2. Outgoing wishbone bus (slave to master)
  (Signal dom a, Signal dom (WishboneS2M (Bytes nBytes)))
registerWb writePriority initVal wbIn sigIn =
  registerWbE writePriority initVal wbIn sigIn (pure maxBound)

{-# NOINLINE registerWbE #-}
-- | Register with additional wishbone interface, this component has a configurable
-- priority that determines which value gets stored in the register during a write conflict.
-- With 'CircuitPriority', the incoming value in the fourth argument gets stored on a
-- collision and the wishbone bus gets acknowledged, but the value is silently ignored.
-- With 'WishbonePriority', the incoming wishbone write gets accepted and the value in the
-- fourth argument gets ignored. This version has an additional argument for circuit write
-- byte enables.
registerWbE ::
  forall dom a nBytes addrW .
  ( HiddenClockResetEnable dom
  , Paddable a
  , KnownNat nBytes
  , 1 <= nBytes
  , KnownNat addrW
  , 2 <= addrW) =>
  -- | Determines the write priority on write collisions
  RegisterWritePriority ->
  -- | Initial value.
  a ->
  -- | Wishbone bus (master to slave)
  Signal dom (WishboneM2S addrW nBytes (Bytes nBytes)) ->
  -- | New circuit value.
  Signal dom (Maybe a) ->
  -- | Explicit Byte enables for new circuit value
  Signal dom (ByteEnable a) ->
  -- |
  -- 1. Outgoing stored value
  -- 2. Outgoing wishbone bus (slave to master)
  (Signal dom a, Signal dom (WishboneS2M (Bytes nBytes)))
registerWbE writePriority initVal wbIn sigIn sigByteEnables = (regOut, wbOut)
 where
  regOut = registerByteAddressable initVal regIn byteEnables
  (byteEnables, wbOut, regIn) = unbundle (go <$> regOut <*> sigIn <*> sigByteEnables <*> wbIn)
  go ::
    a  ->
    Maybe a ->
    BitVector (Regs a 8) ->
    WishboneM2S addrW nBytes (Bytes nBytes) ->
    (BitVector (Regs a 8), WishboneS2M (Bytes nBytes), a)
  go regOut0 sigIn0 sigbyteEnables0 WishboneM2S{..} =
    ( byteEnables0
    , (emptyWishboneS2M @(Bytes nBytes)) {acknowledge, err, readData}
    , regIn0 )
   where
    (alignedAddress, alignment) = split @_ @(addrW - 2) @2 addr
    addressRange = maxBound :: Index (Max 1 (Regs a (nBytes * 8)))
    invalidAddress = (alignedAddress > resize (pack addressRange)) || alignment /= 0
    masterActive = strobe && busCycle
    err = masterActive && invalidAddress
    acknowledge = masterActive && not err
    wbWriting = writeEnable && acknowledge
    wbAddr = unpack . resize $ pack alignedAddress :: Index (Max 1 (Regs a (nBytes * 8)))
    readData = case getRegsLe regOut0 of
      RegisterBank vec -> vec !! wbAddr

    wbByteEnables =
      resize . pack . reverse $ replace wbAddr busSelect (repeat @(Regs a (nBytes*8)) 0)
    sigRegIn = fromMaybe (errorX "registerWb: sigIn is Nothing when Just is expected.") sigIn0
    wbRegIn = getDataLe . RegisterBank $ repeat writeData
    (byteEnables0, regIn0) = case (writePriority, isJust sigIn0, wbWriting) of
      (CircuitPriority , True , _)     -> (sigbyteEnables0, sigRegIn)
      (CircuitPriority , False, True)  -> (wbByteEnables, wbRegIn)
      (WishbonePriority, _    , True)  -> (wbByteEnables, wbRegIn)
      (WishbonePriority, True , False) -> (sigbyteEnables0, sigRegIn)
      (_               , False, False) -> (0, errorX "registerWb: register input not defined.")

-- | Register similar to 'register' with the addition that it takes a byte select signal
-- that controls which nBytes are updated.
registerByteAddressable ::
  forall dom a .
  (HiddenClockResetEnable dom, Paddable a) =>
  -- | Initial value.
  a ->
  -- | New value.
  Signal dom a ->
  -- | Byte enables that determine which nBytes of the new value are stored.
  Signal dom (ByteEnable a) ->
  -- | Stored value.
  Signal dom a
registerByteAddressable initVal newVal byteEnables =
  getDataLe @8 . RegisterBank <$> bundle regsOut
 where
  initBytes = getBytes initVal
  newBytes = unbundle $ getBytes <$> newVal
  regsOut = (`andEnable` register) <$>
    unbundle (reverse . unpack <$> byteEnables) <*> initBytes <*> newBytes
  getBytes (getRegsLe -> RegisterBank vec) = vec

-- | Takes singular write operation (Maybe (Index maxIndex, writeData)) and splits it up
-- according to a supplied byteselect bitvector into a vector of byte sized write operations
-- (Maybe (Index maxIndex, Byte)).
splitWriteInBytes ::
  forall maxIndex writeData .
  (Paddable writeData) =>
  -- | Incoming write operation.
  Maybe (Located maxIndex writeData) ->
  -- | Incoming byte enables.
  ByteEnable writeData ->
  -- | Per byte write operation.
  Vec (Regs writeData 8) (Maybe (LocatedByte maxIndex))
splitWriteInBytes (Just (addr, writeData)) byteSelect =
  case getRegsBe writeData of
    RegisterBank vec -> splitWrites <$> unpack byteSelect <*> vec
     where
      splitWrites :: Bool -> Byte -> Maybe (LocatedByte maxIndex)
      splitWrites b bv = if b then Just (addr, bv) else Nothing

splitWriteInBytes Nothing _ = repeat Nothing

-- | Takes an address and write operation and 'bitCoerce's the addresses as follows:
-- 'bitCoerce' (address, bool)
updateAddrs ::
  (KnownNat n, 1 <= n, KnownNat m, 1 <= m) =>
  -- | An address.
  Index n
  -- | A write operation.
  -> Maybe (Index m, b)
  -- | A boolean that will be used for the addresses LSBs.
  -> AorB
  -- |
  -- 1. Updated address
  -- 2. Write operation with updated address.
  -> (Index (n * 2), Maybe (Index (m * 2), b))
updateAddrs rdAddr (Just (i, a)) bufSelect =
  (mul2Index rdAddr bufSelect, Just (mul2Index i (swapAorB bufSelect), a))

updateAddrs rdAddr Nothing bufSelect =
  (mul2Index rdAddr bufSelect, Nothing)

{-# NOINLINE fifo #-}

-- | State record for the FIFO circuit.
data FifoState depth = FifoState
  { readCounter   :: Index depth
  , writeCounter  :: Index depth
  , dataCount     :: Index (depth + 1)
  , fifoEmpty     :: Bool
  } deriving (Generic, NFDataX)

-- | A generic First-In-First-Out (FIFO) circuit with a specified depth.
-- The FIFO circuit allows data elements of type 'a' to be stored and
-- retrieved in the order they were added. The circuit contains a ready-based back pressure
-- mechanism and does not feature bypassing for an empty fifo.
-- When the reset is high or the enable is low, the output will be `Nothing` and the ready
-- flag will be `False`.
fifo ::
  forall dom depth a .
  (HiddenClockResetEnable dom,  2 <= depth, NFDataX a) =>
  -- | The depth of the FIFO, should be at least 2.
  SNat depth ->
  -- | The input data to the FIFO, when it is 'Just a' and the outgoing ready flag is true,
  -- 'a' is added to the FIFO. When this is 'Nothing', no element is added to the FIFO.
  Signal dom (Maybe a) ->
  -- | A boolean flag indicating whether the outgoing element is ready to be consumed.
  Signal dom Bool ->
  -- |
  -- (1) Output data from the FIFO.
  -- (2) Boolean flag indicating whether the FIFO accepts incoming elements.
  -- (3) Number of data elements in the FIFO.
  ( Signal dom (Maybe a)
  , Signal dom Bool
  , Signal dom (Index (depth + 1)))
fifo depth@SNat fifoIn readyIn = (maybeOut, readyOut .&&. active, dataCountOut)
 where
  maybeOut = mux active fifoOut (pure Nothing)
  active = unsafeToLowPolarity hasReset .&&. fromEnable hasEnable
  bramOut = readNew (blockRamU NoClearOnReset depth (const undefined)) readAddr writeOp
  (readAddr, writeOp, fifoOut, readyOut, dataCountOut) =
    mealyB go initialState (fifoIn, readyIn, bramOut)

  -- Initial state of the FIFO
  initialState = FifoState
    { readCounter  = 0
    , writeCounter = 0
    , dataCount    = 0
    , fifoEmpty    = True
    }
  go ::
    FifoState depth ->
    (Maybe a, Bool, a) ->
    ( FifoState depth
    , (Index depth, Maybe (Index depth, a), Maybe a, Bool, Index (depth + 1)))
  go FifoState{..} (fifoInGo, readyInGo, bramOutGo) =
      (nextState, output)
   where
    readSucc  = satSucc SatWrap readCounter
    writeSucc = satSucc SatWrap writeCounter

    readCounterNext
      | not fifoEmpty && readyInGo = readSucc
      | otherwise                  = readCounter

    (writeCounterNext, writeOpGo)
      | not full && isJust fifoInGo = (writeSucc, (writeCounter,) <$> fifoInGo)
      | otherwise                   = (writeCounter, Nothing)

    countersSame     = readCounter == writeCounter
    nextCountersSame = readCounterNext == writeCounterNext

    full = not fifoEmpty && countersSame

    fifoEmptyNext
      | fifoEmpty = countersSame
      | otherwise = readyInGo && nextCountersSame

    fifoOutGo
      | fifoEmpty = Nothing
      | otherwise = Just bramOutGo

    dataCountNext = satAdd SatBound dataCount dataCountChange
    dataCountChange = case (isJust fifoInGo && not full, not fifoEmpty && readyInGo) of
      (True, False) -> 1
      (False, True) -> -1
      _             -> 0

    nextState = FifoState
      { readCounter  = readCounterNext
      , writeCounter = writeCounterNext
      , dataCount    = dataCountNext
      , fifoEmpty    = fifoEmptyNext
      }

    output = (readCounterNext, writeOpGo, fifoOutGo, not full, dataCount)
