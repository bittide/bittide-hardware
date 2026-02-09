-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE RecordWildCards #-}

module Bittide.DoubleBufferedRam where

import Clash.Prelude

import Data.Constraint
import Data.Maybe
import Protocols (Ack (Ack), CSignal, Circuit (Circuit), Df)
import Protocols.Wishbone

import Bittide.Extra.Maybe
import Bittide.SharedTypes hiding (delayControls)
import Data.Constraint.Nat.Lemmas
import Data.Typeable
import GHC.Stack (HasCallStack)
import Protocols.MemoryMap (
  Access (ReadWrite),
  DeviceDefinition (..),
  MemoryMap (..),
  MemoryMapTree (DeviceInstance),
  Mm,
  Name (..),
  NamedLoc (..),
  Register (..),
  ToConstBwd,
  deviceSingleton,
  locCaller,
  locHere,
  regType,
 )

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
  BlobVec blobVec ->
    getDataBe @8
      . RegisterBank
      <$> bundle
        ((`blockRamBlob` rd) <$> blobVec <*> unbundle ((`splitWriteInBytes` maxBound) <$> wr))
  File fp -> bitCoerce <$> blockRamFile (SNat @n) fp rd (bitCoerce <$> wr)
  FileVec fpVec ->
    getDataBe @8
      . RegisterBank
      <$> bundle
        ( (\fp -> blockRamFile (SNat @n) fp rd)
            <$> fpVec
            <*> unbundle ((`splitWriteInBytes` maxBound) <$> wr)
        )

-- | Circuit wrapper around `wbStorageDP`.
wbStorageDPC ::
  forall dom depth awA awB.
  ( HiddenClockResetEnable dom
  , HasCallStack
  , KnownNat awA
  , KnownNat awB
  , KnownNat depth
  , 1 <= depth
  ) =>
  String ->
  SNat depth ->
  Maybe (ContentType depth (Bytes 4)) ->
  Circuit
    ( ToConstBwd Mm
    , (Bitbone dom awA, Bitbone dom awB)
    )
    ()
wbStorageDPC memoryName depth content = Circuit go
 where
  go (((), (m2sA, m2sB)), ()) = ((SimOnly memMap, (s2mA, s2mB)), ())
   where
    (s2mA, s2mB) = wbStorageDP depth content m2sA m2sB
    memMap =
      MemoryMap
        { tree = DeviceInstance locCaller memoryName
        , deviceDefs = deviceSingleton deviceDef
        }
    deviceDef =
      DeviceDefinition
        { registers =
            [ NamedLoc
                { name = Name "data" ""
                , loc = locHere
                , value =
                    Register
                      { fieldType = regType @(Vec depth (Bytes 4))
                      , address = 0
                      , access = ReadWrite
                      , reset = Nothing
                      , tags = []
                      }
                }
            ]
        , deviceName = Name{name = memoryName, description = ""}
        , definitionLoc = locHere
        , tags = []
        }

{- | Dual-ported Wishbone storage element, essentially a wrapper for the single-ported version
which priorities port A over port B. Transactions are not aborted, but when two transactions
are initiated at the same time, port A will have priority.
-}
wbStorageDP ::
  forall dom depth awA awB.
  ( HiddenClockResetEnable dom
  , KnownNat awA
  , KnownNat awB
  , KnownNat depth
  , 1 <= depth
  ) =>
  SNat depth ->
  Maybe (ContentType depth (Bytes 4)) ->
  Signal dom (WishboneM2S awA 4 (Bytes 4)) ->
  Signal dom (WishboneM2S awB 4 (Bytes 4)) ->
  (Signal dom (WishboneS2M (Bytes 4)), Signal dom (WishboneS2M (Bytes 4)))
wbStorageDP depth initial aM2S bM2S = (aS2M, bS2M)
 where
  storageOut = wbStorage' @_ @_ @(Max awA awB) depth initial storageIn

  storageIn :: Signal dom (WishboneM2S (Max awA awB) 4 (Bytes 4))
  storageIn = mux (nowActive .==. pure A) (resizeM2SAddr <$> aM2S) (resizeM2SAddr <$> bM2S)

  -- We keep track of ongoing transactions to respect Read-modify-write operations.
  nowActive = register A nextActive
  nextActive = selectNow <$> nowActive <*> aM2S <*> aS2M <*> bM2S <*> bS2M
  active WishboneM2S{busCycle, strobe} = busCycle && strobe
  terminated WishboneS2M{acknowledge, err} = acknowledge || err
  selectNow aorb am2s as2m bm2s bs2m =
    case (aorb, active am2s, terminated as2m, active bm2s, terminated bs2m) of
      (_, True, _, False, _) -> A
      (_, False, _, True, _) -> B
      (A, True, True, True, _) -> B
      (B, True, _, True, True) -> A
      _ -> aorb

  (aS2M, bS2M) =
    unbundle
      $ mux
        (nowActive .==. pure A)
        (bundle (storageOut, noTerminate <$> storageOut))
        (bundle (noTerminate <$> storageOut, storageOut))

  noTerminate wb = wb{acknowledge = False, err = False, retry = False, stall = False}

{- | Wishbone storage element with 'Circuit' interface from "Protocols.Wishbone" that
allows for word aligned reads and writes.
-}
wbStorage ::
  forall dom depth aw.
  ( HasCallStack
  , HiddenClockResetEnable dom
  , KnownNat aw
  , KnownNat depth
  , 1 <= depth
  ) =>
  String ->
  SNat depth ->
  Maybe (ContentType depth (Bytes 4)) ->
  Circuit (BitboneMm dom aw) ()
wbStorage memoryName depth initContent = Circuit $ \(((), m2s), ()) ->
  ((SimOnly memMap, wbStorage' depth initContent m2s), ())
 where
  memMap =
    MemoryMap
      { tree = DeviceInstance locCaller memoryName
      , deviceDefs = deviceSingleton deviceDef
      }
  deviceDef =
    DeviceDefinition
      { registers =
          [ NamedLoc
              { name = Name "data" ""
              , loc = locHere
              , value =
                  Register
                    { fieldType = regType @(Vec depth (Bytes 4))
                    , address = 0
                    , access = ReadWrite
                    , reset = Nothing
                    , tags = []
                    }
              }
          ]
      , deviceName = Name{name = memoryName, description = ""}
      , definitionLoc = locHere
      , tags = []
      }
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
  Signal dom (WishboneM2S aw 4 (Bytes 4)) ->
  Signal dom (WishboneS2M (Bytes 4))
wbStorage' SNat initContent wbIn = delayControls wbIn wbOut
 where
  readData = ram readAddr writeEntry wbIn.busSelect

  ram = case initContent of
    Nothing ->
      blockRamByteAddressableU
    Just content ->
      blockRamByteAddressable content

  (readAddr, writeEntry, wbOut) =
    unbundle (go <$> bundle (wbIn, readData))

  go (WishboneM2S{..}, readDataGo) =
    ( wbAddr
    , writeEntryGo
    , (emptyWishboneS2M @(Bytes 4)){acknowledge, readData = readDataGo, err}
    )
   where
    wbAddr = unpack $ resize addr :: Index depth
    addrLegal =
      case compareSNat (SNat @((2 ^ aw) - 1)) (SNat @depth) of
        SNatLE -> True
        _ -> addr < natToNum @depth

    masterActive = strobe && busCycle
    err = masterActive && not addrLegal
    acknowledge = masterActive && addrLegal

    masterWriting = acknowledge && writeEnable

    writeEntryGo
      | masterWriting = Just (wbAddr, writeData)
      | otherwise = Nothing

  -- \| Delays the output controls to align them with the actual read / write timing.
  delayControls ::
    (NFDataX a) =>
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
    s2m1 =
      (\wb newAck newErr -> wb{acknowledge = newAck, err = newErr})
        <$> s2m0
        <*> delayedAck
        <*> err1

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
    BlobVec blobs -> go (fmap (byteRam . Blob) blobs)
    File _ ->
      clashCompileError
        "blockRamByteAddressable: Singular source files for initial content are not supported. "
    FileVec blobs -> go (fmap (byteRam . File) blobs)
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

{- | Register with additional wishbone interface, this component has a configurable
priority that determines which value gets stored in the register during a write conflict.
The `RegisterWritePriority` determines if the wishbone write gets accepted or if the
`Df` write gets accepted. The other value is discarded.
-}
registerWbC ::
  forall dom a nBytes aw.
  ( HiddenClockResetEnable dom
  , Paddable a
  , KnownNat nBytes
  , 1 <= nBytes
  , KnownNat aw
  ) =>
  -- | Determines the write priority on write collisions
  RegisterWritePriority ->
  -- | Initial value.
  a ->
  Circuit (Wishbone dom 'Standard aw (Bytes nBytes), Df dom a) (CSignal dom a)
registerWbC prio initVal = case cancelMulDiv @nBytes @8 of
  Dict -> Circuit go
   where
    go ((wbM2S, dfM2S), _) = ((wbS2M, fmap Ack dfS2M), aOut)
     where
      (aOut, wbS2M) = registerWb prio initVal wbM2S dfM2S
      dfS2M
        | prio == WishbonePriority = (\WishboneM2S{..} -> not (strobe && busCycle)) <$> wbM2S
        | otherwise = pure True

{- | Register with additional wishbone interface, this component has a configurable
priority that determines which value gets stored in the register during a write conflict.
With 'CircuitPriority', the incoming value in the fourth argument gets stored on a
collision and the wishbone bus gets acknowledged, but the value is silently ignored.
With 'WishbonePriority', the incoming wishbone write gets accepted and the value in the
fourth argument gets ignored.
-}
registerWb ::
  forall dom a nBytes addrW.
  ( HiddenClockResetEnable dom
  , Paddable a
  , KnownNat nBytes
  , 1 <= nBytes
  , KnownNat addrW
  ) =>
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

{-# OPAQUE registerWbE #-}

{- | Register with additional wishbone interface, this component has a configurable
priority that determines which value gets stored in the register during a write conflict.
With 'CircuitPriority', the incoming value in the fourth argument gets stored on a
collision and the wishbone bus gets acknowledged, but the value is silently ignored.
With 'WishbonePriority', the incoming wishbone write gets accepted and the value in the
fourth argument gets ignored. This version has an additional argument for circuit write
byte enables.
-}
registerWbE ::
  forall dom a nBytes addrW.
  ( HiddenClockResetEnable dom
  , Paddable a
  , KnownNat nBytes
  , 1 <= nBytes
  , KnownNat addrW
  ) =>
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
    a ->
    Maybe a ->
    BitVector (Regs a 8) ->
    WishboneM2S addrW nBytes (Bytes nBytes) ->
    (BitVector (Regs a 8), WishboneS2M (Bytes nBytes), a)
  go regOut0 sigIn0 sigbyteEnables0 WishboneM2S{..} =
    ( byteEnables0
    , (emptyWishboneS2M @(Bytes nBytes)){acknowledge, err, readData}
    , regIn0
    )
   where
    invalidAddress = addr > resize (pack (maxBound :: Index (Max 1 (Regs a (nBytes * 8)))))
    masterActive = strobe && busCycle
    err = masterActive && invalidAddress
    acknowledge = masterActive && not err
    wbWriting = writeEnable && acknowledge
    wbAddr = unpack (resize addr) :: Index (Max 1 (Regs a (nBytes * 8)))
    readData = case getRegsLe regOut0 of
      RegisterBank vec -> vec !! wbAddr

    wbByteEnables =
      resize . pack . reverse $ replace wbAddr busSelect (repeat @(Regs a (nBytes * 8)) 0)
    sigRegIn = fromMaybe (errorX "registerWb: sigIn is Nothing when Just is expected.") sigIn0
    wbRegIn = getDataLe . RegisterBank $ repeat writeData
    (byteEnables0, regIn0) = case (writePriority, isJust sigIn0, wbWriting) of
      (CircuitPriority, True, _) -> (sigbyteEnables0, sigRegIn)
      (CircuitPriority, False, True) -> (wbByteEnables, wbRegIn)
      (WishbonePriority, _, True) -> (wbByteEnables, wbRegIn)
      (WishbonePriority, True, False) -> (sigbyteEnables0, sigRegIn)
      (_, False, False) -> (0, errorX "registerWb: register input not defined.")

{- | Register similar to 'register' with the addition that it takes a byte select signal
that controls which nBytes are updated.
-}
registerByteAddressable ::
  forall dom a.
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
  regsOut =
    (`andEnable` register)
      <$> unbundle (reverse . unpack <$> byteEnables)
      <*> initBytes
      <*> newBytes
  getBytes (getRegsLe -> RegisterBank vec) = vec

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
  -- 1. Updated address
  -- 2. Write operation with updated address.
  (Index (n * 2), Maybe (Index (m * 2), b))
updateAddrs rdAddr (Just (i, a)) bufSelect =
  (mul2Index rdAddr bufSelect, Just (mul2Index i (swapAorB bufSelect), a))
updateAddrs rdAddr Nothing bufSelect =
  (mul2Index rdAddr bufSelect, Nothing)
