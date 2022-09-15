{-# LANGUAGE GADTs #-}
module TH where

import Clash.Prelude hiding (Exp)

import Bittide.SharedTypes
import Language.Haskell.TH
import Paths_demo1_sim
import ContranomySim.DeviceTreeCompiler
import System.Exit
import System.FilePath

import qualified Data.IntMap.Strict as I
import qualified Data.List as L
import qualified Data.ByteString as BS
import ContranomySim.MemoryMapConsts
import ContranomySim.ReadElf
import System.Directory


elfTargetDir :: IO FilePath
elfTargetDir = do
  dir <- bittideDir
  pure $ dir <> "target/riscv32imc-unknown-none-elf/release"

bittideDir :: IO FilePath
bittideDir = do
  current <- getCurrentDirectory
  let paths = L.reverse $ splitDirectories current
      nthParent = L.length $ L.takeWhile (/= "bittide") paths

  pure $ L.concat $ L.replicate nthParent "../"


readDeviceTree :: String -> Q [BitVector 8]
readDeviceTree name =
  runIO $ do
    deviceTreePath <- getDataFileName $ "devicetree" </> (name <> ".dts")
    compileRes <- compileDeviceTreeSource deviceTreePath

    deviceTreeRaw <- maybe exitFailure pure compileRes

    let
      padding = L.replicate (4 - (BS.length deviceTreeRaw `mod` 4)) 0

    pure $ fmap pack . BS.unpack $ deviceTreeRaw <> BS.pack padding

data ElfFileConfig where
  ElfFileConfig ::
    (KnownNat iSize, KnownNat dSize, 1 <= iSize, 1 <= dSize) =>
    BitVector 32 -> -- ^ iMem start addr
    SNat iSize ->
    MemBlob iSize (4 * 8) ->
    BitVector 32 -> -- ^ dMem start addr
    SNat dSize ->
    MemBlob dSize (4 * 8) ->
    BitVector 32 -> -- ^ entry point
    ElfFileConfig

processElfFile :: FilePath -> String -> Q Exp
processElfFile path fdtName = do
  fdt <- readDeviceTree fdtName
  targetDir <- runIO elfTargetDir
  elf <- runIO $ BS.readFile $ targetDir </> path

  let
    (entry, iMem0, dMem0) = readElfFromMemory elf
    dMem1 = dMem0 `I.union` I.fromAscList (L.zip [fdtAddr..] fdt)
    (iStartAddr, iMem1) = memContentToFlatVec iMem0
    (dStartAddr, dMem2) = memContentToFlatVec dMem1

  let iMemVec = memBlobTH Nothing iMem1
      dMemVec = memBlobTH Nothing dMem2

  [| ElfFileConfig iStartAddr SNat ($iMemVec) dStartAddr SNat ($dMemVec) entry |]

memContentToFlatVec :: I.IntMap (BitVector 8) -> (BitVector 32, [Bytes 4])
memContentToFlatVec dataMap = (resize . bitCoerce $ startAddr, toBytes4 content)
 where
  ordList = I.toAscList dataMap
  startAddr = fst $ L.head ordList

  content =
    snd (L.head ordList) : flattenContent startAddr (L.tail ordList)

  flattenContent _ [] = []
  flattenContent prevAddr ((nextAddr, val):vals) =
    let
      n = nextAddr - prevAddr - 1
      padding = L.replicate n 0
    in padding L.++ (val : flattenContent nextAddr vals)

  toBytes4 :: [Bytes 1] -> [Bytes 4]
  toBytes4 [] = []
  toBytes4 [!a] = [bitCoerce (a, 0 :: Bytes 3)]
  toBytes4 [!a, !b] = [bitCoerce (a, b, 0 :: Bytes 2)]
  toBytes4 [!a, !b, !c] = [bitCoerce (a, b, c, 0 :: Bytes 1)]
  toBytes4 ((!a):(!b):(!c):(!d):rest) = bitCoerce (a, b, c, d) : toBytes4 rest
