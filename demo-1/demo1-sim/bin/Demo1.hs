{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
import Prelude hiding (repeat)
import Clash.Prelude hiding (Exp)

import Clash.Sized.Vector

import Bittide.Calendar
import Bittide.DoubleBufferedRam
import Bittide.Link
import Bittide.Node
import Bittide.ProcessingElement
import Bittide.ScatterGather
import Bittide.Switch

import qualified Data.List as L
import Control.Monad (forM_)

import TH (processElfFile, ElfFileConfig (ElfFileConfig))
import Bittide.SharedTypes (DataLink)


main :: IO ()
main = do
  putStrLn "Demo 1"

  let
    output = simulate_lazy (nodeCircuit clockGen resetGen) (L.repeat $ repeat Nothing)

  forM_ (L.take 11 output) $ \case
    (Nothing :> Nil) -> pure ()
    (Just _ :> Nil) -> putStrLn "something??"


nodeCircuit
  :: "CLK" ::: Clock System
  -> "RST" ::: Reset System
  -> "IN_LINKS" ::: Signal System (Vec 1 (DataLink 64))
  -> "OUT_LINKS" ::: Signal System (Vec 1 (DataLink 64))
nodeCircuit clk rst (unbundle -> inLinks) =
  let
    gppeMemMap :: Vec 2 (BitVector 32) -> Vec 4 (BitVector 32)
    gppeMemMap (iMem :> dMem :> Nil) = iMem :> dMem :> dMem + 234324 :> dMem + 3434343 :> Nil

    managementMemMap :: Vec 2 (BitVector 32) -> Vec 25 (BitVector 32)
    managementMemMap (iMem :> dMem :> Nil) = iMem :> dMem :> fmap (+32768) (iterateI succ dMem)


    pingPeConfig = peConfigFromElfConfig gppeMemMap $(processElfFile "ping" "gppe")
    pongPeConfig = peConfigFromElfConfig gppeMemMap $(processElfFile "pong" "gppe")
    managementPeConfig = peConfigFromElfConfig managementMemMap $(processElfFile "management" "management")

    switchConfig = SwitchConfig preamble switchCal
    switchCal = CalendarConfig (SNat @1024) (repeat @1 $ repeat 0) (repeat @1 $ repeat 0)
    linkConfig = LinkConfig preamble (ScatterConfig sgConfig) (GatherConfig sgConfig)
    sgConfig = CalendarConfig (SNat @1024) (repeat @1 (0 :: Index 1024)) (repeat @1 0)
    preamble = 0x1234567890 :: BitVector 80

    nodeConfig :: NodeConfig 1 2
    nodeConfig =
        NodeConfig
          (ManagementConfig linkConfig managementPeConfig)
          switchConfig
          (GppeConfig linkConfig pingPeConfig :> GppeConfig linkConfig pongPeConfig :> Nil)

  in
    bundle $ withClockResetEnable clk rst enableGen $ node nodeConfig inLinks

peConfigFromElfConfig :: (Vec 2 (BitVector 32) -> Vec n (BitVector 32)) -> ElfFileConfig -> PeConfig n
peConfigFromElfConfig mmapFn (ElfFileConfig iStartAddr iSize iMem dStartAddr dSize dMem entry) =
  PeConfig
    (mmapFn (iStartAddr :> dStartAddr :> Nil))
    iSize
    dSize
    (Reloadable $ memblobToVec iMem)
    (NonReloadable $ memblobToVec dMem)
    entry

memblobToVec :: (KnownNat n, KnownNat m) => MemBlob n m -> Vec n (BitVector m)
memblobToVec blob = unsafeFromList $ unpackMemBlob blob
