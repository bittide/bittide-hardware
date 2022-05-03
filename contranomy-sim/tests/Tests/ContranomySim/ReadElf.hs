-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Tests.ContranomySim.ReadElf where

import qualified Data.ByteString       as BS
import qualified Data.List             as L
import           Prelude

import           Test.Tasty            (TestTree, testGroup)
import           Test.Tasty.HUnit      (assertEqual, testCase, (@?=))

import           ContranomySim.ReadElf (readElf)
import           Data.Elf
import           Data.IntMap           as I


riscvElfEmpty :: Elf
riscvElfEmpty = Elf
            { elfClass = ELFCLASS32
            , elfData = ELFDATA2LSB
            , elfVersion = 1
            , elfOSABI = ELFOSABI_SYSV
            , elfABIVersion = 1
            , elfType = ET_EXEC
            , elfMachine = EM_EXT 0xF3 -- RISC-V
            , elfEntry = 0x80000000
            , elfSections = []
            , elfSegments = []
            }

textSection :: ElfSection
textSection = ElfSection
                { elfSectionName = ".text"
                , elfSectionType = SHT_PROGBITS
                , elfSectionFlags = [SHF_ALLOC, SHF_EXECINSTR]
                , elfSectionAddr = 0x80000000
                , elfSectionSize = 0
                , elfSectionLink = 0
                , elfSectionInfo = 0
                , elfSectionAddrAlign = 0x00010000
                , elfSectionEntSize = 0
                , elfSectionData = BS.empty
                }

dataSection :: ElfSection
dataSection = ElfSection
                { elfSectionName = ".data"
                , elfSectionType = SHT_PROGBITS
                , elfSectionFlags = [SHF_ALLOC, SHF_WRITE]
                , elfSectionAddr = 0x80000000
                , elfSectionSize = 0
                , elfSectionLink = 0
                , elfSectionInfo = 0
                , elfSectionAddrAlign = 0x00010000
                , elfSectionEntSize = 0
                , elfSectionData = BS.empty
                }

rodataSection :: ElfSection
rodataSection = dataSection
                { elfSectionName = ".rodata"
                , elfSectionFlags = [SHF_ALLOC]
                }

instrSegment :: ElfSegment
instrSegment = ElfSegment
                { elfSegmentType = PT_LOAD
                , elfSegmentFlags = [PF_R, PF_X]
                , elfSegmentVirtAddr = 0x80000000
                , elfSegmentPhysAddr = 0x80000000
                , elfSegmentAlign = 0x00010000
                , elfSegmentData = BS.empty
                , elfSegmentMemSize = 0
                }

dataSegment :: ElfSegment
dataSegment = ElfSegment
                { elfSegmentType = PT_LOAD
                , elfSegmentFlags = [PF_R, PF_W]
                , elfSegmentVirtAddr = 0x80000000
                , elfSegmentPhysAddr = 0x80000000
                , elfSegmentAlign = 0x00010000
                , elfSegmentData = BS.empty
                , elfSegmentMemSize = 0
                }


tests :: TestTree
tests = testGroup "Read ELF Tests"
  [ testCase "ELF file empty" $ do
      let elf = riscvElfEmpty

      let (entry, iMem, dMem) = readElf elf

      elfEntry elf @?= fromIntegral entry
      iMem @?= I.fromList []
      dMem @?= I.fromList []
  , testCase "ELF file, only .text" $ do
      let iData = L.replicate 100 0xAB

      let elf = riscvElfEmpty
            { elfSections = [
                textSection
                { elfSectionAddr = 0x80000000
                , elfSectionSize = fromIntegral $ L.length iData
                , elfSectionData = BS.pack iData
                }
              ]
            , elfSegments = [
                instrSegment
                { elfSegmentVirtAddr = 0x80000000
                , elfSegmentPhysAddr = 0x80000000
                , elfSegmentData = BS.pack iData
                , elfSegmentMemSize = fromIntegral $ L.length iData
                }
              ]
            }

      let (entry, iMem, dMem) = readElf elf

      let iDataMap = I.fromList (L.zip [0x80000000..] (fromIntegral <$> iData))

      elfEntry elf @?= fromIntegral entry
      assertEqual "instruction memory contains instruction data" iDataMap (I.intersection iMem iDataMap)
      dMem @?= I.fromList []

    , testCase "ELF file, .data and .rodata" $ do
      let data' = L.replicate 100 0xAB
      let roData = L.replicate 50 0x0F

      let elf = riscvElfEmpty
            { elfSections =
                [ dataSection
                  { elfSectionAddr = 0x80000000
                  , elfSectionSize = fromIntegral $ L.length data'
                  , elfSectionData = BS.pack data'
                  }
                , rodataSection
                  { elfSectionAddr = 0x80000000 + fromIntegral (L.length data')
                  , elfSectionSize = fromIntegral $ L.length roData
                  , elfSectionData = BS.pack roData
                  }
                ]
            , elfSegments = [
                dataSegment
                { elfSegmentVirtAddr = 0x80000000
                , elfSegmentPhysAddr = 0x80000000
                , elfSegmentData = BS.pack (data' <> roData)
                , elfSegmentMemSize = fromIntegral $ L.length data' + L.length roData
                }
              ]
            }

      let (entry, iMem, dMem) = readElf elf

      let dataMap = I.fromList (L.zip [0x80000000..] (fromIntegral <$> (data' <> roData)))

      elfEntry elf @?= fromIntegral entry
      iMem @?= I.fromList []
      assertEqual "instruction memory contains instruction data" dataMap (I.intersection dMem dataMap)
  ,  testCase "ELF file, .text and .data" $ do
      let iData = L.replicate 100 0xAB
      let dData = L.replicate 1000 0xB3

      let elf = riscvElfEmpty
            { elfSections = [
                textSection
                { elfSectionAddr = 0x80000000
                , elfSectionSize = fromIntegral $ L.length iData
                , elfSectionData = BS.pack iData
                }
              , dataSection
                { elfSectionAddr = 0x80000000 + fromIntegral (L.length iData)
                , elfSectionSize = fromIntegral $ L.length dData
                , elfSectionData = BS.pack dData
                }
              ]
            , elfSegments = [
                instrSegment
                  { elfSegmentVirtAddr = 0x80000000
                  , elfSegmentPhysAddr = 0x80000000
                  , elfSegmentData = BS.pack iData
                  , elfSegmentMemSize = fromIntegral $ L.length iData
                  }
              , dataSegment
                  { elfSegmentVirtAddr = 0x80000000 + fromIntegral (L.length iData)
                  , elfSegmentPhysAddr = 0x80000000 + fromIntegral (L.length iData)
                  , elfSegmentData = BS.pack dData
                  , elfSegmentMemSize = fromIntegral $ L.length dData
                  }
              ]
            }

      let (entry, iMem, dMem) = readElf elf

      let iDataMap = I.fromList (L.zip [0x80000000..] (fromIntegral <$> iData))
      let dDataMap = I.fromList (L.zip [(0x80000000 + fromIntegral (L.length iData))..] (fromIntegral <$> dData))

      elfEntry elf @?= fromIntegral entry
      assertEqual "instruction memory contains instruction data" iDataMap (I.intersection iMem iDataMap)
      assertEqual "data memory contains data contents" dDataMap (I.intersection dMem dDataMap)

  ]
