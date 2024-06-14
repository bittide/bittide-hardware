{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
module Protocols.MemoryMap.Check.AbsAddress where

import Prelude

import Protocols.MemoryMap
import Data.Maybe (fromJust, catMaybes)
import GHC.Stack (SrcLoc)

type AddressWidth = Integer


makeAbsolute :: Address -> MemoryMapTree -> MemoryMapTree
makeAbsolute currAddr mmap =
  case mmap of
    DeviceInstance loc _ name typeName -> DeviceInstance loc (Just currAddr) name typeName
    Interconnect loc _ comps ->
      let
        makeAbsComp (addr, size, comp) =
          (addr, size, makeAbsolute (currAddr + addr) comp)
        comps' = makeAbsComp <$> comps
      in
      Interconnect loc (Just currAddr) comps'


-- | Check if a (guaranteed 'Just') address unifies with a
-- user specified 'AbsoluteAddress' 
checkAbsAddr :: AbsoluteAddress -> AbsoluteAddress -> Bool
checkAbsAddr (Just _) Nothing = True
checkAbsAddr (Just a) (Just b) = a == b
checkAbsAddr Nothing _ = error "shouldn't happen"

data AbsAddressValidateError = AbsAddressValidateError
  { path :: ComponentPath
  , componentName :: Maybe String
  , expected :: Integer
  , got :: Integer
  , location :: SrcLoc
  } deriving (Show)

validateAbsAddresses ::
  ComponentPath ->
  MemoryMapTree ->
  MemoryMapTree ->
  [AbsAddressValidateError]
validateAbsAddresses path (DeviceInstance loc a name _) (DeviceInstance _ b _ _) =
  if checkAbsAddr a b then
    []
  else
    [AbsAddressValidateError
          { path = path
          , componentName = Just name
          , expected = fromJust a
          , got = fromJust b
          , location = loc
          }]
validateAbsAddresses path (Interconnect loc a compsA) (Interconnect _ b compsB) =
  if checkAbsAddr a b then
    results
  else
    AbsAddressValidateError
      { path = path
      , componentName = Nothing
      , expected = fromJust a
      , got = fromJust b
      , location = loc
      } : results
  where
    results = concatMap checkComps comps
    comps = zip3 [0..] compsA compsB
    checkComps (i, (_, _, compA'), (_, _, compB')) =
      let
        path' = InterconnectComponent i path
        compRes = validateAbsAddresses path' compA' compB'
      in compRes


validateAbsAddresses _ _ _ = error "should not happen"