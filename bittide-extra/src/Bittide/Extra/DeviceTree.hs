-- SPDX-FileCopyrightText: 2023 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE TemplateHaskell #-}
{- | The module offers compile-time access to the entries of device tree
 specification files. Files with a @.dts@-ending that are listed under
 the @extra-source-files@ field of the project's cabal file are
 automatically loaded at compile-time and are available within this
 interface.

 The API of this module then allows to query data from these files via
 selection of the desired specification file (first argument) and the
 path to the desired property within this file (second argument). The
 data can either be read directly or spliced in using Template
 Haskell. The latter also allows to splice in property values as type
 level 'GHC.TypeLit.Nat's or 'GHC.TypeLit.Symbol's.
-}
module Bittide.Extra.DeviceTree
  ( -- * Device Tree Specification Data
    Value(..)
    -- * Querying Device Tree Specification Data
  , dtsLookup
  , dtsNum
  , dtsStr
  , dtsStrList
  , dtsArray
    -- * Splicing Device Tree Specification Data
    -- ** As Expressions
  , dtsNumE
  , dtsStrE
  , dtsStrListE
  , dtsArrayE
    -- ** On the Type-Level
  , dtsNumT
  , dtsStrT
  ) where

import Prelude
import Control.Arrow (second)
import Control.Monad ((>=>), forM)
import Data.Bits ((.|.), shift)
import Data.Either (partitionEithers)
import Data.List (find)
import Data.Map (Map)
import qualified Data.Map as M (lookup, fromList)
import Data.Maybe (fromMaybe)
import Data.ByteString (ByteString, pack, unpack)
import Language.Haskell.TH.Syntax (Q, Type(..), Exp(..), TyLit(..), Lit(..), runIO, lift)
import Text.Read (readMaybe)

import qualified Bittide.Extra.DeviceTree.Parser as Parser
import Bittide.Extra.DeviceTree.Source (dtsSources)

-- | Container for the parsed device tree specification.
data DTS =
  DTS
    { version  :: Integer
      -- ^ the version @\<X\>@ as specified by the @\/dts-v\<X\>\/;@
      -- header
    , source   :: FilePath
      -- ^ the file path to the parsed device tree specification
    , reserved :: [(Integer, Integer)]
      -- ^ memory reservation requirements, where
      --
      -- * - the first element indicates the address and
      -- * - the second element the length
    , entries  :: Map String [Value]
      -- ^ the paths of tree associated with their corresponding values
    }
  deriving (Show, Ord, Eq)

-- | Container for property values. Note that @\<u32\>@ and @\<u64\>@
-- integers are represented as singleton and two-element arrays,
-- respectively. Mixed property values are represented as 'Value'
-- lists, where the empty list represents the @\<empty\>@ property
-- value.
data Value =
    String String
    -- ^ @"@...@"@-encoded values
  | ByteString ByteString
    -- ^ @[@...@]@-encoded values (always non-empty)
  | Array [Integer]
    -- ^ @\<@...@\>@-encoded values (always non-empty)
  deriving (Eq, Ord)

instance Show Value where
  show = \case
    String str     -> show $ Parser.DtsString str
    ByteString bs  -> show $ Parser.DtsByteString $ unpack bs
    Array xs       -> show $ Parser.DtsArray xs

dts :: [DTS]
dts = map conv
  $( runIO $ do
       sources <- dtsSources
       ListE <$> forM sources (Parser.readDTS >=> lift)
   )
 where
   conv Parser.DTS{..} =
     DTS
       { entries = M.fromList $ map (second $ map vConv) entries
       , ..
       }
   vConv = \case
     Parser.DtsString str    -> String str
     Parser.DtsByteString xs -> ByteString $ pack xs
     Parser.DtsArray xs      -> Array xs

-- | Generic lookup of any kind of device tree specification data. The
-- first argument selects one of the device tree specifications listed
-- in the project's cabal file. The second argument specifies the path
-- to the requested property of the specification. The function errors
-- if the given @.dts@ file is not listed in the project's cabal or if
-- the requested property does not exist.
dtsLookup :: FilePath -> String -> [Value]
dtsLookup file path = case find ((== file) . source) dts of
  Nothing      -> error $ "File not found: " <> file
  Just DTS{..} -> fromMaybe notFound $ M.lookup path entries
 where
  notFound = error $ "'" <> file <> "' has no entry at: " <> path

-- | Looks up an 'Integer' property value.
dtsNum :: String -> String -> Integer
dtsNum file path = case dtsLookup file path of
  [] -> error $ file <> ": no value bound at '" <> path <> "'"
  [String (readMaybe -> Just x)] -> x
  [ByteString (unpack -> [x])] -> toInteger x
  [Array [x]] -> x
  [Array [x,y]] -> shift x 32 .|. y
  _ -> error $ file <> ": value at '" <> path <> "' is not a number"

-- | TH for splicing an 'Integer' property as an expression.
dtsNumE :: FilePath -> String -> Q Exp
dtsNumE file = pure . LitE . IntegerL . dtsNum file

-- | TH for splicing an 'Integer' property as a type.
dtsNumT :: FilePath -> String -> Q Type
dtsNumT file = pure . LitT . NumTyLit . dtsNum file

-- | Looks up a 'GHC.Base.String' property value.
dtsStr :: FilePath -> String -> String
dtsStr file path = case dtsLookup file path of
  [] -> ""
  [String x] -> x
  [x] -> show x
  _ -> error $ file <> ": multiple values bound at " <> path

-- | TH for splicing a 'GHC.Base.String' property as an expression.
dtsStrE :: FilePath -> String -> Q Exp
dtsStrE file = pure . LitE . StringL . dtsStr file

-- | TH for splicing an 'GHC.Base.String' property as a type.
dtsStrT :: FilePath -> String -> Q Type
dtsStrT file = pure . LitT . StrTyLit . dtsStr file

-- | Looks up a ['GHC.Base.String'] property value.
dtsStrList :: FilePath -> String -> [String]
dtsStrList file path =
  case partitionEithers $ map rightString $ dtsLookup file path of
    ([], xs) -> xs
    _ -> error $ file <> ": value at '" <> path <> "' is not a string list"
 where
  rightString = \case
    String y -> Right y
    _        -> Left ()

-- | TH for splicing an ['GHC.Base.String'] property as an expression.
dtsStrListE :: FilePath -> String -> Q Exp
dtsStrListE file = pure . ListE . map (LitE . StringL) . dtsStrList file

-- | Looks up a ['Integer'] property value.
dtsArray :: FilePath -> String -> [Integer]
dtsArray file path = case dtsLookup file path of
  [Array x] -> x
  _ -> error $ file <> ": value at '" <> path <> "' is not a single array"

-- | TH for splicing an ['Integer'] property as an expression.
dtsArrayE :: FilePath -> String -> Q Exp
dtsArrayE file = pure . ListE . map (LitE . IntegerL) . dtsArray file
