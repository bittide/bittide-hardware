-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

import Prelude

import Clash.Prelude
  ( KnownNat, Nat, Vec(..), BitVector, Index, BitPack
  , type (<=), pack
  )

import Data.Yaml
  ( ToJSON(..), FromJSON(..), Parser, Value(..), Object
  , (.!=), object, withObject, encodeFile, decodeFileEither
  , prettyPrintParseException
  )

import System.FilePath
  ( (</>), takeFileName, takeBaseName, replaceExtension, normalise
  )

import Control.Applicative (Alternative(..))
import Control.Monad.Extra (unless, ifM, forM)
import Data.Aeson.Types (Key, (.:!), explicitParseFieldMaybe')
import Data.Default (Default(..))
import Data.List (intercalate)
import Data.Maybe (catMaybes)
import Data.Proxy (Proxy(..))
import Data.Type.Equality ((:~:)(..))
import GHC.TypeLits.Compare ((:<=?)(..))
import GHC.TypeLits.Witnesses ((%<=?), SNat(..))
import Paths_bittide_instances (getDataFileName)
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.IO (hPutStr, stderr)

import qualified Clash.Sized.Vector as Vec
import qualified Data.Aeson.Key     as Key
import qualified Data.Aeson.KeyMap  as KeyMap
import qualified Data.Set           as Set
import qualified Data.String        as String

import Bittide.Instances.Hitl
import Bittide.Instances.Hitl.Setup
import Bittide.Instances.Hitl.BoardTest
import Bittide.Instances.Hitl.FincFdec

-- | Newtype wrapper for converting bit vectors of known size to JSON
-- objects.
newtype YamlPack (n :: Nat) = YamlPack { yamlPack :: BitVector n }

instance KnownNat n => ToJSON (YamlPack n) where
  toJSON = String . String.fromString . show . yamlPack

-- | Collection of test cases associated with the same type interface.
data TestGroup a where
  TestGroup ::
    (HitltConfig a testnames, KnownNat (TestCount testnames)) =>
    Vec (TestCount testnames) (Vec FpgaCount (Maybe a)) ->
    TestGroup a

instance (Default a, HitltConfig a testnames, KnownNat (TestCount testnames)) =>
  Default (TestGroup a)
 where
  def = TestGroup $ Vec.repeat $ Vec.repeat $ Just def

instance ToJSON (TestGroup a)
 where
  toJSON (TestGroup tests) = case testNamesFromType (Proxy @a) of
    (_ :: Proxy testnames) -> object $
      let testKeys = Vec.toList $ Key.fromString <$> testNames @testnames Proxy
       in case SNat @1 %<=? SNat @(TestCount testnames) of
            NLE {} ->
              [("simple_test", object ((, Array empty) <$> fpgaIdKeys))]
            LE  Refl ->
              catMaybes $ zipWith ((<$>) . (,)) testKeys $ toValue
                <$> zip [0 :: Index (TestCount testnames), 1 ..]
                      (Vec.toList <$> Vec.toList tests)
   where
    testNamesFromType :: HitltConfig a names => Proxy a -> Proxy names
    testNamesFromType _ = Proxy

    fpgaIdKeys =
      Vec.toList $ Key.fromString . fst <$> fpgaSetup

    toValue ::
      (KnownNat n, 1 <= n, BitPack b) =>
      (Index n, [Maybe b]) -> Maybe Value
    toValue = mobject . catMaybes . zipWith ((<$>) . (,)) fpgaIdKeys
      . (\(i, xs) -> map (toJSON . YamlPack . pack . (i, ) <$>) xs)

    mobject = \case
      [] -> Nothing
      x  -> Just $ object x

instance (HitltConfig a testnames, KnownNat (TestCount testnames)) =>
  FromJSON (TestGroup a)
 where
  parseJSON = withObjectAndKeys [] testKeys
    $ (TestGroup . Vec.unsafeFromList <$>) . forM testKeys . toTest
   where
    testKeys = Vec.toList $ Key.fromString <$> testNames (Proxy @testnames)
    fpgaIdKeys = Key.fromString . show <$> Vec.toList (Vec.indicesI @FpgaCount)

    toTest o testname =
      explicitParseFieldMaybe' (parseTest [Key.toString testname]) o testname
        .!= Vec.repeat (Just def)

    parseTest ref = withObjectAndKeys ref ["default", "disable", "fpga"] $ \o ->
      ifM (o .:! "disable" .!= False) (pure $ Vec.repeat Nothing) $ do
        newDef <- o .:! "default" .!= def
        let parser = parseId newDef $ "fpga" : ref
        explicitParseFieldMaybe' parser o "fpga" .!= Vec.repeat (Just newDef)

    parseId newDef ref = withObjectAndKeys ref fpgaIdKeys $ \o ->
      fmap Vec.unsafeFromList $ forM fpgaIdKeys $ \i ->
        let parser = parseConfig newDef $ Key.toString i : ref
         in explicitParseFieldMaybe' parser o i .!= Just newDef

    parseConfig newDef ref = withObjectAndKeys ref ["data", "disable"] $ \o ->
      ifM (o .:! "disable" .!= False)
        (return Nothing)
        (Just <$> o .:! "data" .!= newDef)

    withObjectAndKeys ::
      [String] -> [Key] -> (Object -> Parser b) -> Value -> Parser b
    withObjectAndKeys ref keys action =
      withObject (intercalate "." $ reverse ref) $ \o ->
        supportedKeys (null ref) o keys >> action o

    supportedKeys isroot o (Set.fromList -> supported) =
      let showKey = ("  " <>) . Key.toString
          keys = Set.fromList $ KeyMap.keys o
          unknown = Set.toList $ Set.difference keys supported
          entry = if isroot then "test case" else "key"
          multiple = if length unknown == 1 then "" else "s"
       in unless (keys `Set.isSubsetOf` supported || null (supported <> keys))
            $ fail $ unlines $ concat
                [ ["", "", "Unknown " <> entry <> multiple <> ":"]
                , showKey <$> unknown
                , ["", "Supported " <> entry <> "s are:"]
                , showKey <$> Set.toList supported
                ]

-- | Reads the given source YAML configuration and converts it into
-- the packed version. If the source file does not exist, then the
-- default instance for the associated type is used instead.
ypack ::
  forall a testnames.
  (HitltConfig a testnames, KnownNat (TestCount testnames)) =>
  (?srcFile :: FilePath, ?dstFile :: FilePath) =>
  IO ()
ypack = ifM (doesFileExist ?srcFile) (decodeFileEither ?srcFile) (pure $ Right def)
  >>= either (hPutStr stderr . printError) (encodeFile @(TestGroup a) ?dstFile)
 where
  printError err =
    "\"" <> takeFileName ?srcFile <> "\" - " <> prettyPrintParseException err

-- | Creates packed YAML configuration files at all file paths
-- requested via the command line. If a YAML configuration file with
-- the same name exists in @data/hitltconfigs/@ then this file gets
-- packed instead of the default.
main :: IO ()
main = getArgs >>= mapM_ packFile
 where
  packFile dstFile = do
    srcFile <- fmap normalise $ getDataFileName $
      "data" </> "hitltconfigs" </> takeFileName (replaceExtension dstFile ".yml")

    let ?srcFile = srcFile
    let ?dstFile = dstFile

    case takeBaseName srcFile of
      "boardTestExtended" -> ypack @BoardTestExtended
      "fincFdecTests"     -> ypack @FIncFDecTest
      _                   -> ypack @SimpleTest
