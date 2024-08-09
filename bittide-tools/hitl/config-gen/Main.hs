-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PackageImports #-}

{- | Program that writes YAML configuration files to '_build/hitl', to be used
by the TCL using Vivado to run hardware-in-the-loop tests.

By default, it writes all known files. If given an identifier, it will only
write that one.
-}
module Main where

import Clash.Prelude (BitPack)
import Prelude

import Control.Monad (forM, forM_, when)
import Data.Aeson (ToJSON)
import Data.List (intercalate)
import Options.Applicative
import Paths.Bittide.Instances (getDataFileName)
import System.Directory (createDirectoryIfMissing)
import System.Exit (die)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)

import Bittide.Hitl (HitlTestsWithPostProcData, packAndEncode)
import Bittide.Instances.Hitl.Tests (HitlTest (..), hitlTests)

import Data.ByteString.Lazy.Char8 qualified as LazyByteString

data Config = Config
  { name :: String
  , yaml :: LazyByteString.ByteString
  }

-- | Known configurations that can be written to @_build/hitl@
configs :: IO [Config]
configs = forM hitlTests $ \case
  KnownType nm config -> pure $ makeConfig nm config
  LoadConfig nm fileName -> loadConfig nm fileName

-- | First argument on command line, as Haskell type
data Arg
  = Write
      { fqn :: Maybe String
      -- ^ Fully qualified name of HITL YAML to render. If 'Nothing', render all
      -- known identifiers
      }
  | List

-- | Be verbose to stderr?
type Verbose = Bool

-- | First argument passed on command line parser
argParser :: Parser (Verbose, Arg)
argParser = (,) <$> verbose <*> arg
 where
  verbose = switch (long "verbose" <> short 'v' <> help "Whether to be verbose")

  arg =
    subparser $
      command
        "write"
        (info writeConfigsParser (progDesc "Write all known configs to _build/hitl"))
        <> command
          "list"
          (info (pure List) (progDesc "List all known configs stdout"))

-- | Parser for the write command, now expecting an optional identifier
writeConfigsParser :: Parser Arg
writeConfigsParser =
  fmap Write $
    optional $
      strArgument $
        metavar "FULLY_QUALIFIED_NAME"
          <> help "For example, 'Bittide.Instances.Hitl.FincFdec.fincFdecTests'"

-- | Load config from an existing YAML file in 'data/test_configs'
loadConfig :: String -> FilePath -> IO Config
loadConfig nm fileName = do
  fullPath <- getDataFileName ("data" </> "test_configs" </> fileName)
  yamlContents <- LazyByteString.readFile fullPath
  pure $
    Config
      { name = nm
      , yaml = yamlContents
      }

-- | Create config from a known HITL test.
makeConfig ::
  forall a b.
  (BitPack a, ToJSON b) =>
  String ->
  HitlTestsWithPostProcData a b ->
  Config
makeConfig nm config =
  Config
    { name = nm
    , yaml = packAndEncode config
    }

main :: IO ()
main = do
  let buildDir = "_build/hitl"
  createDirectoryIfMissing True buildDir
  configs1 <- configs

  customExecParser parserPrefs opts >>= \case
    -- Write all configs
    (verbose, Write Nothing) -> do
      forM_ configs1 $ \Config{name, yaml} -> do
        let path = buildDir </> name <> ".yml"
        when verbose $ hPutStrLn stderr $ "Writing " <> path <> ".."
        LazyByteString.writeFile path yaml

    -- Write specific config
    (verbose, Write (Just fqn)) -> do
      let
        matchedConfig = filter (\Config{name} -> name == fqn) configs1
        names = intercalate "\n" (map name configs1)

      case matchedConfig of
        [] -> die $ "No config found for '" <> fqn <> "'. Available: \n\n" <> names
        (_ : _ : _) -> die $ "Multiple configs matched '" <> fqn <> "'"
        [Config{name, yaml}] -> do
          let path = buildDir </> name <> ".yml"
          when verbose $ hPutStrLn stderr $ "Writing " <> path <> ".."
          LazyByteString.writeFile path yaml
    (_verbose, List) -> do
      forM_ configs1 $ \Config{name} ->
        putStrLn name
 where
  parserPrefs =
    prefs $
      showHelpOnError
        <> showHelpOnEmpty
        <> noBacktrack

  opts =
    info
      (argParser <**> helper <**> versionOption)
      (fullDesc <> progDesc "HITL config rendering")

  versionOption = infoOption "1.0" (long "version" <> help "Show version")
