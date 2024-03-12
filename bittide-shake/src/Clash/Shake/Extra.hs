-- SPDX-FileCopyrightText: 2022-2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

-- | Shake utilities related to Clash. Although 'clash-shake' already exists, it
-- assumes Clash runs in non-project mode. We should discuss with the author if
-- he sees a way of upstreaming the code in this module.
--
module Clash.Shake.Extra where

import Prelude

import Clash.Annotations.Primitive (HDL (Verilog))
import Data.Char (toLower)
import Development.Shake
import Development.Shake.FilePath ((</>))

import qualified Crypto.Hash.SHA256 as Sha256
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Lazy as ByteStringLazy
import qualified Data.Text.Encoding as Encoding
import qualified Data.Text as Text

hdlToFlag :: HDL -> String
hdlToFlag = ("--" <>) . map toLower . show

-- | Calculate a SHA256 hex digest of a given file path
hexDigestFile :: FilePath -> Action String
hexDigestFile path = do
  need [path]
  contents <- liftIO (ByteStringLazy.readFile path)
  pure
    $ Text.unpack
    $ Encoding.decodeUtf8
    $ Base16.encode
    $ Sha256.hashlazy
    $ contents

-- | Generate command to run and arguments to supply for a top entity passed as
-- a @TemplateHaskell@ name. Generates the expected location of a Clash manifest
-- file.
clashCmd ::
  -- | Build directory
  FilePath ->
  -- | HDL to compile to
  HDL ->
  -- | Entity to compile
  TargetName ->
  -- | Extra arguments to pass to Clash
  [String] ->
  -- (command, arguments)
  (String, [String])
clashCmd buildDir hdl topName extraArgs =
  ( "cabal"
  , [ "run"
    , pkgName <> ":clash"
    , "--"
    , modName
    , "-fclash-hdldir", buildDir
    , "-main-is", funcName
    , hdlToFlag hdl
    , "-fclash-clear"
    , "-fclash-spec-limit=100"
    , "-fclash-debug", "DebugSilent"
    ] <> extraArgs
  )
 where
  (modName, funcName) = splitName topName
  pkgName = "bittide-instances"

-- | Fully qualified name to a function. E.g. @Bittide.Foo.topEntity@.
type TargetName = String

-- | Split a 'TargetName' into the fully qualified module name and the function name.
splitName :: TargetName -> (String, String)
splitName qualifiedName =
    let (f, m) = break (== '.') $ reverse qualifiedName
    in (reverse $ tail m, reverse f)

entityName :: TargetName -> String
entityName = snd . splitName

moduleName :: TargetName -> String
moduleName = fst . splitName

defaultClashCmd :: FilePath -> TargetName -> (String, [String])
defaultClashCmd buildDir topName = clashCmd buildDir Verilog topName []

-- | Given a 'TargetName', return expected location of Clash manifest file.
getManifestLocation :: FilePath -> TargetName -> String
getManifestLocation buildDir topName =
  buildDir </> topName </> "clash-manifest.json"
