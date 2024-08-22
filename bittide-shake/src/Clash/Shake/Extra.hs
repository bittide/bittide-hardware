-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{- | Shake utilities related to Clash. Although 'clash-shake' already exists, it
assumes Clash runs in non-project mode. We should discuss with the author if
he sees a way of upstreaming the code in this module.
-}
module Clash.Shake.Extra where

import Prelude

import Bittide.Hitl (ClashTargetName)
import Clash.Annotations.Primitive (HDL (Verilog))
import Data.Char (toLower)
import Development.Shake
import Development.Shake.FilePath ((</>))

import qualified Crypto.Hash.SHA256 as Sha256
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Lazy as ByteStringLazy
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding

hdlToFlag :: HDL -> String
hdlToFlag = ("--" <>) . map toLower . show

-- | Calculate a SHA256 hex digest of a given file path
hexDigestFile :: FilePath -> IO String
hexDigestFile path = do
  contents <- liftIO (ByteStringLazy.readFile path)
  pure $
    Text.unpack $
      Encoding.decodeUtf8 $
        Base16.encode $
          Sha256.hashlazy $
            contents

{- | Generate command to run and arguments to supply for a top entity passed as
a @TemplateHaskell@ name. Generates the expected location of a Clash manifest
file.
-}
clashCmd ::
  -- | Build directory
  FilePath ->
  -- | HDL to compile to
  HDL ->
  -- | Entity to compile
  ClashTargetName ->
  -- | Extra arguments to pass to Clash
  [String] ->
  -- | (command, arguments)
  (String, [String])
clashCmd buildDir hdl topName extraArgs =
  ( "cabal"
  , [ "run"
    , pkgName <> ":clash"
    , "--"
    , modName
    , "-fclash-hdldir"
    , buildDir
    , "-main-is"
    , funcName
    , hdlToFlag hdl
    , "-fclash-clear"
    , "-fclash-spec-limit=100"
    , "-fclash-debug"
    , "DebugSilent"
    ]
      <> extraArgs
  )
 where
  (modName, funcName) = splitName topName
  pkgName = "bittide-instances"

-- | Split a 'ClashTargetName' into the fully qualified module name and the function name.
splitName :: ClashTargetName -> (String, String)
splitName qualifiedName =
  let (f, m) = break (== '.') $ reverse $ show qualifiedName
   in (reverse $ tail m, reverse f)

entityName :: ClashTargetName -> String
entityName = snd . splitName

moduleName :: ClashTargetName -> String
moduleName = fst . splitName

defaultClashCmd :: FilePath -> ClashTargetName -> (String, [String])
defaultClashCmd buildDir topName = clashCmd buildDir Verilog topName []

-- | Given a 'ClashTargetName', return expected location of Clash manifest file.
getManifestLocation :: FilePath -> ClashTargetName -> String
getManifestLocation buildDir topName =
  buildDir </> show topName </> "clash-manifest.json"
