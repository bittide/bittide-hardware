-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImplicitPrelude #-}
module Bittide.Github.Artifacts
  ( RunId
  , ArtifactName
  , ArtifactAccessError
  , retrieveArtifact
  ) where

import Control.Exception (throwIO)
import Control.Monad (forM)
import Data.Aeson
  ( FromJSON(..), Value(..), Result(..), (.:)
  , fromJSON, withObject, withArray, decodeFileStrict
  )
import Data.Map.Strict (Map)
import Network.HTTP.Conduit (requestHeaders)
import Network.HTTP.Simple
  ( JSONException(..)
  , getResponseBody, httpJSONEither, parseRequest, getResponseStatus
  )
import Network.HTTP.Types.Header (hUserAgent)
import Network.HTTP.Types.Status (Status(..))
import System.Directory (createDirectoryIfMissing, listDirectory)
import System.Environment (lookupEnv, getProgName)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (callCommand, callProcess)

import qualified Data.Map.Strict       as Map        (fromList, lookup)
import qualified Data.ByteString.Char8 as ByteString (pack)
import qualified Data.Text             as Text       (unpack)
import qualified Data.Vector           as Vector     (toList)

-- | The environment variable used to share the artifact access token.
accessTokenEnvVar :: String
accessTokenEnvVar = "BITTIDE_ARTIFACT_ACCESS_TOKEN"

-- | The Bittide repository on Github.
bittideRepo :: String
bittideRepo = "bittide/bittide-hardware"

-- | Offers the artifacts list of a given run via the Github API.
githubApiArtifacts :: String -> String -> String
githubApiArtifacts repo run = "https://api.github.com/repos/"
  <> repo <> "/actions/runs/" <> run <> "/artifacts?per_page=100"

-- | The unique identifier of the Github Action run.
type RunId = String

-- | The name of the artifact to be downloaded.
type ArtifactName = String

-- | Everything that can go wrong while trying to download an artifact
-- from the Bittide Github repository with 'retrieveArtifact' that has
-- its origin on some invalid user input passed to 'retrieveArtifact'.
data ArtifactAccessError =
    NoAccessToken
  | InvalidAccessToken
  | RunNotFound RunId
  | ArtifactNotFound RunId ArtifactName

instance Show ArtifactAccessError where
  show = \case
    NoAccessToken ->
      "No access token found. A valid access token must be set via\n"
        <> "the " <> accessTokenEnvVar <> " environment variable."
    InvalidAccessToken ->
      "The provided access token has no access to the Bittide artifacts."
    RunNotFound runId ->
      "Invalid run ID \"" <> runId <> "\". Cannot access the data for\n"
        <> "the provided ID."
    ArtifactNotFound runId artifactName ->
      "There is no artifact named \"" <> artifactName <> "\" for the\n"
        <> "run with ID " <> runId <> "."

-- | A newtype wrapper for extracting the "artifact name -> download
-- url" mapping of a run via the Github API.
newtype ArtifactDownloadUrl = ArtifactDownloadUrl (Map String String)

instance FromJSON ArtifactDownloadUrl where
  parseJSON =
    withObject "root" $ \root ->
      (root .: "artifacts" >>=) $
        withArray "artifacts" $ \as ->
          fmap (ArtifactDownloadUrl . Map.fromList) $
            forM (Vector.toList as) $
              withObject "artifact" $ \artifact -> do
                String name <- artifact .: "name"
                String url <- artifact .: "archive_download_url"
                return (Text.unpack name, Text.unpack url)

-- | A newtype wrapper for reading back curl response messages
newtype CurlResponseMessage = CurlResponseMessage String

instance FromJSON CurlResponseMessage where
  parseJSON =
    (CurlResponseMessage . Text.unpack <$>) .
      withObject "root" (.: "message")

-- | Retrieve the artifact with the given name for the given run id
-- and save it at the provided location. An 'ArtifactAccessError' is
-- returned on failure with respect to the provided arguments. If the
-- arguments are valid, but there is some external problem with the
-- utilized process, then that error gets reported via an exception
-- instead.
retrieveArtifact ::
  RunId -> ArtifactName -> FilePath -> IO (Maybe ArtifactAccessError)
retrieveArtifact runId artifactName destination = do
  appName <- getProgName
  request <- parseRequest $ githubApiArtifacts bittideRepo runId
  response <- httpJSONEither request
    { -- Github requires to set the User Agent header, as the request
      -- will always be rejected otherwise
      requestHeaders = [(hUserAgent, ByteString.pack appName)]
    }
  case getResponseBody response of
    Left err@(JSONParseException {}) -> throwIO err
    Left err@(JSONConversionException _ resp _) ->
      if statusCode (getResponseStatus resp ) == 404
      then return $ Just $ RunNotFound runId
      else throwIO err
    Right (ArtifactDownloadUrl downloadUrls) -> do
      case Map.lookup artifactName downloadUrls of
        Nothing -> return $ Just $ ArtifactNotFound runId artifactName
        Just downloadUrl -> lookupEnv accessTokenEnvVar >>= \case
          Nothing -> return $ Just NoAccessToken
          Just accessToken ->
            withSystemTempDirectory "retrieve-artifact" $ \path -> do
              let file = path </> "artifact.zip"
              putStrLn $ "Retrieving " <> artifactName <> ".zip"
              putStrLn "---"
              callCommand $ unwords
                [ "curl"
                , "--location"
                , "--header", "\"authorization: Bearer " <> accessToken <> "\""
                , "--output", file
                , downloadUrl
                ]
              putStr "---"
              -- if the downloaded file is a JSON instead of zip, then
              -- something went wrong
              decodeFileStrict file >>= \case
                Just jsonValue -> do
                  putStrLn " Failed."
                  if case fromJSON jsonValue of
                       Success (CurlResponseMessage msg) ->
                         msg == "Bad credentials"
                       _ -> False
                  then return $ Just InvalidAccessToken
                  else do
                    req <- parseRequest downloadUrl
                    throwIO $ JSONConversionException
                      req (jsonValue <$ response) "curl download failed"
                Nothing -> do
                  putStrLn " Success."
                  callProcess "unzip" ["-q", file, "-d", path]
                  callProcess "rm" [file]
                  createDirectoryIfMissing True destination
                  (listDirectory path >>=) $ mapM_ $ \x -> do
                    callProcess "rm" ["-Rf", destination </> x]
                    callProcess "mv" [path </> x, destination </> x]
                  return Nothing
