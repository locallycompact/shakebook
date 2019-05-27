#!/usr/bin/env runhaskell
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

import           Data.Aeson
import           Data.Aeson.Lens
import           Data.Aeson.Encode.Pretty
import           Lens.Micro
import           Network.Wreq
import           RIO
import           RIO.ByteString      as BS
import qualified RIO.ByteString.Lazy as LBS
import           RIO.Partial
import           RIO.Process
import qualified RIO.Text as T
import           System.Environment

data RepoMeta = RepoMeta {
  owner :: Text
, repo :: Text
, rev :: Text
, sha256 :: Text
} deriving (Generic, Show, Eq, Read)

instance FromJSON RepoMeta
instance ToJSON RepoMeta

getLatestNixPkgs = asValue =<< get "https://api.github.com/repos/NixOS/nixpkgs/git/refs/heads/master"

quickDecode :: LBS.ByteString -> String
quickDecode = T.unpack . T.decodeUtf8With lenientDecode . LBS.toStrict

data LookupEnvException = LookupEnvException String deriving (Eq, Show, Typeable)

instance Exception LookupEnvException

main = runSimpleApp $ do
  cachixUser <- liftIO $ lookupEnv "CACHIX_USER"
  case cachixUser of
    Nothing -> throwM $ LookupEnvException "No CACHIX_USER found"
    Just a -> pure ()
  xs <- words . quickDecode <$> proc "nix-build" [] readProcessStdout_
  ys <- fmap join $ forM xs $ \x ->
          words . quickDecode <$> proc "nix-store" ["-q", "--deriver", x] readProcessStdout_
  zs <- fmap join $ forM ys $ \y -> do
          words . quickDecode <$> proc "nix-store" ["-qR", "--include-outputs", y] readProcessStdout_
  void $ forM zs $ \z -> do
    proc "cachix" ["push", fromJust $ cachixUser] $ runProcess_ . setStdin (byteStringInput $ fromString z)
