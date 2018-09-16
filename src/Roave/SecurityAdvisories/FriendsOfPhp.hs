{-# LANGUAGE OverloadedStrings #-}

module Roave.SecurityAdvisories.FriendsOfPhp
  ( parseAdvisoryYaml
  , Advisory(..)
  , AffectedBranch(..)
  ) where

import           Data.ByteString (ByteString)
import           Data.Map        (Map)
import           Data.Text       (Text)
import           Data.Yaml
import qualified Data.Yaml       as Y

data AffectedBranch = AffectedBranch
  { time     :: Maybe Text
  , versions :: [Text]
  } deriving (Eq, Show)

data Advisory = Advisory
  { title     :: Maybe Text
  , link      :: Maybe Text
  , cve       :: Maybe Text
  , branches  :: Map Text AffectedBranch
  , reference :: Maybe Text
  } deriving (Eq, Show)

parseAdvisoryYaml :: ByteString -> Either String Advisory
parseAdvisoryYaml = decodeEither

instance FromJSON AffectedBranch where
  parseJSON (Y.Object v) =
    AffectedBranch <$>
    v .: "time" <*>
    v .: "versions"
  parseJSON _ = fail "Expected an Object to be parsed into an AffectedBranch"

instance FromJSON Advisory where
  parseJSON (Y.Object v) =
    Advisory <$>
    v .: "title" <*>
    v .: "link" <*>
    v .: "cve" <*>
    v .: "branches" <*>
    v .: "reference"
  parseJSON _ = fail "Expected an Object to be parsed into an Advisory"
