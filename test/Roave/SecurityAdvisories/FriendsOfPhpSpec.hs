{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Roave.SecurityAdvisories.FriendsOfPhpSpec (main, spec) where

import Data.Map (singleton)
import Data.ByteString (ByteString)
import Text.RawString.QQ

import Test.Hspec

import Roave.SecurityAdvisories.FriendsOfPhp


-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

exampleAdvisory :: ByteString
exampleAdvisory = [r|
title:     HTTP Proxy header vulnerability
link:      https://github.com/bugsnag/bugsnag-laravel/releases/tag/v2.0.2
cve:       CVE-2016-5385
branches:
    master:
        time:     2016-07-18 20:27:36
        versions: ['>=2', '<2.0.2']
reference: composer://bugsnag/bugsnag-laravel
|]

spec :: Spec
spec = do
  -- https://github.com/Ocramius/SecurityAdvisoriesBuilder-hs/issues/2
  describe "parse advisories into an advisory data structure" $ do
    it "parses a valid advisory" $ do
      parseAdvisoryYaml exampleAdvisory `shouldBe` Right Advisory {
        title = Just "HTTP Proxy header vulnerability"
        , link = Just "https://github.com/bugsnag/bugsnag-laravel/releases/tag/v2.0.2"
        , cve = Just "CVE-2016-5385"
        , branches = singleton "master" AffectedBranch {
          time = Just "2016-07-18 20:27:36"
          , versions = [">=2", "<2.0.2"]
        }
        , reference = Just "composer://bugsnag/bugsnag-laravel"
      }
