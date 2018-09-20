module Roave.SecurityAdvisories.ConstraintSpec
  ( main
  , spec
  ) where

import Data.List.NonEmpty

import Test.Hspec

import Roave.SecurityAdvisories.Constraint

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

doesNotParseVersionLimit :: String -> Expectation
doesNotParseVersionLimit string = stringToVersionLimit string `shouldBe` Nothing

doesNotParseVersion :: String -> Expectation
doesNotParseVersion string = stringToVersion string `shouldBe` Nothing

versionParsedAs :: String -> String -> Expectation
versionParsedAs toBeParsed expected = Prelude.fmap versionToString (stringToVersion toBeParsed) `shouldBe` Just expected

spec :: Spec
spec
  -- https://github.com/Ocramius/SecurityAdvisoriesBuilder-hs/issues/2
 = do
  describe "parses version limits into well defined data" $ do
    it "parses \"<=\"" $ stringToVersionLimit "<=" `shouldBe` Just LessThanEquals
    it "parses \"<\"" $ stringToVersionLimit "<" `shouldBe` Just LessThan
    it "parses \"=\"" $ stringToVersionLimit "=" `shouldBe` Just Equals
    it "parses \">\"" $ stringToVersionLimit ">" `shouldBe` Just GreaterThan
    it "parses \">=\"" $ stringToVersionLimit ">=" `shouldBe` Just GreaterThanEquals
    it "does not parse \"potato\"" $ doesNotParseVersionLimit "potato"
    it "does not parse \"\"" $ doesNotParseVersionLimit ""
    it "does not parse \" >= \" (spaces around it)" $ doesNotParseVersionLimit " >= "
  describe "parses versions into well defined data" $ do
    it "does not parse \"\"" $ doesNotParseVersion ""
    it "does not parse \"abc\"" $ doesNotParseVersion "abc"
    it "does not parse \"  \"" $ doesNotParseVersion "  "
    it "does not parse \"-1\"" $ doesNotParseVersion "-1"
    it "does not parse \"1.-1\"" $ doesNotParseVersion "1.-1"
    it "does not parse \"1. 1\"" $ doesNotParseVersion "1. 1"
    it "does not parse \"1 .1\"" $ doesNotParseVersion "1 .1"
    it "does not parse \"1.\"" $ doesNotParseVersion "1."
    it "parses \"0\"" $ "0" `versionParsedAs` "0"
    it "parses \"0.0\"" $ "0.0" `versionParsedAs` "0"
    it "parses \"1\"" $ "1" `versionParsedAs` "1"
    it "parses \"1.2.3.4\"" $ "1.2.3.4" `versionParsedAs` "1.2.3.4"
    it "parses \"1.0.2\"" $ "1.0.2" `versionParsedAs` "1.0.2"
    it "parses and normalises \"1.0.2.0\"" $ "1.0.2.0" `versionParsedAs` "1.0.2"
    it "parses \"111.222.000.333.000\"" $ "111.222.000.333.000" `versionParsedAs` "111.222.0.333"
    it "parses \"000\"" $ "000" `versionParsedAs` "0"
  describe "ranges can overlap" $ do
    it "can merge >= 1.2 with < 1.2" $
      From (VersionBoundary GreaterThanEquals (Version (fromList [1, 2]))) `canMergeRanges`
      Till (VersionBoundary LessThan (Version (fromList [1, 2]))) `shouldBe`
      True
    it "can merge > 1.2 with <= 1.2" $
      From (VersionBoundary GreaterThan (Version (fromList [1, 2]))) `canMergeRanges`
      Till (VersionBoundary LessThanEquals (Version (fromList [1, 2]))) `shouldBe`
      True
    it "can merge > 1.2, <= 1.3 with <= 1.2" $
      Range
        (VersionBoundary GreaterThan (Version (fromList [1, 2])))
        (VersionBoundary LessThanEquals (Version (fromList [1, 3]))) `canMergeRanges`
      Till (VersionBoundary LessThanEquals (Version (fromList [1, 2]))) `shouldBe`
      True
  describe "versions can be sorted" $ do
    it "0 = 0.0" $ (v0 `compare` v00) `shouldBe` EQ
    it "1.2 < 2.1" $ (v12 `compare` v21) `shouldBe` LT
    it "1.1 < 1.2" $ (v11 `compare` v12) `shouldBe` LT
    it "1.2 > 1.1" $ (v12 `compare` v11) `shouldBe` GT
    it "1.1 = 1.1" $ (v11 `compare` v11) `shouldBe` EQ
    it "1.1.0 = 1.1" $ (v110 `compare` v11) `shouldBe` EQ
    it "1.1.1 = 1.1" $ (v111 `compare` v11) `shouldBe` GT
    it "1.0.9 < 1.1" $ (v109 `compare` v11) `shouldBe` LT
    it "1.1.1 > 1.0.9" $ (v111 `compare` v109) `shouldBe` GT
  where
    v0 = stringToVersion "0"
    v00 = stringToVersion "0.0"
    v11 = stringToVersion "1.1"
    v110 = stringToVersion "1.1.0"
    v111 = stringToVersion "1.1.1"
    v109 = stringToVersion "1.0.9"
    v12 = stringToVersion "1.2"
    v21 = stringToVersion "2.1"