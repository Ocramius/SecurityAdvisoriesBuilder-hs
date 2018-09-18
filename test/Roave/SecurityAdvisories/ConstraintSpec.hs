module Roave.SecurityAdvisories.ConstraintSpec
  ( main
  , spec
  ) where

import Control.DeepSeq
import Control.Exception
import Data.List.NonEmpty

import Test.Hspec

import Roave.SecurityAdvisories.Constraint

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

eitherToMaybeRight :: Either a b -> Maybe b
eitherToMaybeRight (Left _) = Nothing
eitherToMaybeRight (Right b) = Just b

spec :: Spec
spec
  -- https://github.com/Ocramius/SecurityAdvisoriesBuilder-hs/issues/2
 = do
  describe "parses version limits into well defined data" $ do
    it "parses \"<=\"" $ makeVersionLimit "<=" `shouldBe` Right LessThanEquals
    it "parses \"<\"" $ makeVersionLimit "<" `shouldBe` Right LessThan
    it "parses \"=\"" $ makeVersionLimit "=" `shouldBe` Right Equals
    it "parses \">\"" $ makeVersionLimit ">" `shouldBe` Right GreaterThan
    it "parses \">=\"" $ makeVersionLimit ">=" `shouldBe` Right GreaterThanEquals
    it "does not parse \"potato\"" $
      makeVersionLimit "potato" `shouldBe` Left "Unexpected version limit \"potato\" used"
    it "does not parse \" >= \" (spaces around it)" $
      makeVersionLimit " >= " `shouldBe` Left "Unexpected version limit \" >= \" used"
  describe "turns lists of natural numbers into versions" $ do
    it "does not consider an empty list as valid" $ makeVersion [] `shouldBe` Left "No version number provided"
    it "does consider a non-empty list as valid" $
      -- note: using the incomplete function Data.List.NonEmpty.fromList is OK here, since the values are hardcoded
      makeVersion [1, 2, 3] `shouldBe` Right (Version (fromList [1, 2, 3]))
    it "does consider a list with a single integer as valid" $ makeVersion [1] `shouldBe` Right (Version (fromList [1]))
    -- see "Expecting exceptions from pure code" in https://hspec.github.io/expectations.html
    it "negative versions are not valid" $
      (evaluate . force) ((show (makeVersion [-1])) ++ "unused") `shouldThrow` anyArithException
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
    v0 = eitherToMaybeRight $ makeVersion [0]
    v00 = eitherToMaybeRight $ makeVersion [0, 0]
    v11 = eitherToMaybeRight $ makeVersion [1, 1]
    v110 = eitherToMaybeRight $ makeVersion [1, 1, 0]
    v111 = eitherToMaybeRight $ makeVersion [1, 1, 1]
    v109 = eitherToMaybeRight $ makeVersion [1, 0, 9]
    v12 = eitherToMaybeRight $ makeVersion [1, 2]
    v21 = eitherToMaybeRight $ makeVersion [2, 1]