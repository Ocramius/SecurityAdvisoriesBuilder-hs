module Roave.SecurityAdvisories.ConstraintSpec (main, spec) where

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
spec = do
  -- https://github.com/Ocramius/SecurityAdvisoriesBuilder-hs/issues/2
  describe "parses version limits into well defined data" $ do
    it "parses \"<=\"" $ do
      makeVersionLimit "<=" `shouldBe` Right LessThanEquals
    it "parses \"<\"" $ do
      makeVersionLimit "<" `shouldBe` Right LessThan
    it "parses \"=\"" $ do
      makeVersionLimit "=" `shouldBe` Right Equals
    it "parses \">\"" $ do
      makeVersionLimit ">" `shouldBe` Right GreaterThan
    it "parses \">=\"" $ do
      makeVersionLimit ">=" `shouldBe` Right GreaterThanEquals
    it "does not parse \"potato\"" $ do
      makeVersionLimit "potato" `shouldBe` Left "Unexpected version limit \"potato\" used"
    it "does not parse \" >= \" (spaces around it)" $ do
      makeVersionLimit " >= " `shouldBe` Left "Unexpected version limit \" >= \" used"

  describe "turns lists of natural numbers into versions" $ do
    it "does not consider an empty list as valid" $ do
      makeVersion [] `shouldBe` Left "No version number provided"
    it "does consider a non-empty list as valid" $ do
      -- note: using the incomplete function Data.List.NonEmpty.fromList is OK here, since the values are hardcoded
      makeVersion [1, 2, 3] `shouldBe` Right (Version (fromList [1, 2, 3]))
    it "does consider a list with a single integer as valid" $ do
      makeVersion [1] `shouldBe` Right (Version (fromList [1]))
    -- see "Expecting exceptions from pure code" in https://hspec.github.io/expectations.html
    it "negative versions are not valid" $ do
      (evaluate . force) ((show (makeVersion [-1])) ++ "unused") `shouldThrow` anyArithException

  describe "versions can be sorted" $ do
    it "0 = 0.0" $ do
      (v0 `compare` v00) `shouldBe` EQ
    it "1.2 < 2.1" $ do
      (v12 `compare` v21) `shouldBe` LT
    it "1.1 < 1.2" $ do
      (v11 `compare` v12) `shouldBe` LT
    it "1.2 > 1.1" $ do
      (v12 `compare` v11) `shouldBe` GT
    it "1.1 = 1.1" $ do
      (v11 `compare` v11) `shouldBe` EQ
    it "1.1.0 = 1.1" $ do
      (v110 `compare` v11) `shouldBe` EQ
    it "1.1.1 = 1.1" $ do
      (v111 `compare` v11) `shouldBe` GT
    it "1.0.9 < 1.1" $ do
      (v109 `compare` v11) `shouldBe` LT
    it "1.1.1 > 1.0.9" $ do
      (v111 `compare` v109) `shouldBe` GT
      where
        v0 = eitherToMaybeRight $ makeVersion [0]
        v00 = eitherToMaybeRight $ makeVersion [0, 0]
        v11 = eitherToMaybeRight $ makeVersion [1, 1]
        v110 = eitherToMaybeRight $ makeVersion [1, 1, 0]
        v111 = eitherToMaybeRight $ makeVersion [1, 1, 1]
        v109 = eitherToMaybeRight $ makeVersion [1, 0, 9]
        v12 = eitherToMaybeRight $ makeVersion [1, 2]
        v21 = eitherToMaybeRight $ makeVersion [2, 1]
