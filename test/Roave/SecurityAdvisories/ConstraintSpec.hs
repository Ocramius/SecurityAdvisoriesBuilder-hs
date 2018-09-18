module Roave.SecurityAdvisories.ConstraintSpec (main, spec) where

import Test.Hspec

import Roave.SecurityAdvisories.Constraint


-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

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
