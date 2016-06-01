module Why3.AsProcessSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import Why3.AsProcess as W

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "isValidProof" $ do
    it "is valid on sat formulas" $ do
      isValidProof Valid `shouldBe` True
    it "is not valid on unsat formulas" $ property $
      \f -> (if f == Valid then True `shouldBe` True
             else isValidProof f `shouldBe` False)

  describe "discharge" $ do
    it "can discharge a valid proof (NEEDS Z3 and Why3 on your system)" $ do
      discharge Z3 "theory T goal g: true end" `shouldBe` Valid
    it "cannot discharge an invalid proof" $ do
      discharge Z3 "theory T goal g: false end" `shouldNotBe` Valid

instance Arbitrary SMTOutput where
  arbitrary = oneof [return Valid,
                     return Invalid,
                     return Unknown,
                     return W.Failure,
                     (Timeout `fmap` arbitrary)]
