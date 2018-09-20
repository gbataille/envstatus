module Test.EnvStatus.Output.Render (renderTests) where

import Test.Tasty
import Test.Tasty.Hspec

import EnvStatus.Output.Render

-- HLint config have to be put after the imports
{-# ANN module "HLint: ignore Redundant do" #-}

renderTests :: IO TestTree
renderTests =
  testSpec "Render Tests" $ do

    describe "#strip" $ do
      it "returns the untouched string if no spaces" $ do
        strip "foobar" `shouldBe` "foobar"

      it "returns the string without leading spaces" $ do
        strip "  foobar" `shouldBe` "foobar"

      it "returns the string without trailing spaces" $ do
        strip "foobar  " `shouldBe` "foobar"

      it "returns the string without surrounding spaces" $ do
        strip "    foobar  " `shouldBe` "foobar"
