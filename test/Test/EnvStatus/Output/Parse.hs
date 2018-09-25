module Test.EnvStatus.Output.Parse (parseTests) where

import Data.Either (isLeft)
import Test.Tasty
import Test.Tasty.Hspec
import Text.Parsec (parse)

import EnvStatus.Output.Parse
import EnvStatus.Output.Types

-- HLint config have to be put after the imports
{-# ANN module "HLint: ignore Redundant do" #-}

parseTests :: IO TestTree
parseTests =
  testSpec "EnvStatus.Output.Parse Tests" $ do

    describe "#rawParser" $ do
      it "parses standard string with any non-special char" $ do
        let someString = "foo bar %#!()[] nhutoe"
        parse rawParser "" someString `shouldBe` Right (Raw someString)

      it "parses string with a single open curly brace" $ do
        let someString = "foo bar %#!()[] {nhutoe"
        parse rawParser "" someString `shouldBe` Right (Raw someString)

      it "parses string with a single curly brace set" $ do
        let someString = "foo bar %#!()[] {nhutoe} dd"
        parse rawParser "" someString `shouldBe` Right (Raw someString)

      xit "parses string with a double opening curly brace with no closing" $ do
        let someString = "foo bar %#!()[] {{nhutoe dd"
        parse rawParser "" someString `shouldBe` Right (Raw someString)

    describe "#commandParser" $ do
      it "parses commands surrounded by curly braces" $ do
        let someString = "{{foo bar}}"
        parse commandParser "" someString `shouldBe` Right (SubCommand "foo bar")

      it "does not parse things with a single opening curly brace" $ do
        let someString = "{foo bar}}"
        parse commandParser "" someString `shouldSatisfy` isLeft

      it "does not parse things with a single closing curly brace" $ do
        let someString = "{{foo bar}"
        parse commandParser "" someString `shouldSatisfy` isLeft

      it "does not parse things with a no closing curly brace" $ do
        let someString = "{{foo bar"
        parse commandParser "" someString `shouldSatisfy` isLeft
