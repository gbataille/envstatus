module Test.EnvStatus.Output.Parse (parseTests) where

import Data.Either (isLeft)
import Test.Tasty
import Test.Tasty.Hspec
import Text.Parsec (parse, try)
import Text.Parsec.Char (anyChar)
import Test.Hspec

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

    describe "#singleOpenCurly" $ do
      before (pure $ singleOpenCurly <* try anyChar) $ do

        it "parses a single opening curly brace" $ \parser -> do
          let someString = "{x"
          parse parser "" someString `shouldBe` Right '{'

        it "does not parse 2 consecutive opening curly braces" $ \parser -> do
          let someString = "{{"
          parse parser "" someString `shouldSatisfy` isLeft

    describe "#outputFormatParser" $ do
      it "parses a proper templated string" $ do
        let someString = "raw string {{foo bar}} and more"
        parse outputFormatParser "" someString `shouldBe`
          Right [Raw "raw string ", SubCommand "foo bar", Raw " and more"]

    describe "#parseOutputFormat" $ do
      it "parses a proper templated string" $ do
        let someString = "raw string {{foo bar}} and more"
        parseOutputFormat someString `shouldBe`
          [Raw "raw string ", SubCommand "foo bar", Raw " and more"]

      it "handles newline" $ do
        let someString = "raw string {{foo bar}}\n and more"
        parseOutputFormat someString `shouldBe`
          [Raw "raw string ", SubCommand "foo bar", Raw "\n and more"]

      it "handles empty strings and returns an empty Token list" $ do
        parseOutputFormat "" `shouldBe` []

    describe "#quotedOption" $ do
      it "parses a double quoted string" $ do
        let someString = "a double quoted string"
        let quotedString = "\"" ++ someString ++ "\""
        parse quotedOption "" quotedString `shouldBe` Right someString

      it "does not parse a double quoted string preceded by text" $ do
        let someString = "a double quoted string"
        let quotedString = "foo bar \"" ++ someString ++ "\""
        parse quotedOption "" quotedString `shouldSatisfy` isLeft

      it "parses a double quoted string followed by text and stops at the closing quote" $ do
        let someString = "a double quoted string"
        let quotedString = "\"" ++ someString ++ "\" foo bar"
        parse quotedOption "" quotedString `shouldBe` Right someString

      it "does not parse a non close double quote chain" $ do
        let someString = "a double quoted string"
        let quotedString = "\"" ++ someString
        parse quotedOption "" quotedString `shouldSatisfy` isLeft

    describe "#separator" $ do
      it "parses space" $ do
        let char = ' '
        parse separator "" [char] `shouldBe` Right char

      it "parses tab" $ do
        let char = '\t'
        parse separator "" [char] `shouldBe` Right char

      it "parses cr" $ do
        let char = '\n'
        parse separator "" [char] `shouldBe` Right char

      it "parses lf" $ do
        let char = '\r'
        parse separator "" [char] `shouldBe` Right char

      it "does not parse standard string" $ do
        parse separator "" "a" `shouldSatisfy` isLeft

    describe "#word" $ do
      it "parses any non-separator based string" $ do
        let someString = "foobar"
        parse word "" someString `shouldBe` Right someString

      it "parses string until first separator" $ do
        let someString = "foobar"
        parse word "" someString `shouldBe` Right someString

        parse word "" (someString ++ " toto") `shouldBe` Right someString

        parse word "" (someString ++ "\tfoobar") `shouldBe` Right someString

        parse word "" (someString ++ "\n") `shouldBe` Right someString

    describe "#commandPart" $ do
      it "parses simple strings" $ do
        let someString = "foobar"
        parse commandPart "" someString `shouldBe` Right someString

      it "parses words individually" $ do
        let someString = "foobar"
        parse commandPart "" (someString ++ " barbaz") `shouldBe` Right someString

      it "parses double quoted section as a whole" $ do
        let someString = "foobar barbaz"
        parse commandPart "" ("\"" ++ someString ++ "\" something else") `shouldBe` Right someString

    describe "#parseCommand" $ do
      it "parses blocks properly" $ do
        let someString = " echo -n \"hello world\"  \n"

        parseCommand someString `shouldBe` ["echo", "-n", "hello world"]
