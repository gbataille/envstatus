{-# LANGUAGE QuasiQuotes #-}
module Test.EnvStatus.Config (configTests) where

import Data.ConfigFile (emptyCP, get, to_string)
import Data.Either
import PyF

import Test.Tasty
import Test.Tasty.Hspec

import EnvStatus.Config

configTests :: IO TestTree
configTests =
  testSpec "Config Tests" $ do

    describe "#readConfig" $ do
      it "returns an empty config when there is a parser error" $ do
        cp <- readConfig "[Section]text"
        to_string cp `shouldBe` to_string emptyCP

      it "returns a proper config object" $ do
        let k1 = "foo"
        let v1 = "bar"
        let k2 = "toto"
        let v2 = "tutu"
        let section = "section"
        let conf = [fString|{k1}: {v1}\n[{section}]\n{k2}: {v2}|]

        cp <- readConfig conf
        to_string emptyCP `shouldSatisfy` (to_string cp /=)
        fromRight "" (get cp "DEFAULT" k1) `shouldBe` v1
        fromRight "" (get cp section k2) `shouldBe` v2
