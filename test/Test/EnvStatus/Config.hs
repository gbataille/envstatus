{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Test.EnvStatus.Config (configTests) where

import Data.ConfigFile (emptyCP, get, to_string, ConfigParser)
import Data.Either
import PyF

import Test.Tasty
import Test.Tasty.Hspec

import EnvStatus.Config

-- HLint config have to be put after the imports
{-# ANN module "HLint: ignore Redundant do" #-}

defaultKey = "k1"
defaultValue = "v1"
sectionName = "Section"
sectionKey = "k2"
sectionValue = "v2"

dummyConfigString = [fString|{defaultKey}: {defaultValue}\n[{sectionName}]\n{sectionKey}: {sectionValue}|]

dummyConfig :: IO ConfigParser
dummyConfig = readConfig dummyConfigString

configTests :: IO TestTree
configTests =
  testSpec "Config Tests" $ do

    describe "#readConfig" $ do
      it "returns an empty config when there is a parser error" $ do
        cp <- readConfig "[Section]text"
        to_string cp `shouldBe` to_string emptyCP

      it "returns a proper config object" $ do
        cp <- readConfig dummyConfigString
        to_string emptyCP `shouldSatisfy` (to_string cp /=)
        fromRight "" (get cp "DEFAULT" defaultKey) `shouldBe` defaultValue
        fromRight "" (get cp sectionName sectionKey) `shouldBe` sectionValue


    describe "#getConfigValue" $ do
      before dummyConfig $ do

        it "returns Nothing if the config does not define the value" $ \cp -> do
          getConfigValue cp "some_bad_key" `shouldBe` Nothing

        it "returns the value in the default section" $ \cp -> do
          getConfigValue cp defaultKey `shouldBe` Just defaultValue

        it "returns nothing for keys inside sections" $ \cp -> do
          getConfigValue cp sectionKey `shouldBe` Nothing
