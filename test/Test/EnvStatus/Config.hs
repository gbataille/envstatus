{-# LANGUAGE QuasiQuotes #-}
module Test.EnvStatus.Config (configTests) where

import Data.ConfigFile (emptyCP, get, to_string)
import Data.Either
import PyF

import Test.Tasty
import Test.Tasty.HUnit

import EnvStatus.Config


configTests :: TestTree
configTests = testGroup "Config Tests"
  [ testReadConfig
  ]

testReadConfig :: TestTree
testReadConfig = testGroup "#readConfig"
  [ testReadConfigErrorsOnBadFormat
    , testReadConfigProduceProperConfigParser
  ]

testReadConfigErrorsOnBadFormat :: TestTree
testReadConfigErrorsOnBadFormat = testCase "errors on bad format: section + text on same line" $ do
  cp <- readConfig "[Section]text"
  to_string cp @?= to_string emptyCP

testReadConfigProduceProperConfigParser :: TestTree
testReadConfigProduceProperConfigParser = testCase "produces proper ConfigParser object" $ do
  let k1 = "foo"
  let v1 = "bar"
  let k2 = "toto"
  let v2 = "tutu"
  let section = "section"
  let conf = [fString|{k1}: {v1}\n[{section}]\n{k2}: {v2}|]

  cp <- readConfig conf
  assertBool "ConfigParser should not be empty" $ to_string emptyCP /= to_string cp
  fromRight "" (get cp "DEFAULT" k1) @?= v1
  fromRight "" (get cp section k2) @?= v2
