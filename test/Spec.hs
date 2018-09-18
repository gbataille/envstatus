import Test.Tasty

import Test.EnvStatus.Config (configTests)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [ configTests
  ]
