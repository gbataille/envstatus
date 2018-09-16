import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [ dummyTest
  ]

dummyTest = testCase "Dummy" $ show 1 @?= "1"
