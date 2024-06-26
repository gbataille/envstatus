import Test.Tasty

import Test.EnvStatus.Config (configTests)
import Test.EnvStatus.Output.Parse (parseTests)
import Test.EnvStatus.Output.Render (renderTests)

main :: IO ()
main = tests >>= defaultMain

tests :: IO TestTree
tests = testGroup "Tests" <$> sequence
  [ configTests
  , parseTests
  , renderTests
  ]
