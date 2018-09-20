import Test.Tasty

import Control.Applicative ((<$>))
import Control.Monad (sequence)

import Test.EnvStatus.Config (configTests)
import Test.EnvStatus.Output.Render (renderTests)

main :: IO ()
main = tests >>= defaultMain

tests :: IO TestTree
tests = testGroup "Tests" <$> sequence
  [ configTests
  , renderTests
  ]
