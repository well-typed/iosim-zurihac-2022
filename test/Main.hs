module Main (main) where

import           Test.Tasty
import qualified ProjectSolTest as PSolTest
import Test.Tasty.QuickCheck (testProperty)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup "Test Group"
  [ testProperty "no write while read" PSolTest.noWriteWhileRead
  , testProperty "no 2 writers at same time" PSolTest.noWriteWhileWrite
  , testProperty "reads consistent" PSolTest.readConsistent
  ]
