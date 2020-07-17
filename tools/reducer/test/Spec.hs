import Test.Tasty
import Test.Tasty.HUnit

import Reducer (Token(..), reduce)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Reducer" [specs]

specs = testGroup "Tests from specificaton"
  [
      testCase "Doesn't change irreducible inputs" $ do
        reduce ["42"] @?= ["42"]
        reduce ["-13"] @?= ["-13"]

    , testCase "#5" $ do
        reduce ["ap", "inc", "0"] @?= ["1"]
        reduce ["ap", "inc", "1"] @?= ["2"]
        reduce ["ap", "inc", "2"] @?= ["3"]
        reduce ["ap", "inc", "3"] @?= ["4"]
        reduce ["ap", "inc", "300"] @?= ["301"]
        reduce ["ap", "inc", "301"] @?= ["302"]
        reduce ["ap", "inc", "-1"] @?= ["0"]
        reduce ["ap", "inc", "-2"] @?= ["-1"]
        reduce ["ap", "inc", "-3"] @?= ["-2"]

    , testCase "#6" $ do
        reduce ["ap", "dec", "1"] @?= ["0"]
        reduce ["ap", "dec", "2"] @?= ["1"]
        reduce ["ap", "dec", "3"] @?= ["2"]
        reduce ["ap", "dec", "4"] @?= ["3"]
        reduce ["ap", "dec", "1024"] @?= ["1023"]
        reduce ["ap", "dec", "0"] @?= ["-1"]
        reduce ["ap", "dec", "-1"] @?= ["-2"]
        reduce ["ap", "dec", "-2"] @?= ["-3"]

    , testCase "#7" $ do
        reduce ["ap", "ap", "add", "1", "2"] @?= ["3"]
        reduce ["ap", "ap", "add", "2", "1"] @?= ["3"]
        reduce ["ap", "ap", "add", "0", "1"] @?= ["1"]
        reduce ["ap", "ap", "add", "2", "3"] @?= ["5"]
        reduce ["ap", "ap", "add", "3", "5"] @?= ["8"]

    , testCase "#8" $ do
        reduce ["ap", "ap", "add", "0", "x0"] @?= ["x0"]
        reduce ["ap", "ap", "add", "0", "x1"] @?= ["x1"]
        reduce ["ap", "ap", "add", "0", "x2"] @?= ["x2"]
        reduce ["ap", "ap", "add", "x0", "0"] @?= ["x0"]
        reduce ["ap", "ap", "add", "x1", "0"] @?= ["x1"]
        reduce ["ap", "ap", "add", "x2", "0"] @?= ["x2"]
        -- We don't test rewriting rules yet
        -- reduce ["ap", "ap", "add", "x0", "x1"] @?= ["ap", "ap", "add", "x1", "x0"]
  ]
