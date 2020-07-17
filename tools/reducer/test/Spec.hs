import Test.Tasty
import Test.Tasty.HUnit

import Reducer (Token(..), reduce)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Reducer" [specs]

specs = testGroup "Tests from specificaton"
  [
      testCase "#5" $ do
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
  ]
