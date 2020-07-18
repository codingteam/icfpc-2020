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

    , testCase "#9" $ do
        reduce ["ap", "ap", "mul", "4", "2"] @?= ["8"]
        reduce ["ap", "ap", "mul", "3", "4"] @?= ["12"]
        reduce ["ap", "ap", "mul", "3", "-2"] @?= ["-6"]
        -- We don't test rewriting rules yet
        -- reduce ["ap", "ap", "mul", "x0", "x1"] @?= ["ap", "ap", "mul", "x1", "x0"]
        reduce ["ap", "ap", "mul", "x0", "0"] @?= ["0"]
        reduce ["ap", "ap", "mul", "x0", "1"] @?= ["x0"]

    , testCase "#10" $ do
        reduce ["ap", "ap", "div", "4", "2"] @?= ["2"]
        reduce ["ap", "ap", "div", "4", "3"] @?= ["1"]
        reduce ["ap", "ap", "div", "4", "4"] @?= ["1"]
        reduce ["ap", "ap", "div", "4", "5"] @?= ["0"]
        reduce ["ap", "ap", "div", "5", "2"] @?= ["2"]
        reduce ["ap", "ap", "div", "6", "-2"] @?= ["-3"]
        reduce ["ap", "ap", "div", "5", "-3"] @?= ["-1"]
        reduce ["ap", "ap", "div", "-5", "3"] @?= ["-1"]
        reduce ["ap", "ap", "div", "-5", "-3"] @?= ["1"]
        reduce ["ap", "ap", "div", "x0", "1"] @?= ["x0"]

    , testCase "#11" $ do
        reduce ["ap", "ap", "eq", "x0", "x0"] @?= ["t"]
        reduce ["ap", "ap", "eq", "0", "-2"] @?= ["f"]
        reduce ["ap", "ap", "eq", "0", "-1"] @?= ["f"]
        reduce ["ap", "ap", "eq", "0", "0"] @?= ["t"]
        reduce ["ap", "ap", "eq", "0", "1"] @?= ["f"]
        reduce ["ap", "ap", "eq", "0", "2"] @?= ["f"]
        reduce ["ap", "ap", "eq", "1", "-1"] @?= ["f"]
        reduce ["ap", "ap", "eq", "1", "0"] @?= ["f"]
        reduce ["ap", "ap", "eq", "1", "1"] @?= ["t"]
        reduce ["ap", "ap", "eq", "1", "2"] @?= ["f"]
        reduce ["ap", "ap", "eq", "1", "3"] @?= ["f"]
        reduce ["ap", "ap", "eq", "2", "0"] @?= ["f"]
        reduce ["ap", "ap", "eq", "2", "1"] @?= ["f"]
        reduce ["ap", "ap", "eq", "2", "2"] @?= ["t"]
        reduce ["ap", "ap", "eq", "2", "3"] @?= ["f"]
        reduce ["ap", "ap", "eq", "2", "4"] @?= ["f"]
        reduce ["ap", "ap", "eq", "19", "20"] @?= ["f"]
        reduce ["ap", "ap", "eq", "20", "20"] @?= ["t"]
        reduce ["ap", "ap", "eq", "21", "20"] @?= ["f"]
        reduce ["ap", "ap", "eq", "-19", "-20"] @?= ["f"]
        reduce ["ap", "ap", "eq", "-20", "-20"] @?= ["t"]
        reduce ["ap", "ap", "eq", "-21", "-20"] @?= ["f"]

    , testCase "#12" $ do
        reduce ["ap", "ap", "lt", "0", "-1"] @?= ["f"]
        reduce ["ap", "ap", "lt", "0", "0"] @?= ["f"]
        reduce ["ap", "ap", "lt", "0", "1"] @?= ["t"]
        reduce ["ap", "ap", "lt", "0", "2"] @?= ["t"]
        reduce ["ap", "ap", "lt", "1", "0"] @?= ["f"]
        reduce ["ap", "ap", "lt", "1", "1"] @?= ["f"]
        reduce ["ap", "ap", "lt", "1", "2"] @?= ["t"]
        reduce ["ap", "ap", "lt", "1", "3"] @?= ["t"]
        reduce ["ap", "ap", "lt", "2", "1"] @?= ["f"]
        reduce ["ap", "ap", "lt", "2", "2"] @?= ["f"]
        reduce ["ap", "ap", "lt", "2", "3"] @?= ["t"]
        reduce ["ap", "ap", "lt", "2", "4"] @?= ["t"]
        reduce ["ap", "ap", "lt", "19", "20"] @?= ["t"]
        reduce ["ap", "ap", "lt", "20", "20"] @?= ["f"]
        reduce ["ap", "ap", "lt", "21", "20"] @?= ["f"]
        reduce ["ap", "ap", "lt", "-19", "-20"] @?= ["f"]
        reduce ["ap", "ap", "lt", "-20", "-20"] @?= ["f"]
        reduce ["ap", "ap", "lt", "-21", "-20"] @?= ["t"]

    , testCase "#16" $ do
        reduce ["ap", "neg", "0"] @?= ["0"]
        reduce ["ap", "neg", "1"] @?= ["-1"]
        reduce ["ap", "neg", "-1"] @?= ["1"]
        reduce ["ap", "neg", "2"] @?= ["-2"]
        reduce ["ap", "neg", "-2"] @?= ["2"]

    , testCase "#17" $ do
        reduce ["ap", "inc", "ap", "inc", "0"] @?= ["2"]
        reduce ["ap", "inc", "ap", "inc", "ap", "inc", "0"] @?= ["3"]
        reduce ["ap", "inc", "ap", "dec", "x0"] @?= ["x0"]
        reduce ["ap", "dec", "ap", "inc", "x0"] @?= ["x0"]
        reduce ["ap", "dec", "ap", "ap", "add", "x0", "1"] @?= ["x0"]
        reduce ["ap", "ap", "add", "ap", "ap", "add", "2", "3", "4"] @?= ["9"]
        reduce ["ap", "ap", "add", "2", "ap", "ap", "add", "3", "4"] @?= ["9"]
        reduce ["ap", "ap", "add", "ap", "ap", "mul", "2", "3", "4"] @?= ["10"]
        reduce ["ap", "ap", "mul", "2", "ap", "ap", "add", "3", "4"] @?= ["14"]
        -- We don't test rewriting rules yet
        -- reduce ["inc"] @?= ["ap", "add", "1"]
        -- reduce ["dec"] @?= ["ap", "add", "ap", "neg", "1"]

    , testCase "#18" $ do
        reduce ["ap", "ap", "ap", "s", "x0", "x1", "x2"] @?= ["ap", "ap", "x0", "x2", "ap", "x1", "x2"]
        reduce ["ap", "ap", "ap", "s", "add", "inc", "1"] @?= ["3"]
        reduce ["ap", "ap", "ap", "s", "mul", "ap", "add", "1", "6"] @?= ["42"]

    , testCase "#19" $ do
        reduce ["ap", "ap", "ap", "c", "x0", "x1", "x2"] @?= ["ap", "ap", "x0", "x2", "x1"]
        reduce ["ap", "ap", "ap", "c", "add", "1", "2"] @?= ["3"]

    , testCase "#20" $ do
        reduce ["ap", "ap", "ap", "b", "x0", "x1", "x2"] @?= ["ap", "x0", "ap", "x1", "x2"]
        reduce ["ap", "ap", "ap", "b", "inc", "dec", "x0"] @?= ["x0"]

    , testCase "#22" $ do
        reduce ["ap", "ap", "f", "x0", "x1"] @?= ["x1"]
        -- We don't test rewriting rules yet
        --reduce ["f"] @?= ["ap", "s", "t"]

    , testCase "#24" $ do
        reduce ["ap", "i", "x0"] @?= ["x0"]
        reduce ["ap", "i", "1"] @?= ["1"]
        reduce ["ap", "i", "i"] @?= ["i"]
        reduce ["ap", "i", "add"] @?= ["add"]
        reduce ["ap", "i", "ap", "add", "1"] @?= ["ap", "add", "1"]

    , testCase "#25" $ do
        reduce ["ap", "ap", "ap", "cons", "x0", "x1", "x2"] @?= ["ap", "ap", "x2", "x0", "x1"]

    , testCase "#26" $ do
        reduce ["ap", "car", "ap", "ap", "cons", "x0", "x1"] @?= ["x0"]
        reduce ["ap", "car", "ap", "ap", "cons", "x0", "ap", "ap", "cons", "x2", "x1"] @?= ["x0"]
        reduce ["ap", "car", "ap", "ap", "cons", "ap", "ap", "cons", "x2", "x1", "nil"] @?= ["ap", "ap", "cons", "x2", "x1"]
        reduce ["ap", "car", "x2"] @?= ["ap", "x2", "t"]

    , testCase "#27" $ do
        reduce ["ap", "cdr", "ap", "ap", "cons", "x0", "x1"] @?= ["x1"]
        reduce ["ap", "cdr", "ap", "ap", "cons", "x0", "ap", "ap", "cons", "x2", "x1"] @?= ["ap", "ap", "cons", "x2", "x1"]
        reduce ["ap", "cdr", "ap", "ap", "cons", "ap", "ap", "cons", "x2", "x1", "nil"] @?= ["nil"]
        reduce ["ap", "cdr", "x2"] @?= ["ap", "x2", "f"]

    , testCase "#28" $ do
        reduce ["ap", "nil", "x0"] @?= ["t"]

    , testCase "#29" $ do
        reduce ["ap", "isnil", "nil"] @?= ["t"]
        reduce ["ap", "isnil", "ap", "ap", "cons", "x0", "x1"] @?= ["f"]
  ]
