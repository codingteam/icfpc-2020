import Test.Tasty
import Test.Tasty.HUnit

import Demodulator (demodulate)
import Invaluator (Data(..))
import Reducer (Token, ExprTree(..), reduce, parseProgram, flatten)
import Evaluator (evaluateSymbol)
import Modulator (printBits, modulate)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Reducer" [specs, ourSamplePrograms, ourModulator, ourDemodulator]

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

    , testCase "#21" $ do
        reduce ["ap", "ap", "t", "x0", "x1"] @?= ["x0"]
        reduce ["ap", "ap", "t", "1", "5"] @?= ["1"]
        reduce ["ap", "ap", "t", "t", "i"] @?= ["t"]
        reduce ["ap", "ap", "t", "t", "ap", "inc", "5"] @?= ["t"]
        reduce ["ap", "ap", "t", "ap", "inc", "5", "t"] @?= ["6"]

    , testCase "#22" $ do
        reduce ["ap", "ap", "f", "x0", "x1"] @?= ["x1"]
        -- We don't test rewriting rules yet
        --reduce ["f"] @?= ["ap", "s", "t"]

    , testCase "#23" $ do
        -- We don't test rewriting rules yet
        -- reduce ["pwr2"] @?= reduce ["ap", "ap", "s", "ap", "ap", "c", "ap", "eq", "0", "1", "ap", "ap", "b", "ap", "mul", "2", "ap", "ap", "b", "pwr2", "ap", "add", "-1"]
        -- reduce ["ap", "pwr2", "0"] @?= reduce ["ap", "ap", "ap", "s", "ap", "ap", "c", "ap", "eq", "0", "1", "ap", "ap", "b", "ap", "mul", "2", "ap", "ap", "b", "pwr2", "ap", "add", "-1", "0"]
        -- reduce ["ap", "pwr2", "0"] @?= reduce ["ap", "ap", "ap", "ap", "c", "ap", "eq", "0", "1", "0", "ap", "ap", "ap", "b", "ap", "mul", "2", "ap", "ap", "b", "pwr2", "ap", "add", "-1", "0"]
        -- reduce ["ap", "pwr2", "0"] @?= reduce ["ap", "ap", "ap", "ap", "eq", "0", "0", "1", "ap", "ap", "ap", "b", "ap", "mul", "2", "ap", "ap", "b", "pwr2", "ap", "add", "-1", "0"]
        -- reduce ["ap", "pwr2", "0"] @?= reduce ["ap", "ap", "t", "1", "ap", "ap", "ap", "b", "ap", "mul", "2", "ap", "ap", "b", "pwr2", "ap", "add", "-1", "0"]
        reduce ["ap", "pwr2", "0"] @?= ["1"]
        -- reduce ["ap", "pwr2", "1"] @?= reduce ["ap", "ap", "ap", "s", "ap", "ap", "c", "ap", "eq", "0", "1", "ap", "ap", "b", "ap", "mul", "2", "ap", "ap", "b", "pwr2", "ap", "add", "-1", "1"]
        -- reduce ["ap", "pwr2", "1"] @?= reduce ["ap", "ap", "ap", "ap", "c", "ap", "eq", "0", "1", "1", "ap", "ap", "ap", "b", "ap", "mul", "2", "ap", "ap", "b", "pwr2", "ap", "add", "-1", "1"]
        -- reduce ["ap", "pwr2", "1"] @?= reduce ["ap", "ap", "ap", "ap", "eq", "0", "1", "1", "ap", "ap", "ap", "b", "ap", "mul", "2", "ap", "ap", "b", "pwr2", "ap", "add", "-1", "1"]
        -- reduce ["ap", "pwr2", "1"] @?= reduce ["ap", "ap", "f", "1", "ap", "ap", "ap", "b", "ap", "mul", "2", "ap", "ap", "b", "pwr2", "ap", "add", "-1", "1"]
        -- reduce ["ap", "pwr2", "1"] @?= reduce ["ap", "ap", "ap", "b", "ap", "mul", "2", "ap", "ap", "b", "pwr2", "ap", "add", "-1", "1"]
        -- reduce ["ap", "pwr2", "1"] @?= reduce ["ap", "ap", "mul", "2", "ap", "ap", "ap", "b", "pwr2", "ap", "add", "-1", "1"]
        -- reduce ["ap", "pwr2", "1"] @?= reduce ["ap", "ap", "mul", "2", "ap", "pwr2", "ap", "ap", "add", "-1", "1"]
        -- reduce ["ap", "pwr2", "1"] @?= reduce ["ap", "ap", "mul", "2", "ap", "ap", "ap", "s", "ap", "ap", "c", "ap", "eq", "0", "1", "ap", "ap", "b", "ap", "mul", "2", "ap", "ap", "b", "pwr2", "ap", "add", "-1", "ap", "ap", "add", "-1", "1"]
        -- reduce ["ap", "pwr2", "1"] @?= reduce ["ap", "ap", "mul", "2", "ap", "ap", "ap", "ap", "c", "ap", "eq", "0", "1", "ap", "ap", "add", "-1", "1", "ap", "ap", "ap", "b", "ap", "mul", "2", "ap", "ap", "b", "pwr2", "ap", "add", "-1", "ap", "ap", "add", "-1", "1"]
        -- reduce ["ap", "pwr2", "1"] @?= reduce ["ap", "ap", "mul", "2", "ap", "ap", "ap", "ap", "eq", "0", "ap", "ap", "add", "-1", "1", "1", "ap", "ap", "ap", "b", "ap", "mul", "2", "ap", "ap", "b", "pwr2", "ap", "add", "-1", "ap", "ap", "add", "-1", "1"]
        -- reduce ["ap", "pwr2", "1"] @?= reduce ["ap", "ap", "mul", "2", "ap", "ap", "ap", "ap", "eq", "0", "0", "1", "ap", "ap", "ap", "b", "ap", "mul", "2", "ap", "ap", "b", "pwr2", "ap", "add", "-1", "ap", "ap", "add", "-1", "1"]
        -- reduce ["ap", "pwr2", "1"] @?= reduce ["ap", "ap", "mul", "2", "ap", "ap", "t", "1", "ap", "ap", "ap", "b", "ap", "mul", "2", "ap", "ap", "b", "pwr2", "ap", "add", "-1", "ap", "ap", "add", "-1", "1"]
        -- reduce ["ap", "pwr2", "1"] @?= reduce ["ap", "ap", "mul", "2", "1"]
        reduce ["ap", "pwr2", "1"] @?= ["2"]
        -- reduce ["ap", "pwr2", "2"] @?= reduce ["ap", "ap", "ap", "s", "ap", "ap", "c", "ap", "eq", "0", "1", "ap", "ap", "b", "ap", "mul", "2", "ap", "ap", "b", "pwr2", "ap", "add", "-1", "2"]

        reduce ["ap", "pwr2", "2"] @?= ["4"]
        reduce ["ap", "pwr2", "3"] @?= ["8"]
        reduce ["ap", "pwr2", "4"] @?= ["16"]
        reduce ["ap", "pwr2", "5"] @?= ["32"]
        reduce ["ap", "pwr2", "6"] @?= ["64"]
        reduce ["ap", "pwr2", "7"] @?= ["128"]
        reduce ["ap", "pwr2", "8"] @?= ["256"]

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

    , testCase "#30" $ do
        reduce ["(", ")"] @?= ["nil"]
        reduce ["(", "x0", ")"] @?= ["ap", "ap", "cons", "x0", "nil"]
        reduce ["(", "x0", ",", "x1", ")"] @?= ["ap", "ap", "cons", "x0", "ap", "ap", "cons", "x1", "nil"]
        reduce ["(", "x0", ",", "x1", ",", "x2", ")"] @?= ["ap", "ap", "cons", "x0", "ap", "ap", "cons", "x1", "ap", "ap", "cons", "x2", "nil"]
        reduce ["(", "x0", ",", "x1", ",", "x2", ",", "x5", ")"] @?= ["ap", "ap", "cons", "x0", "ap", "ap", "cons", "x1", "ap", "ap", "cons", "x2", "ap", "ap", "cons", "x5", "nil"]

    , testCase "#31" $ do
        -- This is copied of of #25, but "cons" is replaced by "vec" in input
        reduce ["ap", "ap", "ap", "vec", "x0", "x1", "x2"] @?= ["ap", "ap", "x2", "x0", "x1"]
        -- This is copied of of #26, but "cons" is replaced by "vec" in input
        reduce ["ap", "car", "ap", "ap", "vec", "x0", "x1"] @?= ["x0"]
        reduce ["ap", "car", "ap", "ap", "vec", "x0", "ap", "ap", "vec", "x2", "x1"] @?= ["x0"]
        reduce ["ap", "car", "ap", "ap", "vec", "ap", "ap", "vec", "x2", "x1", "nil"] @?= ["ap", "ap", "cons", "x2", "x1"]
        -- This is copied of of #27, but "cons" is replaced by "vec" in input
        reduce ["ap", "cdr", "ap", "ap", "vec", "x0", "x1"] @?= ["x1"]
        reduce ["ap", "cdr", "ap", "ap", "vec", "x0", "ap", "ap", "vec", "x2", "x1"] @?= ["ap", "ap", "cons", "x2", "x1"]
        reduce ["ap", "cdr", "ap", "ap", "vec", "ap", "ap", "vec", "x2", "x1", "nil"] @?= ["nil"]

    , testCase "#37" $ do
        reduce ["ap", "ap", "ap", "if0", "0", "x0", "x1"] @?= ["x0"]
        reduce ["ap", "ap", "ap", "if0", "1", "x0", "x1"] @?= ["x1"]

        -- Minoru's tests
        reduce ["ap", "ap", "ap", "if0", "2", "42", "ap", ":1", "ap", "dec", "2"]
          @?= ["ap", ":1", "ap", "dec", "2"]
  ]

ourSamplePrograms = testGroup "Our sample programs"
  [
      testCase "data/simple.txt" $
        let program = parseProgram $ unlines [
              ":321 = 12",
              ":123 = ap ap add 1 :321" ]
        in flatten (evaluateSymbol 123 program) @?= ["13"]

    , testCase "data/simple2.txt" $
        let program = parseProgram $ unlines [
              ":1 = ap ap vec 2 5",
              ":0 = ap ap cons :1 nil" ]
            expected = ["ap","ap","cons","ap","ap","cons","2","5","nil"]
        in flatten (evaluateSymbol 0 program) @?= expected

    , testCase "data/recursion.txt" $
        let program = parseProgram $ unlines [
              ":2048 = ap f :2048",
              ":0 = ap :2048 42" ]
            expected = ["42"]
        in flatten (evaluateSymbol 0 program) @?= expected

    , testCase "data/recursion2.txt" $
        let program = parseProgram $ unlines [
              ":1 = ap ap s ap ap s if0 ap t 42 ap ap b :1 dec",
              ":0 = ap :1 13" ]
            expected = ["42"]
        in flatten (evaluateSymbol 0 program) @?= expected
  ]

ourModulator = testGroup "Our modulator"
  [
    testCase "01100001" $ do
      (printBits $ modulate $ DNum 1) @?= "01100001"
  , testCase "10100001" $ do
      (printBits $ modulate $ DNum (-1)) @?= "10100001"
  , testCase "1101100001110111110110111001010100000" $ do
      (printBits $ modulate $ DCons (DNum 1) (DCons (DNum 56488) DNil)) @?= "1101100001110111110110111001010100000"
  ]

ourDemodulator = testGroup "Our demodulator"
  [
    testCase "01100001" $ do
      demodulate "01100001" @?= DNum 1
  , testCase "10100001" $ do
      demodulate "10100001" @?= DNum (-1)
  , testCase "1101100001110111110110111001010100000" $ do
      demodulate "1101100001110111110110111001010100000" @?= DCons (DNum 1) (DCons (DNum 56488) DNil)
  ]
