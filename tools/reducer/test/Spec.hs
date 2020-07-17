import Test.Tasty
import Test.Tasty.HUnit

import Reducer

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Reducer" [specs]

specs = testGroup "Tests from specificaton"
  [
      testCase "#5.1" $ reduce [Ap, Inc, Number 0] @?= [Number 1]
    , testCase "#5.2" $ reduce [Ap, Inc, Number 1] @?= [Number 2]
    , testCase "#5.3" $ reduce [Ap, Inc, Number 2] @?= [Number 3]
    , testCase "#5.4" $ reduce [Ap, Inc, Number 3] @?= [Number 4]
    , testCase "#5.5" $ reduce [Ap, Inc, Number 300] @?= [Number 301]
    , testCase "#5.6" $ reduce [Ap, Inc, Number 301] @?= [Number 302]
    , testCase "#5.7" $ reduce [Ap, Inc, Number (-1)] @?= [Number 0]
    , testCase "#5.8" $ reduce [Ap, Inc, Number (-2)] @?= [Number (-1)]
    , testCase "#5.9" $ reduce [Ap, Inc, Number (-3)] @?= [Number (-2)]

    , testCase "#6.1" $ reduce [Ap, Dec, Number 1] @?= [Number 0]
    , testCase "#6.2" $ reduce [Ap, Dec, Number 2] @?= [Number 1]
    , testCase "#6.3" $ reduce [Ap, Dec, Number 3] @?= [Number 2]
    , testCase "#6.4" $ reduce [Ap, Dec, Number 4] @?= [Number 3]
    , testCase "#6.5" $ reduce [Ap, Dec, Number 1024] @?= [Number 1023]
    , testCase "#6.6" $ reduce [Ap, Dec, Number 0] @?= [Number (-1)]
    , testCase "#6.7" $ reduce [Ap, Dec, Number (-1)] @?= [Number (-2)]
    , testCase "#6.8" $ reduce [Ap, Dec, Number (-2)] @?= [Number (-3)]
  ]
