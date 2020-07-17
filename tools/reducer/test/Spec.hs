import Test.Tasty
import Test.Tasty.HUnit

import Reducer

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Reducer" [specs]

specs = testGroup "Tests from specificaton"
  [
      testCase "#5" $ do
        reduce [Ap, Inc, Number 0] @?= [Number 1]
        reduce [Ap, Inc, Number 1] @?= [Number 2]
        reduce [Ap, Inc, Number 2] @?= [Number 3]
        reduce [Ap, Inc, Number 3] @?= [Number 4]
        reduce [Ap, Inc, Number 300] @?= [Number 301]
        reduce [Ap, Inc, Number 301] @?= [Number 302]
        reduce [Ap, Inc, Number (-1)] @?= [Number 0]
        reduce [Ap, Inc, Number (-2)] @?= [Number (-1)]
        reduce [Ap, Inc, Number (-3)] @?= [Number (-2)]

    , testCase "#6" $ do
        reduce [Ap, Dec, Number 1] @?= [Number 0]
        reduce [Ap, Dec, Number 2] @?= [Number 1]
        reduce [Ap, Dec, Number 3] @?= [Number 2]
        reduce [Ap, Dec, Number 4] @?= [Number 3]
        reduce [Ap, Dec, Number 1024] @?= [Number 1023]
        reduce [Ap, Dec, Number 0] @?= [Number (-1)]
        reduce [Ap, Dec, Number (-1)] @?= [Number (-2)]
        reduce [Ap, Dec, Number (-2)] @?= [Number (-3)]
  ]
