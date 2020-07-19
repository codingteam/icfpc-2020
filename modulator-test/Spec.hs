import qualified Data.Vector as V
import Data.Bifunctor (bimap)

import Test.Hspec

import Modulator


main :: IO ()
main = hspec $ do
  describe "Printing bits" $ do
    it "Simple" $ do
      printBits (V.fromList [O]) `shouldBe` "0"
      printBits (V.fromList [I]) `shouldBe` "1"
      printBits (V.fromList [O,O]) `shouldBe` "00"
      printBits (V.fromList [I,I]) `shouldBe` "11"
      printBits (V.fromList [O,I]) `shouldBe` "01"
      printBits (V.fromList [I,O]) `shouldBe` "10"

    it "Positive (with SigNum prefix)" $ do
      printBits (V.fromList [O,I,O]) `shouldBe` "010"
      printBits (V.fromList [O,I,I]) `shouldBe` "011"
      printBits (V.fromList [O,I,I,O]) `shouldBe` "0110"
      printBits (V.fromList [O,I,I,I,O,O,O,O,O,O,I,I,I,O,O,I])
        `shouldBe` "0111000000111001"

    it "Negative (with SigNum prefix)" $ do
      printBits (V.fromList [I,O,I]) `shouldBe` "101"
      printBits (V.fromList [I,O,I,O]) `shouldBe` "1010"
      printBits (V.fromList [I,O,I,I,O,O,O,O,O,O,I,I,I,O,O,I])
        `shouldBe` "1011000000111001"

  describe "Number to binary conversion" $ do
    let f = V.fromList

    it "Positive" $ do
      bitsWithSigNum (0 :: Integer) `shouldBe` bimap f f ([O,I], [O])
      bitsWithSigNum (1 :: Integer) `shouldBe` bimap f f ([O,I], [I])
      bitsWithSigNum (2 :: Integer) `shouldBe` bimap f f ([O,I], [I,O])

      bitsWithSigNum (12345 :: Integer)
        `shouldBe` bimap f f ([O,I], [I,I,O,O,O,O,O,O,I,I,I,O,O,I])

    it "Negative" $ do
      bitsWithSigNum (-1 :: Integer) `shouldBe` bimap f f ([I,O], [I])
      bitsWithSigNum (-2 :: Integer) `shouldBe` bimap f f ([I,O], [I,O])

      bitsWithSigNum (-12345 :: Integer)
        `shouldBe` bimap f f ([I,O], [I,I,O,O,O,O,O,O,I,I,I,O,O,I])
