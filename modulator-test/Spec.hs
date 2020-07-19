{-# LANGUAGE OverloadedLists #-}

import qualified Data.Vector as V

import Test.Hspec

import Modulator
import Bits (Bits, Bit (I, O), bitsWithSigNum)


main :: IO ()
main = hspec $ do
  describe "Printing bits" $ do
    let showBits = show :: Bits -> String

    it "Simple" $ do
      showBits [O] `shouldBe` "0"
      showBits [I] `shouldBe` "1"
      showBits [O,O] `shouldBe` "00"
      showBits [I,I] `shouldBe` "11"
      showBits [O,I] `shouldBe` "01"
      showBits [I,O] `shouldBe` "10"

    it "Positive (with SigNum prefix)" $ do
      showBits [O,I,O] `shouldBe` "010"
      showBits [O,I,I] `shouldBe` "011"
      showBits [O,I,I,O] `shouldBe` "0110"
      showBits [O,I,I,I,O,O,O,O,O,O,I,I,I,O,O,I] `shouldBe` "0111000000111001"

    it "Negative (with SigNum prefix)" $ do
      showBits [I,O,I] `shouldBe` "101"
      showBits [I,O,I,O] `shouldBe` "1010"
      showBits [I,O,I,I,O,O,O,O,O,O,I,I,I,O,O,I] `shouldBe` "1011000000111001"

  describe "Number to binary conversion" $ do
    it "Positive" $ do
      bitsWithSigNum (0 :: Integer) `shouldBe` ((O,I), [O])
      bitsWithSigNum (1 :: Integer) `shouldBe` ((O,I), [I])
      bitsWithSigNum (2 :: Integer) `shouldBe` ((O,I), [I,O])

      bitsWithSigNum (12345 :: Integer)
        `shouldBe` ((O,I), [I,I,O,O,O,O,O,O,I,I,I,O,O,I])

    it "Negative" $ do
      bitsWithSigNum (-1 :: Integer) `shouldBe` ((I,O), [I])
      bitsWithSigNum (-2 :: Integer) `shouldBe` ((I,O), [I,O])

      bitsWithSigNum (-12345 :: Integer)
        `shouldBe` ((I,O), [I,I,O,O,O,O,O,O,I,I,I,O,O,I])
