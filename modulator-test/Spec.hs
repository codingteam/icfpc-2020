import qualified Data.Vector as V

import Test.Hspec

import Modulator


main :: IO ()
main = hspec $ do
  describe "Printing bits" $ do
    let f = Bits . V.fromList

    it "Simple" $ do
      show (f [O]) `shouldBe` "0"
      show (f [I]) `shouldBe` "1"
      show (f [O,O]) `shouldBe` "00"
      show (f [I,I]) `shouldBe` "11"
      show (f [O,I]) `shouldBe` "01"
      show (f [I,O]) `shouldBe` "10"

    it "Positive (with SigNum prefix)" $ do
      show (f [O,I,O]) `shouldBe` "010"
      show (f [O,I,I]) `shouldBe` "011"
      show (f [O,I,I,O]) `shouldBe` "0110"
      show (f [O,I,I,I,O,O,O,O,O,O,I,I,I,O,O,I]) `shouldBe` "0111000000111001"

    it "Negative (with SigNum prefix)" $ do
      show (f [I,O,I]) `shouldBe` "101"
      show (f [I,O,I,O]) `shouldBe` "1010"
      show (f [I,O,I,I,O,O,O,O,O,O,I,I,I,O,O,I]) `shouldBe` "1011000000111001"

  describe "Number to binary conversion" $ do
    let f = fmap (Bits . V.fromList)

    it "Positive" $ do
      bitsWithSigNum (0 :: Integer) `shouldBe` f ((O,I), [O])
      bitsWithSigNum (1 :: Integer) `shouldBe` f ((O,I), [I])
      bitsWithSigNum (2 :: Integer) `shouldBe` f ((O,I), [I,O])

      bitsWithSigNum (12345 :: Integer)
        `shouldBe` f ((O,I), [I,I,O,O,O,O,O,O,I,I,I,O,O,I])

    it "Negative" $ do
      bitsWithSigNum (-1 :: Integer) `shouldBe` f ((I,O), [I])
      bitsWithSigNum (-2 :: Integer) `shouldBe` f ((I,O), [I,O])

      bitsWithSigNum (-12345 :: Integer)
        `shouldBe` f ((I,O), [I,I,O,O,O,O,O,O,I,I,I,O,O,I])
