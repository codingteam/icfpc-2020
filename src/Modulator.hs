{-# LANGUAGE TypeApplications, ViewPatterns, ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}

module Modulator
     ( Modulatable (..)
     , Bit (..)
     , Command (..)
     , CallToAliens (..)
     , UnknownYetThirdValue (..)
     , UnknownYetFourthValue (..)
     , bitsWithSigNum
     , printBits
     ) where

import qualified Data.Vector as V
import Data.Vector (Vector)
import Numeric.Natural (Natural)
import Text.Printf (IsChar (toChar, fromChar), printf)
import Data.String (IsString (fromString))

import Control.Arrow ((&&&))

import Newtypes (PlayerKey, fromPlayerKey)


data Bit = O | I deriving (Eq, Enum, Bounded)

instance Show Bit where
  show = pure . toChar

instance IsChar Bit where
  toChar O = '0'
  toChar I = '1'

  fromChar '0' = O
  fromChar '1' = I
  fromChar  x  = error ("Unexpected char for Bit: " <> pure x)


data Command = Join | Start | Commands deriving (Show, Eq, Enum, Bounded)


data CallToAliens
   = CallToAliens
       Command
       PlayerKey
       (Maybe UnknownYetThirdValue)
       (Maybe UnknownYetFourthValue)
     deriving (Eq, Show)

-- | TODO Figure out what the third value is and rename this type
--
-- [5, 10, 15, 20] is from the our Python example
data UnknownYetThirdValue
   = UnknownYetThirdValue Integer Integer Integer Integer
     deriving (Eq, Show)

-- | TODO Figure out what the fourth value is and rename this type
--
-- This is impossible to construct a value of this type.
-- For "CallToAliens" it’s only possible to use "Nothing" as a value of it.
data UnknownYetFourthValue
instance Eq UnknownYetFourthValue where _ == _ = True
instance Show UnknownYetFourthValue where show _ = "absurd!"


-- | Encodable for the aliens
class Modulatable entity where
  modulate :: entity -> Vector Bit

instance Modulatable (Vector Bit) where
  modulate = id

instance Modulatable Command where
  modulate Join     = modulate @Integer 2
  modulate Start    = modulate @Integer 3
  modulate Commands = modulate @Integer 4

-- | This instance follows logic from “mod_number” from “modulator.py”
instance Modulatable Integer where
  modulate 0 = V.fromList [O,I, O]
  modulate (bitsWithSigNum -> (signum, num)) = result where
    result = signum <> lenPrefix <> num
    lenPrefix = V.replicate nI I <> V.replicate (succ nO) O

    len = V.length num
    lenMod = len `mod` 4
    nI = if lenMod == 0 then 0 else 4 - lenMod
    nO = (len + nI) `div` 4

instance Modulatable Natural where
  modulate = modulate . toInteger

instance Modulatable PlayerKey where
  modulate = modulate . fromPlayerKey

-- | See “None” handler in “modulate” from “modulator.py”
instance Modulatable a => Modulatable (Maybe a) where
  modulate = maybe (V.fromList [O,O]) modulate

-- | See “mod_list” from “modulator.py”
instance Modulatable a => Modulatable [a] where
  modulate [] = V.fromList [O,O]
  -- ↓ This pattern wasn’t in the “modulator.py”, I assumed it from the code
  modulate [a] = modulate a
  -- ↓ This pattern was in the “modulator.py” but it seems it’s redundant
  -- modulate [a, b] = V.fromList [I,I] <> modulate a <> modulate b
  modulate (init &&& last -> (exceptLast, last')) =
    foldMap ((V.fromList [I,I] <>) . modulate) exceptLast <> modulate last'

-- | Just a Nil dummy plug since we don’t know yet what this value is
instance Modulatable UnknownYetThirdValue where
  modulate (UnknownYetThirdValue a b c d) = modulate [a, b, c, d]

-- | Just a Nil dummy plug since we don’t know yet what this value is
instance Modulatable UnknownYetFourthValue where
  modulate _ = error "absurd!"

instance Modulatable CallToAliens where
  modulate (CallToAliens a b c d) =
    modulate [modulate a, modulate b, modulate c, modulate d]


-- | First is signum, second is absolute number.
bitsWithSigNum :: Integer -> (Vector Bit, Vector Bit)
bitsWithSigNum x =
  ( V.fromList (if x >= 0 then [O,I] else [I,O])
  , V.fromList $ printf "%b" (fromInteger (abs x) :: Natural)
  )


printBits :: IsString s => Vector Bit -> s
printBits = fromString . V.toList . V.map toChar
