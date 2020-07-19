{-# LANGUAGE TypeApplications, ViewPatterns, ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances, LambdaCase, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}

module Modulator
     ( Modulatable (..)
     , Bit (..)
     , Bits (..)
     , Command (..)
     , CallToAliens (..)
     , UnknownYetThirdValue (..)
     , UnknownYetFourthValue (..)
     , bitsWithSigNum
     ) where

import qualified Data.Vector as V
import Data.Vector (Vector)
import Numeric.Natural (Natural)
import Text.Printf (IsChar (toChar, fromChar), printf)
import Data.String (IsString (fromString))
import Text.Read (readPrec, get)
import Data.Typeable (Typeable)

import Control.Arrow ((&&&))
import Control.Applicative (many)

import Newtypes (PlayerKey, fromPlayerKey)
import Invaluator (Data (..))

data Bit = O | I
  deriving stock (Eq, Enum, Bounded, Typeable)

instance Show Bit where
  show = pure . toChar

instance Read Bit where
  readPrec =
    get >>= \case
      '0' -> pure O
      '1' -> pure I
      x   -> fail ("Unexpected char for Bit: " <> pure x)

instance IsChar Bit where
  toChar O = '0'
  toChar I = '1'

  -- | Just for "printf"
  fromChar '0' = O
  fromChar '1' = I
  fromChar  x  = error ("Unexpected char for Bit: " <> pure x)


newtype Bits = Bits (Vector Bit)
  deriving stock (Eq, Typeable)
  deriving newtype (Semigroup, Monoid)

instance Show Bits where
  show (Bits x) = V.toList (V.map toChar x)

instance Read Bits where
  readPrec = Bits . V.fromList <$> many1 readPrec
    where many1 p = (:) <$> p <*> many p


data Command = Join | Start | Commands
  deriving stock (Show, Eq, Enum, Bounded)


data CallToAliens
   = CallToAliens
       Command
       PlayerKey
       (Maybe UnknownYetThirdValue)
       (Maybe UnknownYetFourthValue)
     deriving stock (Eq, Show)

-- | TODO Figure out what the third value is and rename this type
--
-- [5, 10, 15, 20] is from the our Python example
data UnknownYetThirdValue
   = UnknownYetThirdValue Integer Integer Integer Integer
     deriving stock (Eq, Show)

-- | TODO Figure out what the fourth value is and rename this type
--
-- This is impossible to construct a value of this type.
-- For "CallToAliens" it’s only possible to use "Nothing" as a value of it.
data UnknownYetFourthValue
instance Eq UnknownYetFourthValue where _ == _ = True
instance Show UnknownYetFourthValue where show _ = "absurd!"


-- | Encodable for the aliens
class Modulatable entity where
  modulate :: entity -> Bits

instance Modulatable Bits where
  modulate = id

instance Modulatable Command where
  modulate Join     = modulate @Integer 2
  modulate Start    = modulate @Integer 3
  modulate Commands = modulate @Integer 4

-- | This instance follows logic from “mod_number” from “modulator.py”
instance Modulatable Integer where
  modulate 0 = Bits $ V.fromList [O,I, O]
  modulate (bitsWithSigNum -> ((sig1, sig2), num)) = result where
    result = Bits (V.fromList [sig1, sig2]) <> lenPrefix <> num
    lenPrefix = Bits (V.replicate nI I <> V.replicate nO O)

    len = V.length $ case num of Bits x -> x
    lenMod = len `mod` 4
    nI = if lenMod == 0 then len `div` 4 else 1 + (len `div` 4)
    nO = 1 + if lenMod == 0 then 0 else (4 - lenMod)

instance Modulatable Natural where
  modulate = modulate . toInteger

instance Modulatable PlayerKey where
  modulate = modulate . fromPlayerKey

instance Modulatable Data where
  modulate DNil = Bits (V.fromList [O,O])
  modulate (DNum x) = modulate x
  modulate (DCons x y) = Bits (V.fromList [I, I]) <> modulate x <> modulate y

-- | See “None” handler in “modulate” from “modulator.py”
instance Modulatable a => Modulatable (Maybe a) where
  modulate = maybe (Bits $ V.fromList [O,O]) modulate

-- | See “mod_list” from “modulator.py”
instance Modulatable a => Modulatable [a] where
  modulate [] = Bits $ V.fromList [O,O]
  -- ↓ This pattern wasn’t in the “modulator.py”, I assumed it from the code
  modulate [a] = modulate a
  -- ↓ This pattern was in the “modulator.py” but it seems it’s redundant
  -- modulate [a, b] = V.fromList [I,I] <> modulate a <> modulate b
  modulate (init &&& last -> (init', last')) =
    foldMap ((Bits (V.fromList [I,I]) <>) . modulate) init' <> modulate last'

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
bitsWithSigNum :: Integer -> ((Bit, Bit), Bits)
bitsWithSigNum x =
  ( if x >= 0 then (O,I) else (I,O)
  , Bits $ V.fromList $ printf "%b" (fromInteger (abs x) :: Natural)
  )
