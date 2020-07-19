{-# LANGUAGE TypeApplications, ViewPatterns, ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances, DerivingStrategies, OverloadedLists #-}
{-# LANGUAGE PartialTypeSignatures #-}

{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Modulator
     ( Modulatable (..)
     , Command (..)
     , CallToAliens (..)
     , UnknownYetThirdValue (..)
     , UnknownYetFourthValue (..)
     ) where

import Numeric.Natural (Natural)

import Control.Arrow ((&&&))

import Newtypes (PlayerKey, fromPlayerKey)
import Invaluator (Data (..))
import Bits



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
  modulate 0 = [O,I, O]
  modulate (bitsWithSigNum -> ((sig1, sig2), num)) = result where
    result = [sig1, sig2] <> lenPrefix <> num
    lenPrefix = replicateBits nI I <> replicateBits nO O

    len = bitsLength num
    lenMod = len `mod` 4
    nI = if lenMod == 0 then len `div` 4 else 1 + (len `div` 4)
    nO = 1 + if lenMod == 0 then 0 else 4 - lenMod

instance Modulatable Natural where
  modulate = modulate . toInteger

instance Modulatable PlayerKey where
  modulate = modulate . fromPlayerKey

instance Modulatable Data where
  modulate DNil        = modulate ()
  modulate (DNum x)    = modulate x
  modulate (DCons x y) = modulate ([x, y] :: [_])

-- | See “None” handler in “modulate” from “modulator.py”
instance Modulatable () where
  modulate () = [O,O]

-- | See “None” handler in “modulate” from “modulator.py”
instance Modulatable a => Modulatable (Maybe a) where
  modulate = maybe (modulate ()) modulate

-- | See “mod_list” from “modulator.py”
instance Modulatable a => Modulatable [a] where
  modulate [] = modulate ()
  -- ↓ This pattern wasn’t in the “modulator.py”, I assumed it from the code
  modulate [a] = modulate a
  -- ↓ This pattern was in the “modulator.py” but it seems it’s redundant
  -- modulate [a, b] = [I,I] <> modulate a <> modulate b
  modulate (init &&& last -> (init', last')) =
    foldMap (([I,I] <>) . modulate) init' <> modulate last'

-- | Just a Nil dummy plug since we don’t know yet what this value is
instance Modulatable UnknownYetThirdValue where
  modulate (UnknownYetThirdValue a b c d) = modulate ([a, b, c, d] :: [_])

-- | Just a Nil dummy plug since we don’t know yet what this value is
instance Modulatable UnknownYetFourthValue where
  modulate _ = error "absurd!"

instance Modulatable CallToAliens where
  modulate (CallToAliens a b c d) =
    modulate ([modulate a, modulate b, modulate c, modulate d] :: [_])
