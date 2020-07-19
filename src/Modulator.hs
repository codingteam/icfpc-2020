{-# LANGUAGE TypeApplications, ViewPatterns, ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances, OverloadedLists, PartialTypeSignatures #-}

{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Modulator
     ( Modulatable (..)
     ) where

import Numeric.Natural (Natural)

import Control.Arrow ((&&&))

import Newtypes (PlayerKey, fromPlayerKey)
import Invaluator (Data (..))
import Bits
import Modulatable.Types


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

-- | Nil-pattern
--
-- See “None” handler in “modulate” from “modulator.py”.
instance Modulatable () where
  modulate () = [O,O]

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

instance Modulatable UnknownYetThirdValue where
  modulate (UnknownYetThirdValue a b c d) = modulate ([a, b, c, d] :: [_])

-- | FIXME Implement
instance Modulatable UnknownYetFourthValue where
  modulate _ = error "absurd!"

instance Modulatable CallToAliens where
  modulate (CallToAliens a b c d) =
    modulate ([modulate a, modulate b, modulate c, modulate d] :: [_])
