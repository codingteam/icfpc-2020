{-# LANGUAGE ViewPatterns, ScopedTypeVariables, OverloadedLists  #-}
{-# LANGUAGE FlexibleInstances, PartialTypeSignatures #-}

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
  modulate (DCons (DNum x) (DNum y)) = modulate (x,y)
  modulate (DCons x y) = modulate ([x, y] :: [_])

-- | Nil-pattern
--
-- See “None” handler in “modulate” from “modulator.py”.
instance Modulatable () where
  modulate () = [O,O]

instance (Modulatable a, Modulatable b) => Modulatable (a, b) where
  modulate (a, b) = [I,I] <> modulate a <> modulate b

instance Modulatable a => Modulatable (Maybe a) where
  modulate = maybe (modulate ()) modulate

-- | See “mod_list” from “modulator.py”
instance Modulatable a => Modulatable [a] where
  modulate [] = modulate ()
  -- ↓ This pattern wasn’t in the “modulator.py”, I assumed it from the code
  -- modulate [a] = modulate a
  -- ↓ This pattern was in the “modulator.py” but it seems it’s redundant
  -- modulate [a, b] = [I,I] <> modulate a <> modulate b
  modulate (init &&& last -> (init', last')) =
    foldMap (([I,I] <>) . modulate) init' <> modulate last'

instance Modulatable CallToAliens where
  -- ( 1, 0 )
  modulate Create = modulate ([1, 0] :: [Integer])
  -- (2, playerKey, (...unknown list...))
  modulate (Join playerKey) =
    modulate ([modulate (2 :: Integer), modulate playerKey, modulate ()] :: [_])
  -- (3, playerKey, (x0, x1, x2, x3))
  modulate (Start playerKey a b c d) =
    modulate ([ modulate (3 :: Integer)
              , modulate playerKey
              , modulate ([modulate a, modulate b, modulate c, modulate d] :: [_])
              ] :: [_])
  -- (4, playerKey, commands)
  modulate (Commands playerKey cmd) =
    modulate ([modulate (4 :: Integer), modulate playerKey, modulate cmd] :: [_])

instance Modulatable Command where
  -- (0, shipId, vector)
  modulate (Accelerate shipId vec) =
    modulate ([modulate (0 :: Integer), modulate shipId, modulate vec] :: [_])
  -- (1, shipId)
  modulate (Detonate shipId) =
    modulate ([modulate (1 :: Integer), modulate shipId] :: [_])
  -- (2, shipId, target, x3)
  modulate (Shoot shipId target) =
    modulate ([modulate (2 :: Integer), modulate shipId, modulate target] :: [_])

instance Modulatable ShipId where
  modulate = modulate . fromShipId

instance Modulatable Target where
  modulate (Target x y) = modulate ([modulate x, modulate y] :: [_])

instance Modulatable Vec where
  modulate (Vec x y) = modulate ([modulate x, modulate y] :: [_])
