{-# LANGUAGE ViewPatterns, ScopedTypeVariables, OverloadedLists  #-}
{-# LANGUAGE FlexibleInstances, PartialTypeSignatures #-}

{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Modulator
     ( Modulatable (..)
     ) where

import Numeric.Natural (Natural)

import Newtypes (PlayerKey, fromPlayerKey)
import Invaluator (Data (..))
import Bits
import Modulatable.Types


-- | Encodable for the aliens
class Modulatable entity where
  modulate :: entity -> Bits

instance Modulatable Bits where
  modulate = id

-- | Nil-pattern
--
-- See “None” handler in “modulate” from “modulator.py”.
instance Modulatable () where
  modulate () = [O,O]

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

-- | Pair of @(x, y)@ is encoded as @11 x y@
instance (Modulatable a, Modulatable b) => Modulatable (a, b) where
  modulate (a, b) = [I,I] <> modulate a <> modulate b

-- | List of @[x, y]@ is encoded as @11 x 11 y 00@
instance Modulatable a => Modulatable [a] where
  modulate [] = modulate ()
  modulate xs = foldMap (([I,I] <>) . modulate) xs <> modulate ()

instance Modulatable Data where
  modulate DNil        = modulate ()
  modulate (DNum x)    = modulate x
  modulate (DCons x y) = modulate (x, y)

instance Modulatable CallToAliens where
  -- ( 1, 0 )
  modulate Create = modulate ([1, 0] :: [Integer])
  -- (2, playerKey, (...unknown list...))
  modulate (Join playerKey) =
    modulate ([ modulate (2 :: Integer)
              , modulate playerKey
              , modulate ([] :: [()]) -- 🤷 just empty list (Nil)
              ] :: [_])
  -- (3, playerKey, (x0, x1, x2, x3))
  modulate (Start playerKey a b c d) =
    modulate ([ modulate (3 :: Integer)
              , modulate playerKey
              , modulate ([ modulate a
                          , modulate b
                          , modulate c
                          , modulate d
                          ] :: [_])
              ] :: [_])
  -- (4, playerKey, commands)
  modulate (Commands playerKey cmd) =
    modulate ([ modulate (4 :: Integer)
              , modulate playerKey
              , modulate cmd
              ] :: [_])

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
  modulate (Target x y) = modulate (modulate x, modulate y)

instance Modulatable Vec where
  modulate (Vec x y) = modulate (modulate x, modulate y)

instance Modulatable PlayerKey where
  modulate = modulate . fromPlayerKey

instance Modulatable Natural where
  modulate = modulate . toInteger

instance Modulatable a => Modulatable (Maybe a) where
  modulate = maybe (modulate ()) modulate
