{-# LANGUAGE DerivingStrategies #-}

module Modulatable.Types
     ( CallToAliens (..)
     , Command (..)
     , ShipId (..)
     , Target (..)
     , Vec (..)
     ) where

import Newtypes (PlayerKey)


-- | See the docs
--   https://message-from-space.readthedocs.io/en/latest/game.html#protocol
data CallToAliens
   = Create
   -- ^ Request new PlayerKey

   | Join PlayerKey

   | Start PlayerKey Integer Integer Integer Integer
   -- ^ Four numbers are initial ship parameters.
   --   @x y w h@? or @x1 y1 x2 y2@?

   | Commands PlayerKey Command

     deriving stock (Eq, Show)

newtype ShipId
      = ShipId { fromShipId :: Integer }
        deriving stock (Eq, Show)

data Vec
   = Vec
   { vecX :: Integer
   , vecY :: Integer
   } deriving stock (Eq, Show)

data Target
   = Target
   { targetX :: Integer
   , targetY :: Integer
   } deriving stock (Eq, Show)

data Command
   = Accelerate ShipId Vec
   | Detonate   ShipId
   | Shoot      ShipId Target -- ^ It’s not clear, see the docs
     deriving stock (Show, Eq)
