{-# LANGUAGE DerivingStrategies #-}

module Modulatable.Types
     ( CallToAliens (..)
     , Command (..)
     , ShipId (..)
     , ShipParameters (..)
     , ShipType (..)
     , Target (..)
     , Vec (..)
     , decodeShipType
     , shipTypeToInteger
     ) where

import Invaluator (Data)
import Newtypes (PlayerKey)

data ShipType = Default -- 15
              | Mothership -- 24
              | Zero -- 0
              deriving (Show, Eq)

shipTypeToInteger :: ShipType -> Integer
shipTypeToInteger Zero = 0
shipTypeToInteger Default = 15
shipTypeToInteger Mothership = 24

decodeShipType :: Integer -> ShipType
decodeShipType 0 = Zero
decodeShipType 15 = Default
decodeShipType 24 = Mothership

data ShipParameters = ShipParameters {
  fuel :: Integer,
  guns :: Integer,
  shipType :: ShipType,
  shipX4 :: Integer
} deriving (Show, Eq)

-- | See the docs
--   https://message-from-space.readthedocs.io/en/latest/game.html#protocol
data CallToAliens
   = Create
   -- ^ Request new PlayerKey

   | Join PlayerKey

   | Start PlayerKey Integer Integer Integer Integer
   -- ^ Four numbers are initial ship parameters.
   --   @x y w h@? or @x1 y1 x2 y2@?

   | Commands PlayerKey [Command]

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
   | Shoot      ShipId Target Data
   | Spawn ShipId ShipParameters
   | UnknownCommand [Data]
     deriving stock (Show, Eq)
