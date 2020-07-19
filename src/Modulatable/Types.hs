{-# LANGUAGE DerivingStrategies #-}

module Modulatable.Types
     ( Command (..)
     , CallToAliens (..)
     , UnknownYetThirdValue (..)
     , UnknownYetFourthValue (..)
     ) where

import Newtypes (PlayerKey)


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
