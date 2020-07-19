{-# LANGUAGE DerivingStrategies, GeneralizedNewtypeDeriving, LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Bits
     ( Bit (..)
     , Bits (..)
     , bitsWithSigNum
     , bitsLength
     , replicateBits
     ) where

import GHC.Exts (IsList (Item, fromList, toList))

import Text.Printf (IsChar (toChar, fromChar), printf)
import Text.Read (readPrec, get)
import Data.Typeable (Typeable)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Numeric.Natural (Natural)

import Control.Applicative (many)


-- * "Bit"

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


-- * "Bits"

newtype Bits = Bits (Vector Bit)
  deriving stock (Eq, Typeable)
  deriving newtype (Semigroup, Monoid)

instance Show Bits where
  show (Bits x) = V.toList (V.map toChar x)

instance Read Bits where
  readPrec = Bits . V.fromList <$> many1 readPrec
    where many1 p = (:) <$> p <*> many p

-- | Support @OverloadedLists@
instance IsList Bits where
  type Item Bits = Bit
  fromList = Bits . V.fromList
  toList (Bits x) = V.toList x


-- * Related functions

-- | First is signum, second is absolute number.
bitsWithSigNum :: Integer -> ((Bit, Bit), Bits)
bitsWithSigNum x =
  ( if x >= 0 then (O,I) else (I,O)
  , Bits $ V.fromList $ printf "%b" (fromInteger (abs x) :: Natural)
  )

bitsLength :: Num i => Bits -> i
bitsLength (Bits v) = fromIntegral (V.length v)

replicateBits :: Int -> Bit -> Bits
replicateBits n = Bits . V.replicate n
