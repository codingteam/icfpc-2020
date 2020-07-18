{-# LANGUAGE TypeApplications, GADTs, ScopedTypeVariables #-}

module Newtypes
     ( BaseUrl, fromBaseUrl, parseBaseUrl
     , AliensResponseId, fromAliensResponseId, parseAliensResponseId
     , PlayerKey, fromPlayerKey, parsePlayerKey
     , ApiKey, fromApiKey, parseApiKey
     ) where

import Data.Typeable (Typeable, typeRep)
import Data.Proxy (Proxy (Proxy))
import Numeric.Natural (Natural)
import Text.Read (readEither)
import Data.Bifunctor (bimap)

import Control.Monad (when)


newtype BaseUrl = BaseUrl String deriving (Eq, Show, Typeable)

fromBaseUrl :: BaseUrl -> String
fromBaseUrl (BaseUrl x) = x

parseBaseUrl :: forall a. a~BaseUrl => String -> Either String a
parseBaseUrl src = do
  failOn (src == mempty) "Input is empty"
  pure (BaseUrl src)
  where failOn = failOnGeneric (Proxy @a)


-- | This is UUID
newtype AliensResponseId = AliensResponseId String deriving (Eq, Show, Typeable)

fromAliensResponseId :: AliensResponseId -> String
fromAliensResponseId (AliensResponseId x) = x

parseAliensResponseId :: forall a. a~AliensResponseId => String -> Either String a
parseAliensResponseId src = do
  failOn (src == mempty) "Input is empty"
  failOn (length src /= (8+4+4+4+12) + 4) "String length doesn't match UUID"
  pure (AliensResponseId src)
  where failOn = failOnGeneric (Proxy @a)


newtype PlayerKey = PlayerKey Natural deriving (Eq, Show)

fromPlayerKey :: PlayerKey -> Natural
fromPlayerKey (PlayerKey x) = x

parsePlayerKey :: forall a. a~PlayerKey => String -> Either String a
parsePlayerKey src = do
  failOn (src == mempty) "Input is empty"
  failOn (any (`notElem` ['0'..'9']) src) "Only digits are allowed"
  failOn (head src == '0') "Can't start with zero"
  bimap ((typeName (Proxy @a) <>) . (": " <>)) PlayerKey (readEither src)
  where failOn = failOnGeneric (Proxy @a)


-- | It looks like 32 HEX digits: "0123456789abcdef0123456789abcdef"
newtype ApiKey = ApiKey String deriving (Eq, Show)

fromApiKey :: ApiKey -> String
fromApiKey (ApiKey x) = x

parseApiKey :: forall a. a~ApiKey => String -> Either String a
parseApiKey src = do
  failOn (src == mempty) "Input is empty"
  failOn (length src /= 32) "Length must be 32 symbols"
  failOn (any (`notElem` ['0'..'9'] <> ['a'..'f']) src)
    "Each symbol must be a hex digit"
  pure (ApiKey src)
  where failOn = failOnGeneric (Proxy @a)


-- * Helpers

typeName :: Typeable a => Proxy a -> String
typeName = show . typeRep

failOnGeneric
  :: forall a. Typeable a
  => Proxy a
  -> Bool
  -> String
  -> Either String ()

failOnGeneric proxy predicate msg =
  when predicate $
    Left (typeName proxy <> ": " <> msg)
