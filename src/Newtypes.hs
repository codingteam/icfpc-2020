{-# LANGUAGE DerivingStrategies, GeneralizedNewtypeDeriving #-}

module Newtypes
     ( BaseUrl, fromBaseUrl, mkBaseUrl
     , AliensResponseId, fromAliensResponseId, mkAliensResponseId
     ) where

import Control.Monad (guard)


newtype BaseUrl = BaseUrl String deriving (Eq, Show)

fromBaseUrl :: BaseUrl -> String
fromBaseUrl (BaseUrl x) = x

mkBaseUrl :: String -> IO BaseUrl
mkBaseUrl src = do
  guard (src /= mempty)
  pure (BaseUrl src)


-- | This is UUID
newtype AliensResponseId = AliensResponseId String deriving (Eq, Show)

fromAliensResponseId :: AliensResponseId -> String
fromAliensResponseId (AliensResponseId x) = x

mkAliensResponseId :: String -> IO AliensResponseId
mkAliensResponseId src = do
  guard (src /= mempty)
  guard (length src == 8+4+4+4+12 + 4)
  pure (AliensResponseId src)
