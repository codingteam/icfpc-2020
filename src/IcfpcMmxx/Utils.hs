{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}

module IcfpcMmxx.Utils
     ( prettyPrintJSON

     -- * Stringy enum default "ToJSON" and "FromJSON" implementation
     , stringyEnumToJSON
     , stringyEnumFromJSON
     ) where

import Data.Aeson (ToJSON (toJSON), FromJSON (parseJSON))
import Data.Aeson.Types (Parser, Value (String), typeMismatch)
import Data.String (IsString (fromString))
import Data.Typeable (Typeable, typeRep, Proxy (Proxy))
import Data.List (find)
import qualified Data.Text.Lazy.Builder as TL (toLazyText)
import qualified Data.Text.Lazy.IO as TL (putStrLn)
import Data.Aeson.Encode.Pretty (encodePrettyToTextBuilder)


prettyPrintJSON :: ToJSON a => a -> IO ()
prettyPrintJSON = TL.putStrLn . TL.toLazyText . encodePrettyToTextBuilder


-- * Stringy enum default "ToJSON" and "FromJSON" implementation

stringyEnumToJSON :: Show a => a -> Value
stringyEnumToJSON = String . fromString . show

stringyEnumFromJSON
  :: forall a. (ToJSON a, Enum a, Bounded a, Typeable a)
  => Value
  -> Parser a

stringyEnumFromJSON src =
  maybe mismatch pure $ find ((src ==) . toJSON) [minBound .. maxBound :: a]
  where mismatch = typeMismatch (show (typeRep (Proxy @a))) src
