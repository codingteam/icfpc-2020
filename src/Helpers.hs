{-# LANGUAGE TypeFamilies, PolyKinds, DataKinds, TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Helpers
     ( errPutStrLn
     , liftEither
     , typeName
     , TypeIsOneOf
     ) where

import GHC.TypeLits

import Data.Kind (Type, Constraint)
import Data.Proxy (Proxy (Proxy))
import Data.Typeable (Typeable, typeRep)
import Data.String (IsString (fromString))

import System.IO (hPutStrLn, stderr)


errPutStrLn :: String -> IO ()
errPutStrLn = hPutStrLn stderr


liftEither :: Either String a -> IO a
liftEither = either fail pure


typeName :: (Typeable a, IsString s) => Proxy a -> s
typeName = fromString . show . typeRep


-- | Type-level "elem" as constraint (type is one of those in a list)
type family TypeIsOneOf (x :: Type) (xs :: [Type]) :: Constraint where
  TypeIsOneOf x xs = TypeIsOneOfInternal x xs xs

type family TypeIsOneOfInternal
  (x :: Type) (xs :: [Type]) (origin :: [Type]) :: Constraint where
    TypeIsOneOfInternal x (x ': xs) oxs = ()
    TypeIsOneOfInternal x (_ ': xs) oxs = (TypeIsOneOfInternal x xs oxs)

    TypeIsOneOfInternal x '[] oxs =
      TypeError (
        'Text "Type " ':<>:
        'ShowType x ':<>:
        'Text " not found in a list of allowed types: " ':<>:
        'ShowType oxs
      )
