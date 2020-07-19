{-# LANGUAGE RankNTypes #-}

module Production
     ( production
     ) where

import Invaluator (Data)
import Modulator
import Newtypes


-- | TODO implement whatever logic is needed for production
production :: PlayerKey -> (forall a. Modulatable a => a -> IO Data) -> IO ()
production playerKey talkWithAliens = fail "TODO implement"
