{-# LANGUAGE RankNTypes #-}

module Production
     ( production
     ) where

import Control.Monad (forever)

import Invaluator (Data(..))
import HttpApi (sendMessageToAliens)
import Modulator
import Newtypes
import Modulatable.Types (Command(..), CallToAliens(..))

joinMessage :: PlayerKey -> CallToAliens
joinMessage playerKey = CallToAliens Join playerKey Nothing Nothing

production :: PlayerKey -> (forall a. Modulatable a => a -> IO Data) -> IO ()
production playerKey talkWithAliens = do
  forever $ do
    result <- talkWithAliens $ joinMessage playerKey
    putStrLn $ show result
