{-# LANGUAGE RankNTypes #-}

module Production
     ( production
     ) where

import Control.Monad (forever)

import Invaluator (Data(..))
import HttpApi (sendMessageToAliens)
import Modulator
import Newtypes
import Modulatable.Types

joinMessage :: PlayerKey -> CallToAliens
joinMessage playerKey = CallToAliens Join playerKey Nothing Nothing

startMessage :: PlayerKey -> CallToAliens
startMessage playerKey = CallToAliens Start playerKey (Just $ UnknownYetThirdValue 5 10 15 20) Nothing

detonateMessage :: PlayerKey -> Integer -> Data 
detonateMessage playerKey shipId =
  let
    command = 
      DCons (DNum 1) $
      DCons (DNum shipId) $
      DNil
    commands =
      DCons (command) $
      DNil
  in
  DCons (DNum 4) $
  DCons (DNum $ toInteger $ fromPlayerKey playerKey) $
  DCons commands $
  DNil

production :: PlayerKey -> (forall a. Modulatable a => a -> IO Data) -> IO ()
production playerKey talkWithAliens = do
  result <- talkWithAliens $ joinMessage playerKey
  putStrLn $ "Join result: " ++ show result
  result <- talkWithAliens $ startMessage playerKey
  putStrLn $ "Start result: " ++ show result
  forever $ do
    result <- talkWithAliens $ detonateMessage playerKey 0
    putStrLn $ "Detonate result: " ++ show result
