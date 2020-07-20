{-# LANGUAGE RankNTypes #-}

module Production
     ( production
     ) where

import Control.Monad (forever)

import GameState (ShipType(..), decodeResponse, shipTypeToInteger)
import Helpers (errPutStrLn)
import Invaluator (Data(..))
import HttpApi (sendMessageToAliens)
import Modulator
import Newtypes
import Modulatable.Types

joinMessage :: PlayerKey -> CallToAliens
joinMessage playerKey = Join playerKey

startMessage :: PlayerKey -> Integer -> Integer -> ShipType -> Integer -> CallToAliens
startMessage playerKey fuel guns type_ x = Start playerKey fuel guns (shipTypeToInteger type_) x

detonateMessage :: PlayerKey -> Integer -> Data
detonateMessage playerKey shipId =
  -- Fornever, maybe try this?:
  --
  -- @
  -- Commands playerKey [Detonate (ShipId shipId)]
  -- @
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

--
-- @talkWithAliens@ is compatible with these inferred types for instance:
--
-- * @Data -> IO Data@
-- * @CallToAliens -> IO Data@
--
production :: PlayerKey -> (forall a. Modulatable a => a -> IO Data) -> IO ()
production playerKey talkWithAliens = do
  errPutStrLn "----- Sending Join request -----"
  result <- talkWithAliens $ joinMessage playerKey
  errPutStrLn $ "Join result: " ++ show result
  let response = decodeResponse result 
  errPutStrLn $ "Join result (decoded): " ++ show response 
  errPutStrLn "----- Sending Start request -----"
  result <- talkWithAliens $ startMessage playerKey 5 10 Default 20
  errPutStrLn $ "Start result: " ++ show result
  let response = decodeResponse result 
  errPutStrLn $ "Start result (decoded): " ++ show response
  forever $ do
    errPutStrLn "----- Sending command request -----"
    result <- talkWithAliens $ detonateMessage playerKey 0
    errPutStrLn $ "Detonate result: " ++ show result
    let response = decodeResponse result 
    errPutStrLn $ "Detonate result (decoded): " ++ show response
