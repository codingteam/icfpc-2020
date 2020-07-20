{-# LANGUAGE RankNTypes #-}

module Production
     ( production
     ) where

import Control.Monad (forever)
import Data.Maybe (listToMaybe)

import GameState (GameRole(..), GameState(..), ServerResponse(..), decodeResponse, sgiRole, shipId, shipParameters)
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

findMyShip :: ServerResponse -> ShipType -> Maybe ShipId
findMyShip (Success _ (Just sgi) (Just (GameState _ _ shipData))) type_ =
  let myRole = sgiRole sgi
      ships = map fst shipData 
  in listToMaybe $ map shipId $ filter (isShipOfType type_) ships
  where isShipOfType type_ ship = (shipType $ shipParameters ship) == type_
  
findMyShip _ _ = Nothing

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
  result <- talkWithAliens $ startMessage playerKey 5 10 Mothership 20
  errPutStrLn $ "Start result: " ++ show result
  let response = decodeResponse result 
  errPutStrLn $ "Start result (decoded): " ++ show response
  
  response <- spawnShip response
  response <- if isAttacker response 
    then do destroyMyShip response Default
    else do destroyMyShip response Mothership 
  
  forever $ do
    doNothing

  where
  
  isAttacker (Success _ (Just info) _) =
    case sgiRole info of
      Attacker -> True
      Defender -> False
  
  spawnShip response = do
    errPutStrLn "----- Sending Spawn request -----"
    let (Just myShipId) = findMyShip response Mothership
    result <- talkWithAliens $ Commands playerKey [Spawn myShipId $ ShipParameters 0 0 Zero 1]
    errPutStrLn $ "Spawn result: " ++ show result
    let response = decodeResponse result 
    errPutStrLn $ "Spawn result (decoded): " ++ show response
    return response

  destroyMyShip response type_ = do
    errPutStrLn "----- Sending Destroy request -----"
    errPutStrLn $ "Destroying ship of type: " ++ show type_
    let (Just myShipId) = findMyShip response type_
    result <- talkWithAliens $ Commands playerKey [Detonate myShipId]
    errPutStrLn $ "Destroy result: " ++ show result
    let response = decodeResponse result 
    errPutStrLn $ "Destroy result (decoded): " ++ show response
    return response

  doNothing = do
    errPutStrLn "----- Sending empty command list -----"
    result <- talkWithAliens $ Commands playerKey []
    errPutStrLn $ "Empty command list result: " ++ show result
    let response = decodeResponse result 
    errPutStrLn $ "Empty command list result (decoded): " ++ show response