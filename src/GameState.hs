module GameState where

import Invaluator (Data(..))
import Modulatable.Types (Command(..), ShipId(..), Target(..), Vec(..))

data ServerResponse =
    Failure
  | Success GameStage StaticGameInfo GameState
  deriving Show

data GameStage = NotStarted -- 0
               | Started -- 1
               | Finished -- 2
               deriving Show

data StaticGameInfo = StaticGameInfo {
  sgiX0 :: Data,
  sgiRole :: GameRole,
  sgiX2 :: Data,
  sgiX3 :: Data,
  sgiX4 :: Data
} deriving Show

data GameState = GameState Integer Data [(Ship, [Command])]
                 deriving Show

data GameRole = Attacker -- 0
              | Defender -- 1
              deriving Show

data Ship = Ship {
  shipRole :: GameRole,
  shipId :: ShipId,
  shipPosition :: Vec,
  shipVelocity :: Vec,
  shipX4 :: Data,
  shipX5 :: Data,
  shipX6 :: Data,
  shipX7 :: Data
} deriving Show

decodeList :: Data -> [Data]
decodeList datum =
  case datum of
    DNil -> []
    DCons v DNil -> [v]
    DCons x y -> x : decodeList y
    _ -> error $ "Unknown datum when decoding a list: " ++ show datum

decodeResponse :: Data -> ServerResponse
decodeResponse datum =
  case decodeList datum of
    [DNum 0] -> Failure
    [DNum 1, gameStage, staticGameInfo, gameState] ->
     Success (decodeGameStage gameStage) (decodeStaticGameInfo staticGameInfo) (decodeGameState gameState)

decodeGameStage :: Data -> GameStage
decodeGameStage (DNum 0) = NotStarted
decodeGameStage (DNum 1) = Started
decodeGameStage (DNum 2) = Finished

decodeStaticGameInfo :: Data -> StaticGameInfo
decodeStaticGameInfo datum =
  case decodeList datum of
  [x0, role, x2, x3, x4] -> StaticGameInfo x0 (decodeGameRole role) x2 x3 x4
  
decodeGameState :: Data -> GameState
decodeGameState datum =
  case decodeList datum of
  [(DNum gameTick), x1, shipsAndCommands] -> GameState gameTick x1 $ decodeShipsAndCommands shipsAndCommands

decodeGameRole :: Data -> GameRole
decodeGameRole (DNum 0) = Attacker
decodeGameRole (DNum 1) = Defender

decodeShipsAndCommands :: Data -> [(Ship, [Command])]
decodeShipsAndCommands datum =
  decode $ decodeList datum
  where decode [] = []
        decode (ship:rest) = decodeShipData ship:decode rest

decodeShipData :: Data -> (Ship, [Command])
decodeShipData datum =
  case decodeList datum of
  [ship, command] -> (decodeShip ship, decodeCommands command)

decodeShip :: Data -> Ship
decodeShip datum =
  case decodeList datum of
  [role, shipId, position, velocity, x4, x5, x6, x7] ->
    Ship (decodeGameRole role) (decodeShipId shipId) (decodeVec position) (decodeVec velocity) x4 x5 x6 x7

decodeCommands :: Data -> [Command]
decodeCommands datum =
  map decodeCommand $ decodeList datum

decodeCommand :: Data -> Command
decodeCommand datum =
  case decodeList datum of
  [DNum 0, shipId, vector] -> Accelerate (decodeShipId shipId) (decodeVec vector)
  [DNum 1, shipId] -> Detonate (decodeShipId shipId)
  [DNum 2, shipId, target, x3] -> Shoot (decodeShipId shipId) (decodeTarget target) x3
  
decodeShipId :: Data -> ShipId
decodeShipId (DNum id) = ShipId id

decodeVec :: Data -> Vec
decodeVec (DCons (DNum x) (DNum y)) = Vec x y

decodeTarget :: Data -> Target
decodeTarget (DCons (DNum x) (DNum y)) = Target x y