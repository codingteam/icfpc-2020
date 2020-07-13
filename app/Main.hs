import System.Environment
import Network.HTTP.Simple

main = do
  [serverUrl, playerKey] <- getArgs
  putStrLn ("ServerUrl: " ++ serverUrl ++ "; PlayerKey: " ++ playerKey)
  request <- parseRequest (serverUrl ++ "?playerKey=" ++ playerKey)
  httpLBS request
