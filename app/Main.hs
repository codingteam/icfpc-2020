import System.Environment
import Network.HTTP.Simple

main = do  
    args <- getArgs
    putStrLn ("ServerUrl: " ++ args!!0 ++ "; PlayerKey: " ++ args!!1)
    request <- parseRequest (args!!0 ++ "?playerKey=" ++ args!!1)
    httpLBS request
