import Control.Exception
import Data.ByteString.Lazy.UTF8 as BLU
import Data.Int
import Network.HTTP.Simple
import System.Environment
import System.Random

main = catch (
    do
        args <- getArgs
        putStrLn ("ServerUrl: " ++ args!!0 ++ "; PlayerKey: " ++ args!!1)
        request' <- parseRequest ("POST " ++ (args!!0))
        number <- (randomIO :: IO Int32)
        let request = setRequestBodyLBS (BLU.fromString (show number)) request'
        response <- httpLBS request
        let statuscode = show (getResponseStatusCode response)
        case statuscode of
            "200" -> putStrLn ("Server response: " ++ BLU.toString (getResponseBody response))
            _ -> putStrLn ("Unexpected server response:\nHTTP code: " ++ statuscode ++ "\nResponse body: " ++ BLU.toString (getResponseBody response))
    ) handler
    where
        handler :: SomeException -> IO ()
        handler ex = putStrLn $ "Unexpected server response:\n" ++ show ex
