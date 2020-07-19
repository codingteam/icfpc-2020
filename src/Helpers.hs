module Helpers
     ( errPutStrLn
     , liftEither
     ) where

import System.IO (hPutStrLn, stderr)


errPutStrLn :: String -> IO ()
errPutStrLn = hPutStrLn stderr

liftEither :: Either String a -> IO a
liftEither = either fail pure
