import System.Environment

main = do  
    args <- getArgs
    putStrLn (args!!0 ++ " " ++ args!!1)
