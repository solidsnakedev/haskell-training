--import Create.Box
import System.Random
import System.IO
import System.Environment


main :: IO ()
main = do
    (command:args) <- getArgs 
    getCommand command args


getCommand :: String -> [String] -> IO ()
getCommand comm args 
    | comm == "set" = set args
    | otherwise = putStrLn "wrong command"
    
set :: [String] -> IO ()    
set = mapM_ print
    


    --let rows' = readIO rows
   {-
    a <- randomRIO (0,9)
    b <- randomRIO (0,9)
    acceleration = 100000 -- 0.1 seconds
    let vector = Direction GoDown GoLeft
    let velocity = Velocity {
        initialVel = 1000000, --1 second
        finalVel = 100000, -- 0.1 seconds
    }
    moveBox' (10,10) (a,b) vector (initialVel velocity) velocity
    -}