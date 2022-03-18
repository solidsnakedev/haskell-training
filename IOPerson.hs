import System.IO (hFlush, stdin,stdout, hSetBuffering, BufferMode (NoBuffering))

flush = hFlush stdout

getFullName :: IO Person
getFullName =
    putStrLn "Enter firstname: " >>
    getLine   >>= \fn ->
    putStrLn "Enter lastname: " >>
    readLn  >>= \ln ->
    putStrLn "Enter Age: " >>
    readLn  >>= \age' ->
    putStrLn "Enter Color: " >>
    readLn  >>= \color' ->
    pure (Person fn ln age' color')

--helper :: Read a => String -> IO a
helper msg = putStrLn msg >> readLn

data Color = Red | Green | Blue
    deriving (Show, Read)

data Person = Person {
    fname :: String,
    lname :: String,
    age   :: Int,
    color :: Color
}
    deriving Read

instance Show Person where
  show p = "the first name is " ++ show (fname p) ++
                                        "\nthe lastname name is " ++ show (lname p) ++
                                        "\nage is " ++ show (age p) ++
                                        "\nfav color is " ++ show (color p) ++ " "

main :: IO ()
main = do 
    name <- getFullName
    putStrLn . show $ name