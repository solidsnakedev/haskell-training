
readLn' :: IO [Char]
readLn' = do
    x <- getLine
    readIO x
    --getLine >>= readIO

main :: IO ()
main = do

    --readLn' >>= putStrLn
    --x <- readLn'
    x <- getLine 
    y <- readIO x
    putStrLn y