printFromTo :: Int -> Int -> Int -> IO ()
printFromTo minValue maxValue currValue =
    if minValue <= currValue && currValue <= maxValue then
    print currValue >>
    printFromTo minValue maxValue (currValue + 1)
    else
        putStrLn "end loop"


main = do
    printFromTo 2 10 2