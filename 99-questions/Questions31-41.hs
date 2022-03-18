

isPrime x = checkPrime x x
    where
        checkPrime x 0 = False
        checkPrime x 1 = False
        checkPrime x n 
            | x `mod` (n-1) /= 0 = checkPrime x (n-1)
            | otherwise = True