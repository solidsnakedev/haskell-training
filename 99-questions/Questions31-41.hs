

isPrime x = checkPrime x (x-1)
    where
        checkPrime x 0 = True
        checkPrime x 1 = True
        checkPrime x n
            | (x `mod` n) == 0 = False
            | otherwise = checkPrime x (n-1)


-- (gcd 36 63)
-- 9

gcd x y =  getGCD (abs x) (abs y) (min (abs x) (abs y))
    where
        getGCD x y 0 = 0
        getGCD x y minN
            | (x `mod` minN) == 0 && (y `mod` minN) == 0 = minN
            | otherwise = getGCD x y (minN - 1)