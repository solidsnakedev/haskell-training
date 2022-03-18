{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import Data.Fixed (mod')
list :: [Integer]
list = [1, 2, 3, 4, 5]
data MessageType
    = Information Int String
    | Warning Int String
    | Error Int Int String
    deriving Show

betterLogFileLines :: [MessageType]
betterLogFileLines = [ Information 147 "Vader has risen"
    , Error 2 148 "The droids have invaded the star-ship"
    ]

data BinaryTree
    = Empty
    | Node Int BinaryTree BinaryTree
    deriving Show

treeBinary :: BinaryTree
treeBinary = Node 2 (Node 3 Empty (Node 5 Empty Empty)) (Node 4 Empty Empty)

------ SKips --------

createIndex :: [a] -> [(Int,a)]
createIndex x = zip [1..length x] x

getEven :: [(Int,a)] -> [a]
getEven [] = []
getEven ((x,y):xs) =
  if x `mod` 2 == 0 then
    [y] ++ getEven xs
  else
    getEven xs

getSnd :: [a] -> [a]
getSnd [] = []
getSnd x = getEven $ createIndex x

getElem :: [a]->[[a]]
getElem [] = []
getElem (x:xs) = [x] : getElem xs

skips :: [a] -> [[a]]
skips (x:[]) = [x:[]]
skips (x:y:xs) = [x:y:xs] ++ [getSnd (x:y:xs)] ++ getElem (xs)


------ LocalMaxima --------
takeFst :: [Int] -> Int
takeFst [] = 0
takeFst x =
  if length x >= 3 then
    head x
  else
    0

takeThr :: [Int] -> Int
takeThr [] = 0
takeThr x =
  if length x >= 3 then
    last $ take 3 x
  else
    0

takeSnd :: [Int] -> Int
takeSnd [] = 0
takeSnd x =
  if length x >= 3 then
    head $ drop 1 $ init $ take 3 x
  else
    0

localMaxima :: [Int] -> [Int]
localMaxima []= []
localMaxima (_:[]) = []
localMaxima (x:xs) =
  if sndElem > fstElem && sndElem > thrElem then
    [sndElem] ++ localMaxima xs
  else
    localMaxima xs
  where fstElem = takeFst (x:xs)
        thrElem = takeThr (x:xs)
        sndElem = takeSnd (x:xs)

------ Histogram --------
filterNumbers :: [Int] -> [Int]
filterNumbers x =
  [length [y | y <-x, y==1]] ++
  [length [y | y <-x, y==2]] ++
  [length [y | y <-x, y==3]] ++
  [length [y | y <-x, y==4]] ++
  [length [y | y <-x, y==5]] ++
  [length [y | y <-x, y==6]] ++
  [length [y | y <-x, y==7]] ++
  [length [y | y <-x, y==8]] ++
  [length [y | y <-x, y==9]]

--filterNumbers' = (==) <$> [1,12] <*> [2,3,1,1,1]
--filterNumbers' = filter (==1) [2,3,1,1,1]
filterNumbers' = (\x y -> [y | x == y]  ) <$> [1,2] <*> [2,3,1,1,1]



--Convert Int list of hisogram and convert it to either * or =
cNumToChar :: Int -> [Int] -> [String]
cNumToChar y [] = []
cNumToChar y (x:xs) =
    [replicate (y-x) '_' ++ replicate x '*'] ++ cNumToChar y xs

-- In order to make a vertical diagram we need to transpose the diagram
transposeList :: [String] -> [String]
transposeList [] = []
transposeList (x:xs) =
  if length (concat (x:xs)) == 0 then
    []
  else
    map head (x:xs) : transposeList (map tail (x:xs))

getHistogram :: [Int] -> String
getHistogram x = _result
  where
    _filterNumbers = filterNumbers x
    _cNumToChar = cNumToChar (maximum _filterNumbers) _filterNumbers
    _transposeList = transposeList _cNumToChar
    _result = unlines _transposeList

--fTest :: (b -> c) (a -> b) (a -> c)
--fTest f g =  \x -> f(g x)

main :: IO ()
main = do
    print list

    print $ head list
    print $ tail list
    print $ last list
    print $ init list

    print betterLogFileLines
    print treeBinary

    putStrLn "\n----skips----"
    print $ skips "ABCD"
    putStrLn "\n----Histogram----"
    putStrLn $ getHistogram [1,5,5,5,5,5,5,1,4,4,4,2,2,3,3,6,6,3,7,7,7,8,8,9]
    putStrLn "\n----localMaxima----"
    print $ localMaxima [1,2,1,3,2,1]

    print filterNumbers'