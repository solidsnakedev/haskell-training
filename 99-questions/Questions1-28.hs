
myLast [x] = x
myLast (x:xs) = myLast xs


myButLast (y:x:[]) = y
myButLast (x:xs) = myButLast xs


elementAt :: [a] -> Int -> a
elementAt x 0 = error "ada"
elementAt x y = x !! (y-1)


myLength [] = 0
myLength [x] = 1
myLength (x:xs) = 1 + myLength xs

myReverse [x] = [x]
myReverse (x:xs) = myReverse xs ++ [x]

isPalindrome x = x == myReverse x

data NestedList a = Elem a | List [NestedList a]

--flatten (Elem x) = [x]
--flatten (x:xs) = flatten x ++ flatten xs
--flatten (Elem:rx) = x
--flatten (x:xs) = flatten x : flatten xs
flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List (x:xs)) = flatten x ++ flatten (List xs)
flatten (List []) = []
--flatten (List (x:xs))= xs

-- flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])
--[1,2,3,4,5]

compress [] = []
compress [x] = [x]
compress (x:y:xs)
  | x == y = compress (y:xs)
  | otherwise = [x] ++ compress (y:xs)


--compress "aaaabccaadeeee"
--"abcade"
pack :: String -> [String]
pack [] = []
pack x = first : pack second
  where (first,second) = span (==head x) x

--pack ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a','a', 'd', 'e', 'e', 'e', 'e']
--["aaaa","b","cc","aa","d","eeee"]

binarySearch [] y = False
binarySearch x y
  | x !! (length x `div` 2) == y = True
  | x !! (length x `div` 2) > y = binarySearch (take (length x `div` 2) x) y
  | x !! (length x `div` 2) < y = binarySearch (drop ((length x `div` 2)+1) x) y

--encode "aaaabccaadeeee"
--[(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]

encode = map (\x -> (length x, head x)) . pack

-- encodeModified "aaaabccaadeeee"
--[Multiple 4 'a',Single 'b',Multiple 2 'c',
-- Multiple 2 'a',Single 'd',Multiple 4 'e']

data MultipleSingle = Multiple Int Char | Single Char
  deriving Show

encodeModified = map (\x -> if length x == 1 then Single (head x) else Multiple (length x) (head x)) . pack

--decodeModified [Multiple 4 'a',Single 'b',Multiple 2 'c', Multiple 2 'a',Single 'd',Multiple 4 'e']
--decodeModified = map (decodeModified')
decodeModified [] = []
decodeModified ((Multiple num char):xs) =  take num (repeat char) ++ decodeModified xs
decodeModified ((Single char):xs) = [char] ++ decodeModified xs


--dupli [1, 2, 3]
--[1,1,2,2,3,3]
dupli [] = []
dupli (x:xs) = [x] ++ [x] ++ dupli xs

--repli "abc" 3
--"aaabbbccc"
repli [] y = []
repli (x:xs) y = take y (repeat x) ++ repli xs y

--dropEvery "abcdefghik" 3
-- "abdeghk"

dropEvery [] num = []
dropEvery [x] num = [x]
dropEvery list num = init (take num list) ++ dropEvery (drop num list) num

--split "abcdefghik" 3
--("abc", "defghik")

split list num = (take num list , drop num list)

--slice ['a','b','c','d','e','f','g','h','i','k'] 3 7
--"cdefg"

slice list num1 num2 = drop (num1-1) $ take num2 list


--rotate ['a','b','c','d','e','f','g','h'] 3
--"defghabc"

rotate x y
    | y > 0 = drop y x ++ take y x
    | otherwise =  drop (length x + y) x ++ take (length x + y) x

-- removeAt 2 "abcd"
-- ('b',"acd")    
removeAt 0 x = (' ',x)
removeAt y x
    | length x >= y = (x !! (y-1) , take (y-1) x ++ drop y x)
    | otherwise = (' ',x)

-- insertAt 'X' "abcd" 2
-- "aXbcd"

insertAt elem list index = take (index -1) list ++ [elem] ++ drop (index -1) list


-- range 4 9
--[4,5,6,7,8,9]

range index1 index2 = [index1 .. index2]

-- combinations 3 "abcdef"
-- ["abc","abd","abe",...]
combinations 0 xs = [[]]
combinations n xs = [ xs !!i : x  | i <- [0..(length xs)-1] , x <- combinations (n-1) (drop (i+1) xs) ]

grouping nx xs =  map (\x -> combinations x xs) nx



-- (group3 '(aldo beat carla david evi flip gary hugo ida))
--( ( (ALDO BEAT) (CARLA DAVID EVI) (FLIP GARY HUGO IDA) )
-- ... )

-- lsort ["abc","de","fgh","de","ijkl","mn","o"]
-- ["o","de","de","mn","abc","fgh","ijkl"]
lsort [] = []
lsort (x:xs) = getMinimum (x:xs) : lsort (dropN (getMinimum (x:xs)) (x:xs))

getMinimum :: Foldable t => [t a] -> t a
getMinimum xs = foldl (\acc xs' -> if length acc > length xs' then xs' else acc ) (head xs) xs

-- foldl (if "de" > "abc" then "de" else "abc")
-- \acc xs' -> if xs' > acc then xs' else acc
dropN n (x:xs)
    | n == x = xs
    | otherwise = x:dropN n xs

--    lfsort ["abc", "de", "fgh", "de", "ijkl", "mn", "o"]
--["ijkl","o","abc","fgh","de","de","mn"]

lfsort x = reverse [freq | x' <- compress (map length (lsort x)), freq <- filter (\x -> x' == length x) x]

lfsort' x = do
    x' <- compress (map length (lsort x))
    filter (\x -> x' == length x) x

lfsort'' list = map (\y -> filter (\x -> length x == y) list) (compress (map length (lsort list)))
tsort n = filter (\x -> n == length x) 


    --compress (map length (lsort x))

-- map (\x' -> filter (\y -> length y == x' ) x ) compress (map length (lsort x))