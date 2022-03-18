data Tree a = Node (Tree a) a (Tree a)
            | Empty
  deriving (Show, Eq)

treeDepth :: (Eq a ) => Tree a -> Int--  Int
treeDepth Empty = 0
treeDepth (Node left x right) 
    | left /= Empty && right /= Empty = 1 + max (treeDepth left) (treeDepth right)
    | left == Empty && right /= Empty = 1 +  treeDepth right
    | left /= Empty && right == Empty = 1 + treeDepth left
    | otherwise = 1 + max (treeDepth left) (treeDepth right)
    
main = do
    print $ treeDepth (Node (Node (Node Empty 2 (Node Empty 2 Empty)) 2 Empty) 1 (Node Empty 2 (Node Empty 2 (Node Empty 2 Empty))))