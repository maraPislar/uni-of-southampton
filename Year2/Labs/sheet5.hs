-- exercise 1
-- searching in a binary search tree

data Tree a = Leaf a | Node (Tree a) a (Tree a)

occurs :: Ord a => a -> Tree a -> Bool
occurs a (Leaf v) = a == v
occurs x (Node l v r) = case compare x v of
                            EQ -> True
                            LT -> occurs x l
                            GT -> occurs x r
                            
-- exercise 2
-- Note: foldr (\elem acc -> <term>) <start_acc> <list>
-- Note: foldl (\acc elem -> <term>) <start_acc> <list>

foldTree :: (t1 -> t2) -> (t2 -> t1 -> t2 -> t2) -> Tree t1 -> t2
foldTree f1 _ (Leaf v) = f1 v
foldTree f1 f2 (Node l v r) = f2 (foldTree f1 f2 l) v (foldTree f1 f2 r)

flatten :: Tree a -> [a]
flatten = foldTree (\x -> [x]) (\ls x rs -> ls ++ (x:rs))
