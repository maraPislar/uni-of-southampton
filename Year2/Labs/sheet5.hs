-- exercise 1
-- searching in a binary search tree

data Tree a = Leaf a | Node (Tree a) a (Tree a)

occurs :: Ord a => a -> Tree a -> Bool
occurs a (Leaf v) = a == v
occurs x (Node l v r) = case compare x v of
                            EQ -> True
                            LT -> occurs x l
                            GT -> occurs x r
