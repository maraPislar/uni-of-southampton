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

--exercise 3

data Expr = Val Int | Add Expr Expr | Sub Expr Expr

foldExpr :: (Int -> t) -> (t -> t -> t) -> (t -> t -> t) -> Expr -> t
foldExpr valf _ _ (Val n) = valf n
foldExpr valf addf subf (Add e1 e2) = addf (foldExpr valf addf subf e1) (foldExpr valf addf subf e2)
foldExpr valf addf subf (Sub e1 e2) = subf (foldExpr valf addf subf e1) (foldExpr valf addf subf e2)

-- evaluate an expression
eval :: Expr -> Int
eval = foldExpr id (+) (-)

-- calculates the size of the AST (in nodes) that represents the expression
size :: Expr -> Int
size = foldExpr (const 1) (\x y -> x + y + 1) (\x y -> x + y + 1)

-- exercise 4

data Prop = Const Bool | Var Char | Not Prop | And Prop Prop | Imply Prop Prop
data Form = Negative | Positive | Mixed | Either deriving (Eq, Ord, Show, Read)

getForm :: Prop -> Form
getForm (Const _) = Either
getForm (Var _) = Positive
getForm (Not p) = negateForm $ getForm p
getForm (And p q) = andForm (getForm p) (getForm q)
getForm (Imply p q) = andForm (negateForm $ getForm p) (getForm q)

negateForm :: Form -> Form
negateForm Negative = Positive
negateForm Positive = Negative
negateForm Either = Either
negateForm Mixed = Mixed

andForm :: Form -> Form -> Form
andForm Positive Positive = Positive
andForm Negative Negative = Negative
andForm f Either = f
andForm Either f = f
andForm _ _ = Mixed

-- exercise 5

-- fmap :: (b -> c) -> Pair a b -> Pair a c
data Pair a b = P (a, b)
instance Functor (Pair a) where
    fmap f (P (x,y)) = P (x, f y)

-- fmap :: (b -> c) -> Fun a b -> Fun a c
data Fun a b = F (a -> b)
instance Functor (Fun a) where
    fmap f (F g) = F (f.g)

-- where f :: a -> b , g :: b -> c and we need a function in the output of type a -> c
