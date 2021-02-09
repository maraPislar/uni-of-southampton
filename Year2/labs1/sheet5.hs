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

-- exercise 6

data LTree a = LLeaf a | LNode (LTree a) (LTree a) deriving (Eq,Ord,Show,Read)
data Direction a = L (LTree a) | R (LTree a)
type Trail a = [Direction a]
type Zipper a = (LTree a, Trail a)

goLeft :: (LTree a, [Direction a]) -> (LTree a, [Direction a])
goLeft  (LNode l r, ts) = (l, L r : ts)

goRight :: (LTree a, [Direction a]) -> (LTree a, [Direction a])
goRight (LNode l r, ts) = (r, R l : ts)

goUp :: (LTree a, [Direction a]) -> (LTree a, [Direction a])
goUp    (t , L r : ts) = (LNode t r , ts)
goUp    (t , R l : ts) = (LNode l t , ts)

increment :: Zipper Int -> Zipper Int
increment (LLeaf x, ts) = (LLeaf (x+1), ts)

goLeftMost :: Zipper a -> Zipper a
goLeftMost (LLeaf a, ts) = (LLeaf a, ts)
goLeftMost (LNode l r, ts) = goLeftMost $ goLeft (LNode l r,ts)

goRightMost :: Zipper a -> Zipper a
goRightMost (LLeaf a, ts) = (LLeaf a, ts)
goRightMost (LNode l r, ts) = goRightMost $ goRight (LNode l r,ts)

goRoot :: Zipper a -> Zipper a
goRoot (t,[]) = (t,[])
goRoot z = goRoot (goUp z)

(-:) :: t1 -> (t1 -> t2) -> t2
x -: f = f x

-- increments by 1 the value of the 2nd leftmost and 2nd rightmost leaves in the tree
incr2LR :: LTree Int -> LTree Int
incr2LR (LLeaf a) = LLeaf a
incr2LR tree = 
 fst $ (tree,[]) -: goLeftMost -: goUp -: goRight -: goLeftMost -: increment -: goRoot 
                 -: goRightMost -: goUp -: goLeft -: goRightMost -: increment -: goRoot

-- exercise 7
-- you must import Data.Graph
-- if you uncomment this exercise and exercise 8, comment exercise 1
-- 
-- evenEdges :: [(Vertex, Vertex)]
-- evenEdges = [(n, n + 1) | n <- [0,2..998]]
-- 
-- oddEdges :: [(Vertex, Vertex)]
-- oddEdges = [(n, n `div` 5) | n <- [1, 3..999]]
-- 
-- graph :: Graph
-- graph = buildG (0, 1000) (evenEdges ++ oddEdges)
-- 
-- decides whether there is a path between two given nodes in the graph defined above
-- isReachable :: Int -> Int -> Bool
-- isReachable n m = m `elem` reachable graph n

-- exercise 8
-- -- graph with the same properties as in exercise 7
-- -- construct the graph but with cyclic dependecies

-- data GGraph a = GNode a (GGraph a) deriving Show

-- mkGraph :: [ (a, Int) ] -> [GGraph a]
-- mkGraph table = table'
--  where table' = map (\(x,n) -> GNode x (table'!!n)) table

-- table :: [(Vertex, Vertex)]
-- table = merge evenEdges oddEdges
--   where merge (x:xs) ys = x : merge ys xs
--         merge [] ys = ys

-- graphCD :: [GGraph Vertex]
-- graphCD = mkGraph table

-- nextNode :: GGraph a -> GGraph a
-- nextNode (GNode a g) = g

-- nodeID :: GGraph a -> a
-- nodeID (GNode v g) = v

-- isReachableCD :: Int -> Int -> Bool
-- isReachableCD n m = isReachableCD' n m []
--   where isReachableCD' n m visited | n == m = True
--         isReachableCD' n m visited | n `elem` visited = False 
--         isReachableCD' n m visited | otherwise = isReachableCD' (nodeID $ nextNode (graphCD!!n)) m (n:visited)
