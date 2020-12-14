-- exercise one

[] ++ ys = ys
(x:xs) ++ ys = x : (xs ++ ys)

-- use equational reasoning to show that
-- (1) xs ++ [] = xs
-- (2) xs ++ (ys ++ zs) = (xs ++ ys) ++ zs

-- (1)

Reason by induction on List x
Base case, xs is []
[] ++ []
= [] by definition ->

Inductive case, suppose xs ++ [] = xs
(x:xs) ++ []
    = x : (xs ++ []) by definition ->
    = x : xs by inductive hypothesis

-- (2)

Reason by induction on List x
Base case, xs is []
[] ++ (ys ++ zs)
    = ys ++ zs ->
    = ([] ++ ys) ++ zs by definition <-

Inductive case, suppose xs ++ (ys ++ zs) = (xs ++ ys) ++ zs
(x:xs) ++ (ys ++ zs)
    = x : (xs ++ (ys ++ zs)) by definition ->
    = x : ((xs ++ ys) ++ zs) by inductive hypothesis
    = (x : (xs ++ ys)) ++ zs by definition <-
    = ((x:xs) ++ ys) ++ zs by definition <-

-- exercise two

data Nat = Zero | Succ Nat

replicate :: Nat -> a -> [a]
replicate Zero _ = []
replicate (Succ n) x = x : replicate n x

all :: (a -> Bool) -> [a] -> Bool
all p [] = True
all p (x:xs) = p x && all p xs

-- prove that all (== x) (replicate n x) = True for any x and any n
-- What assumptions do you need to make about == in order for the proof to hold?
-- Are these valid assumptions?

Reason by induction on Nat n
Base case, n is Zero
all (== x) (replicate Zero x)
    = all (== x) [] by definition of replicate ->
    = True by definition of all ->

Inductive case, suppose all (== x) (replicate n x) = True
all (== x) (replicate (Succ n) x)
    = all (== x) (x : replicate n x) by definition ->
    = (== x) x && all (== x) (replicate n x) by definition ->
    = (== x) x && True by the inductive hypothesis
    = x == x rewriting as infix
    = True by assuming on ==, yes, this is reasonable due to Eq

-- exercise three

take Zero _  = []						 
take _ [] = []							  
take (Succ n) (x:xs) = x : take n xs

drop Zero xs = xs
drop _ [] = []
drop (Succ n) (_:xs) = drop n (x:xs)

-- prove that for any list we have take n xs ++ drop n xs = xs

Reason by induction on Nat n
Base case, n is Zero
take Zero xs ++ drop Zero xs
    = [] ++ drop Zero xs by definition of take ->
    = [] ++ xs by definition of drop ->
    = xs by exercise one

Inductive case, suppose take n xs ++ drop n xs = xs
Suppose list is empty
take (Succ n) [] ++ drop (Succ n) []
    = [] ++ drop (Succ n) [] by definition of take ->
    = [] ++ [] by definition of drop ->
    = [] by exercise one

Suppose list is not empty
take (Succ n) xs ++ drop (Succ n) xs
    = (x : (take n xs)) ++ drop (Succ n) (x:xs) by definition of take ->
    = (x : (take n xs)) ++ drop n xs by definition of drop ->
    = x : (take n xs ++ drop n xs) by definition o ++ ->
    = x : xs by inductive hypothesis

-- exercise four

even :: Nat -> Bool
even x = x `mod` 2 == 0

double :: Nat -> Nat
double x = x + x

-- prove that even (double n) = True for any n

Reason by induction on Nat n
Base case, n is Zero
even(double Zero)
    = even (Zero + Zero) 
    = even (add Zero Zero) -- demonstrated in lecture
    = even Zero
    = True

Inductive case, suppose even (double n) = True
even (double (Succ n))
    = even (Succ (Succ (double n))) by definition of add 
    = odd (Succ (double n)) by definition of even
    = even (double n) by definition of odd
    = True by inductive hypothesis

-- exercise five

data Tree a = Leaf a | Node (Tree a) (Tree a)
leaves :: Tree a -> Int
leaves (Leaf a) = 1
leaves (Node l r) = leaves l + leaves r

nodes :: Tree a -> Int
nodes (Leaf a) = 0
nodes (Node l r) = (nodes l) + (nodes r) + 1

-- prove that in any such tree the number of leaves 
-- is always one greater then the number of nodes

-- prove that leaves t = nodes t + 1 for any tree t

Reason by induction over Tree t
Base case, t is a Leaf node
leaves Leaf
    = 1
    = 0 + 1
    = nodes (Leaf a) + 1

Inductive case, suppose leaves l = nodes l + 1
                suppose leaves r = nodes r + 1
leaves (Node l r)
    = leaves l + leaves r by definition of leaves ->
    = nodes l + 1 + leaves r by inductive hypothesis
    = nodes l + 1 + nodes r + 1 by inductive hypothesis
    = ((nodes l) + (nodes r) + 1) + 1 by arithmetic of +
    = (nodes (Node l r)) + 1 by definition of nodes

-- Sanity check: for a tree of height k, there are 2^k leaf nodes 
-- and (1 + 2 + 4 + ... + 2^(k-1)) = 2^k - 1