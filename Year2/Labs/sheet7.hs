-- Example Lambda calculus interpreter

data Expr = Var String | Lam String Expr | App Expr Expr | CL String Expr Environment
  deriving (Eq, Show, Read)
type Environment = [(String, Expr)]

free :: String -> Expr -> Bool
free x (Var y) =  x == y
free x (Lam y e) | x == y = False
free x (Lam y e) | x /= y = free x e
free x (App e1 e2)  = (free x e1) || (free x e2)

rename :: [Char] -> Expr -> [Char]
rename x e | free (x ++"\'") e = rename (x++"\'") e
           | otherwise = (x++"\'")

subst :: Expr -> String ->  Expr -> Expr
subst (Var x) y e | x == y = e
subst (Var x) y e | x /= y = Var x
subst (Lam x e1) y e  |  x /= y && not (free x e)  = Lam x (subst e1 y e)
subst (Lam x e1) y e  |  x /= y &&     (free x e)  = let x' = (rename x e1) in subst (Lam x' (subst e1 x (Var x'))) y e
subst (Lam x e1) y e  | x == y  = Lam x e1
subst (App e1 e2) y e = App (subst e1 y e) (subst e2 y e) 

-- Performs a single step of call-by-name reduction
eval1cbn :: Expr -> Expr
eval1cbn (Lam x e) = (Lam x e)
eval1cbn (App (Lam x e1) e2) = subst e1 x e2
eval1cbn (App e1 e2) = App (eval1cbn e1) e2

-- Peforms multiple steps of call-by-name reduction until no change in term is observed
reductions :: (Expr -> Expr) -> Expr -> [ (Expr, Expr) ]
reductions ss e = [ p | p <- zip evals (tail evals) ]
   where evals = iterate ss e

eval :: (Expr -> Expr) -> Expr -> Expr
eval ss = fst . head . dropWhile (uncurry (/=)) . reductions ss

trace :: (Expr -> Expr) -> Expr -> [Expr]
trace ss  = (map fst) . takeWhile (uncurry (/=)) .  reductions ss

eval1cbv :: Expr -> Expr
eval1cbv (Lam x e) = (Lam x e)
eval1cbv (App (Lam x e1) e@(Lam y e2)) = subst e1 x e
eval1cbv (App e@(Lam x e1) e2) = App e (eval1cbv e2)
eval1cbv (App e1 e2) = App (eval1cbv e1) e2

evalcbn :: Expr -> Expr
evalcbn = eval eval1cbn
tracecbn :: Expr -> [Expr]
tracecbn = trace eval1cbn
evalcbv :: Expr -> Expr
evalcbv = eval eval1cbv
tracecbv :: Expr -> [Expr]
tracecbv = trace eval1cbv


myid :: Expr
myid = Lam "z" (Var "z")

-- This function builds a "pair" expression tree from two subtrees
pair :: Expr -> Expr -> Expr
pair e1 e2 = Lam "v" (App (App (Var "v") e1) e2)

-- These are lambda terms that select the first or second input
tru :: Expr
tru = Lam "x" (Lam "y" (Var "x") )
fls :: Expr
fls = Lam "x" (Lam "y" (Var "y") )

-- These build the fst and snd functions to extract from our pair encoding
myfst :: Expr -> Expr
myfst e =  (App e tru)
mysnd :: Expr -> Expr
mysnd e =  (App e fls)

term1 :: Expr
term1 = myfst (pair tru myid)
term2 :: Expr
term2 = mysnd (pair myid term1)

-- While True Loop

ww :: Expr
ww = Lam "x" (App (Var "x") (Var "x"))
omega :: Expr
omega = App ww ww

-- exercise one

lookup :: String -> Environment -> Maybe Expr
lookup key [] = Nothing
lookup key ((x:y):ps)
    | key == x = Just y
    | otherwise = lookup key ps

-- exercise two

