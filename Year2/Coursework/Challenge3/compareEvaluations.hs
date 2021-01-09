data LamMacroExpr = LamDef [ (String , LamExpr) ] LamExpr deriving (Eq,Show,Read)
data LamExpr = LamMacro String | LamApp LamExpr LamExpr 
    | LamAbs Int LamExpr | LamVar Int deriving (Eq, Show, Read)

free :: Int -> LamExpr -> Bool
free x (LamVar y) = x == y
free x (LamAbs y e) 
    | x == y = False
    | x /= y = free x e
free x (LamApp e1 e2) = free x e1 || free x e2

rename :: Int -> LamExpr -> Int
rename x e 
    | free (x + 1) e = rename (x + 1) e
    | otherwise = x + 1

subst :: LamExpr -> Int -> LamExpr -> LamExpr
subst (LamVar x) y e 
    | x == y = e
    | x /= y = LamVar x
subst (LamAbs x e1) y e
    | x /= y && not (free x e) = LamAbs x (subst e1 y e)
    | x /= y && free x e = let x' = rename 0 e1 in
        subst (LamAbs x' (subst e1 x (LamVar x'))) y e
    | x == y = LamAbs x e1
subst (LamApp e1 e2) y e = LamApp (subst e1 y e) (subst e2 y e)

oreduction :: LamExpr -> LamExpr
oreduction (LamVar x) = LamVar x
oreduction (LamAbs x e) = LamAbs x (oreduction e)
oreduction (LamApp (LamAbs x e1) e2) = subst e1 x e2
oreduction (LamApp e1 e2) = LamApp (oreduction e1) e2

oreductionCPS :: LamExpr -> LamExpr
oreductionCPS (LamAbs x (LamApp e1 e2)) = oreduction (LamApp e2 (LamAbs x e1))
oreductionCPS (LamAbs x e) = LamAbs x (oreductionCPS e)
oreductionCPS (LamVar x) = LamVar x

ireduction :: LamExpr -> LamExpr
ireduction (LamVar x) = LamVar x
ireduction (LamAbs x e) = LamAbs x (ireduction e)
ireduction (LamApp (LamAbs x e1) e@(LamAbs y e2)) = subst e1 x e
ireduction (LamApp e@(LamAbs x e1) e2) = LamApp e (ireduction e2)
ireduction (LamApp e1 e2) = LamApp (ireduction e1) e2

ireductionCPS :: LamExpr -> LamExpr
ireductionCPS (LamAbs x (LamApp e1 e2)) = ireduction (LamApp e2 (LamAbs x e1))
ireductionCPS (LamAbs x e) = LamAbs x (ireductionCPS e)
ireductionCPS (LamVar x) = LamVar x

ex1 = LamAbs 1 (LamVar 1)
ex1CPS = LamAbs 0 (LamApp (LamVar 0) (LamAbs 1 (LamAbs 2 (LamApp (LamVar 2) (LamVar 1)))))
-- LamAbs 1 (LamAbs 2 (LamApp (LamVar 2) (LamVar 1)))
ex2 = LamApp (LamAbs 1 (LamVar 1)) (LamAbs 2 (LamVar 2))
ex3 = LamAbs 1 (LamApp (LamVar 1) (LamVar 2))


-- call by value = innermost reduction
innerRedn1 :: LamMacroExpr -> Maybe LamMacroExpr
innerRedn1 = Just

-- call by name = outermost reduction
outerRedn1 :: LamMacroExpr -> Maybe LamMacroExpr
outerRedn1 (LamDef macros e)
    | null macros = Just (LamDef [] e)

-- (\x -> x) (x')
-- LamApp (LamAbs x e1) e2 => subst (LamAbs x e1) y e
-- subst ()