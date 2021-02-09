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

ireduction :: LamExpr -> LamExpr
ireduction (LamVar x) = LamVar x
ireduction (LamAbs x e) = LamAbs x (ireduction e)
ireduction (LamApp (LamAbs x e1) e@(LamAbs y e2)) = subst e1 x e
ireduction (LamApp e@(LamAbs x e1) e2) = LamApp e (ireduction e2)
ireduction (LamApp e1 e2) = LamApp (ireduction e1) e2

-- get macro expression
getMacroExpr :: String -> [(String, LamExpr)] -> Maybe LamExpr
getMacroExpr _ [] = Nothing
getMacroExpr macro ((m,e):macros)
  | macro == m = Just e
  | otherwise = getMacroExpr macro macros

-- replace macro in final expression
prelucrateExpr :: LamExpr -> [(String, LamExpr)] -> LamExpr
prelucrateExpr (LamMacro x) macros = prelucrateExpr e macros
  where
    Just e = getMacroExpr x macros
prelucrateExpr (LamVar x) macros = LamVar x
prelucrateExpr (LamAbs x e) macros = LamAbs x (prelucrateExpr e macros)
prelucrateExpr (LamApp e1 e2) macros = LamApp (prelucrateExpr e1 macros) (prelucrateExpr e2 macros)

-- reductions for a simple macro expression
innerRedn1 :: LamMacroExpr -> Maybe LamMacroExpr
innerRedn1 (LamDef macros e)= Just (LamDef macros (ireduction newExpr))
  where
    newExpr = prelucrateExpr e macros

outerRedn1 :: LamMacroExpr -> Maybe LamMacroExpr
outerRedn1 (LamDef macros e) = Just (LamDef macros (oreduction newExpr))
  where
    newExpr = prelucrateExpr e macros

isApp :: LamExpr -> Bool
isApp (LamApp e1 e2) = True
isApp (LamAbs x e) = False
isApp (LamVar x) = False
isApp (LamMacro x) = False

reduceExpressions :: (LamMacroExpr -> Maybe LamMacroExpr) -> LamMacroExpr -> Int -> Int -> Maybe Int
reduceExpressions reduce (LamDef macros e) bound step
   | step > bound = Nothing
   | currentReduction == previousReduction && not (isApp currentReduction) = Just step
   | otherwise = reduceExpressions reduce (LamDef macros currentReduction) bound (step + 1)
   where 
         Just (LamDef m currentReduction) = reduce (LamDef macros e)
         previousReduction = e

compareInnerOuter :: LamMacroExpr -> Int -> ( Maybe Int, Maybe Int, Maybe Int, Maybe Int )
compareInnerOuter expr bound = (inner, outer, innerCPS, outerCPS)
   where inner = reduceExpressions innerRedn1 expr bound 0
         outer = reduceExpressions outerRedn1 expr bound 0
         innerCPS = reduceExpressions innerRedn1 expr bound 0 -- AICI TREBUIE APLICAT CPS INAINTE + IDENTITY
         outerCPS = reduceExpressions outerRedn1 expr bound 0 -- AICI TREBUIE APLICAT CPS INAINTE + IDENTITY

-- BUGS TO FIX:
-- - "Nothing" is not returned when the reduction enters a loop
-- - CPS evaluations don't work properly

ex1 = LamDef [] (LamAbs 1 (LamApp (LamVar 1) (LamVar 2)))
ex1CPSIdentity = LamDef [] (LamApp (LamAbs 3 (LamApp (LamVar 3) (LamAbs 1 (LamAbs 6 (LamApp (LamAbs 4 (LamApp (LamVar 4) (LamVar 1))) (LamAbs 7 (LamApp (LamAbs 5 (LamApp (LamVar 5) (LamVar 2))) (LamAbs 8 (LamApp (LamApp (LamVar 7) (LamVar 8)) (LamVar 6)))))))))) (LamAbs 10 (LamVar 10)))