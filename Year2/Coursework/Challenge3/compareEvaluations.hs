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
-- oreductionCPS (LamApp e1 e2) = oreductionCPS e2 
oreductionCPS (LamAbs x (LamApp e1 e2)) = oreduction (LamApp (LamAbs x e1) e2)
oreductionCPS (LamAbs x e) = LamAbs x (oreductionCPS e)
oreductionCPS (LamVar x) = LamVar x

ireduction :: LamExpr -> LamExpr
ireduction (LamVar x) = LamVar x
ireduction (LamAbs x e) = LamAbs x (ireduction e)
ireduction (LamApp (LamAbs x e1) e@(LamAbs y e2)) = subst e1 x e
ireduction (LamApp e@(LamAbs x e1) e2) = LamApp e (ireduction e2)
ireduction (LamApp e1 e2) = LamApp (ireduction e1) e2

ireductionCPS :: LamExpr -> LamExpr
-- ireductionCPS (LamApp e1 e2) = 
ireductionCPS (LamAbs x (LamApp e1 e2)) = ireduction (LamApp e2 (LamAbs x e1))
ireductionCPS (LamAbs x e) = LamAbs x (ireductionCPS e)
ireductionCPS (LamVar x) = LamVar x

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
    
-- reductions for a macro expression in CPS
innerRedn2 :: LamMacroExpr -> Maybe LamMacroExpr
innerRedn2 (LamDef macros e)= Just (LamDef macros (ireductionCPS newExpr))
  where
    newExpr = prelucrateExpr e macros

outerRedn2 :: LamMacroExpr -> Maybe LamMacroExpr
outerRedn2 (LamDef macros e) = Just (LamDef macros (oreductionCPS newExpr))
  where
    newExpr = prelucrateExpr e macros

reduceExpressions :: (LamMacroExpr -> Maybe LamMacroExpr) -> LamMacroExpr -> Int
reduceExpressions reduce (LamDef macros e) 
   | currentReduction == previousReduction = 0
   | otherwise = 1 + reduceExpressions reduce (LamDef macros currentReduction)
   where 
         Just (LamDef m currentReduction) = reduce (LamDef macros e)
         previousReduction = e
         
compareInnerOuter :: LamMacroExpr -> Int -> ( Maybe Int, Maybe Int, Maybe Int, Maybe Int )
compareInnerOuter expr bound = (result1, result2, result3, result4)
   where inner = reduceExpressions innerRedn1 expr
         outer = reduceExpressions outerRedn1 expr
         innerCPS = reduceExpressions innerRedn2 expr
         outerCPS = reduceExpressions outerRedn2 expr
         result1 = if inner <= bound then Just inner else Nothing
         result2 = if outer <= bound then Just outer else Nothing
         result3 = if innerCPS <= bound then Just innerCPS else Nothing
         result4 = if outerCPS <= bound then Just outerCPS else Nothing