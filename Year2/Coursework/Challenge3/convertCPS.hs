import System.Random ( Random(randomRIO) )

data LamMacroExpr = LamDef [ (String , LamExpr) ] LamExpr deriving (Eq,Show,Read)
data LamExpr = LamMacro String | LamApp LamExpr LamExpr 
    | LamAbs Int LamExpr | LamVar Int deriving (Eq, Show, Read)

visitedList :: LamExpr -> [Int]
visitedList (LamMacro x) = []
visitedList (LamVar x) = [x]
visitedList (LamAbs x e) = x : visitedList e
visitedList (LamApp e1 e2) = visitedList e1 ++ visitedList e2

visitedInMacros :: [(String, LamExpr)] -> [Int]
visitedInMacros [] = []
visitedInMacros ((x, y):macros) = visitedList y ++ visitedInMacros macros

alreadyVisited :: Int -> [Int] -> Bool
alreadyVisited s [] = False 
alreadyVisited s (x:xs)
    | s == x = True 
    | otherwise = alreadyVisited s xs

checkForVar :: Int -> [Int] -> Int
checkForVar a xs
    | alreadyVisited a xs = checkForVar (a + 1) xs
    | otherwise = a

transformExpr :: LamExpr -> [Int] -> Maybe LamExpr
-- var
transformExpr (LamVar x) visited =
    do
        let k = checkForVar 0 (x : visited)
        return (LamAbs k (LamApp (LamVar k) (LamVar x)))

-- macro
transformExpr (LamMacro x) visited =
    do
        return (LamMacro x)

-- abs
transformExpr (LamAbs x e) visited =
    do
        let k = checkForVar 0 (x : visited)
        body <- transformExpr e (k : x : visited)
        return (LamAbs k (LamApp (LamVar k) (LamAbs x body)))

-- app
transformExpr (LamApp e1 e2) visited = 
    do
        let k = checkForVar 0 visited
        let f = checkForVar 0 (k : visited)
        let e = checkForVar 0 (f : k : visited)
        func <- transformExpr e1 (k : f : e : visited)
        arg <- transformExpr e2 (k : f : e : (k + 1) : (f + 1) : (e + 1) : visited)
        return (LamAbs k (LamApp func (LamAbs f (LamApp arg (LamAbs e (LamApp (LamApp (LamVar f) (LamVar e)) (LamVar k)))))))

convertMacros :: [(String, LamExpr)] -> [(String, LamExpr)] -> [Int] -> [(String, LamExpr)]
convertMacros [] _ _ = []
convertMacros ((x, y):macros) acc visited = (x, converted) : convertMacros macros accMacros visitedM
    where
        Just converted = transformExpr y visited
        accMacros = (x, converted) : acc
        visitedM = visitedInMacros accMacros

cpsTransform :: LamMacroExpr -> LamMacroExpr
cpsTransform (LamDef macros e)
    | null macros = LamDef [] x
    | otherwise = LamDef convertedMacros y
    where
        Just x = transformExpr e visitedInExpression
        Just y = transformExpr e (visitedConverted ++ visitedInExpression)
        visitedInExpression = visitedList e
        visitedMacros = visitedInMacros macros
        visitedConverted = visitedInMacros convertedMacros
        convertedMacros = convertMacros macros macros (visitedMacros ++ visitedInExpression)

-- Examples in the instructions
exId = LamAbs 1 (LamVar 1)
ex5'1 = LamDef [] (LamApp (LamVar 1) (LamVar 2))
ex5'2 = LamDef [ ("F", exId) ] (LamVar 2)
ex5'3 = LamDef [ ("F", exId) ] (LamMacro "F")
ex5'4 = LamDef [ ("F", exId) ] (LamApp (LamMacro "F") (LamMacro "F"))