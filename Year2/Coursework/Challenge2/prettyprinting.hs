import Data.List (isInfixOf,  isPrefixOf )
import Data.Maybe ( isNothing )

data LamMacroExpr = LamDef [ (String , LamExpr) ] LamExpr deriving (Eq,Show,Read)
data LamExpr = LamMacro String | LamApp LamExpr LamExpr 
    | LamAbs Int LamExpr | LamVar Int deriving (Eq, Show, Read)

-- examples

-- def F = \x1 -> x1 in def G = \x1 -> (\x1 -> x1) x2 in \x1 -> G x3
expr1 :: LamMacroExpr
expr1 = LamDef [("F", LamAbs 1 (LamVar 1)), ("G", LamAbs 1 (LamApp (LamAbs 1 (LamVar 1)) (LamVar 2)))] 
    (LamAbs 1 (LamApp (LamAbs 1 (LamApp (LamAbs 1 (LamVar 1)) (LamVar 2))) (LamVar 3)))

-- (\x1 -> x1 x1) \x2 -> x2
expr2 :: LamMacroExpr
expr2 = LamDef [] (LamApp (LamAbs 1 (LamApp (LamVar 1) (LamVar 1))) (LamAbs 2 (LamVar 2)))

-- \x1 -> x1 x1
expr3 :: LamMacroExpr
expr3 = LamDef [] (LamAbs 1 (LamApp (LamVar 1) (LamVar 1)))

-- def G = λx -> λy -> x in def F = λx -> λy -> y in λx -> λy -> G (F y x) y
expr4 :: LamMacroExpr
expr4 = LamDef [("G", LamAbs 1 (LamAbs 2 (LamVar 1))), ("F", LamAbs 1 (LamAbs 2 (LamVar 2)))] 
    (LamAbs 1 (LamAbs 2 (LamApp (LamMacro "G") (LamApp (LamApp (LamMacro "F") (LamApp (LamVar 2) (LamVar 1))) (LamVar 2)))))

-- def G = λx -> λy -> x in def F = λx -> λy -> y in λx -> λy -> G (F y x) y
expr5 :: LamMacroExpr
expr5 = LamDef [("G", LamAbs 1 (LamAbs 2 (LamVar 1))), ("F", LamAbs 1 (LamAbs 2 (LamVar 2)))] 
    (LamAbs 1 (LamAbs 2 (LamApp (LamAbs 1 (LamAbs 2 (LamVar 1))) (LamApp (LamApp (LamAbs 1 (LamAbs 2 (LamVar 2))) (LamApp (LamVar 2) (LamVar 1))) (LamVar 2)))))

-- prettu print a lambda expression with macros
prettyPrint :: LamMacroExpr -> String 
prettyPrint (LamDef xs lam)
    -- if the list of macros is empty, simply evaluate the lambda expresion
    | null xs = printExpr lam
    -- if the list of macros is not empty, print all the formulas for 
    -- macros and and replace them in the formula accordingly
    | otherwise = printFormulas xs ++ replace (updateMacros xs xs) (printExpr lam)

-- pretty print any lambda expression
printExpr :: LamExpr -> String 
printExpr (LamVar n) = "x" ++ show n
printExpr (LamAbs n e) = "\\x" ++ show n ++ " -> " ++ printExpr e
printExpr (LamMacro s) = s
-- parenthesis must be put in such a way that they are not redundant
printExpr (LamApp (LamAbs a b) (LamAbs x (LamApp y z)) ) = "(" ++ printExpr (LamAbs a b) ++ ") " ++ "(" ++ printExpr (LamAbs x (LamApp y z)) ++ ")"
printExpr (LamApp (LamApp a b) (LamApp x y)) = "(" ++ printExpr (LamApp a b) ++ ")" ++ " (" ++ printExpr (LamApp x y) ++ ")"
printExpr (LamApp (LamApp a b) e) = "(" ++ printExpr (LamApp a b) ++ ") " ++ printExpr e
-- printExpr (LamApp e (LamApp x y)) = printExpr e ++ " (" ++ printExpr (LamApp x y) ++ ")"
printExpr (LamApp e1 (LamAbs x (LamApp y z))) = printExpr e1 ++ " " ++ "(" ++ printExpr (LamAbs x (LamApp y z)) ++ ")"
printExpr (LamApp (LamAbs x y) e2) = "(" ++ printExpr (LamAbs x y) ++ ") " ++ printExpr e2
printExpr (LamApp e1 e2) = printExpr e1 ++ " " ++ printExpr e2

-- prints all macros at the beginning of the pretty printed string
printFormulas :: [(String, LamExpr)] -> String
printFormulas [] = [] 
printFormulas ((s, lam):list) = "def " ++ s ++ " = " ++ printExpr lam ++ " in " ++ printFormulas list

-- update the list of macros with their corresponding lambda exprsion that don't appear in any other macro
-- from the initial list
updateMacros :: [(String, LamExpr)] -> [(String, LamExpr)] -> [(String, LamExpr)]
updateMacros [] _ = []
updateMacros ((m, lam):ms) macros
    | overlaps (m, lam) macros = updateMacros ms macros
    | otherwise = (m, lam) : updateMacros ms macros

-- helper function for the above function that checks if a given macro overlaps any other macro from the given list
overlaps :: (String, LamExpr) -> [ (String , LamExpr) ] -> Bool
overlaps (_,_) [] = False
overlaps (m1, lam1) ((m2, lam2):macros)
    | m1 /= m2 && printExpr lam1 `isInfixOf` printExpr lam2 = True
    | otherwise = overlaps (m1, lam1) macros

-- returns the position where a substring is found in another string
substringPosition :: String -> String -> Maybe Int
substringPosition _ []  = Nothing
substringPosition sub str =
    if sub `isPrefixOf` str
        then Just 0
    else
        (+1) <$> substringPosition sub (tail str)

-- replaces the macros in the lambda expresion
replace :: [(String, LamExpr)] -> String -> String
replace [] lam = lam
replace ((macro, sub):macros) lam
    | isNothing (substringPosition expr lam) = replace macros lam
    | otherwise = replace macros newLambda
    where
        Just start = substringPosition (printExpr sub) lam
        expr = printExpr sub
        newLambda = take (start - 1) lam ++ macro ++ drop (start + length expr + 1) lam