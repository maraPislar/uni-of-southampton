import Data.List ( isPrefixOf )

data LamMacroExpr = LamDef [ (String , LamExpr) ] LamExpr deriving (Eq,Show,Read)
data LamExpr = LamMacro String | LamApp LamExpr LamExpr 
    | LamAbs Int LamExpr | LamVar Int deriving (Eq, Show, Read)

prettyPrint :: LamMacroExpr -> String 
prettyPrint (LamDef xs lam)
    | null xs = printExpr lam
    | otherwise = printFormula xs ++ printExpr lam

printExpr :: LamExpr -> String 
printExpr (LamVar n) = "x" ++ show n
printExpr (LamAbs n e) = "(" ++ "\\x" ++ show n ++ " -> " ++ printExpr e ++ ")"
printExpr (LamMacro s) = s 
printExpr (LamApp e1 e2) = printExpr e1 ++ " " ++ printExpr e2

printFormula :: [(String, LamExpr)] -> String
printFormula [] = [] 
printFormula ((s, lam):list) = "def " ++ s ++ " = " ++ printExpr lam ++ " in " ++ printFormula list

substringP :: String -> String -> Maybe Int
substringP _ []  = Nothing
substringP sub str = case isPrefixOf sub str of
  False -> fmap (+1) $ substringP sub (tail str)
  True  -> Just 0