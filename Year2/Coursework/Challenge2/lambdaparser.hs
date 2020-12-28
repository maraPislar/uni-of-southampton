import Parsing

-- Syntax:
-- 
-- MacroExpr ::= "def" MacroName "=" Expr "in" MacroExpr | Expr
-- Expr ::= Var | MacroName | Expr Expr | “\” Var “->” Expr | “(“ Expr “)”
-- MacroName ::= UChar | UChar MacroName
-- UChar ::= "A" | "B" | ... | "Z"
-- Var ::= “x” Digits
-- Digits ::= Digit | Digit Digits
-- Digit ::= “0” | “1” | “2” | “3” | “4” | “5” | “6” | “7” | “8” | “9”

data LamMacroExpr = LamDef [ (String , LamExpr) ] LamExpr deriving (Eq,Show,Read)
data LamExpr = LamMacro String | LamApp LamExpr LamExpr 
    | LamAbs Int LamExpr | LamVar Int deriving (Eq, Show, Read)

-- macroExpr :: Parser LamMacroExpr
-- macroExpr = 
--     do 
--         string "def"
--         macroName
--         string "="
--         expr
--         string "in"
--         macroExpr
--     <|> 
--         expr

expr :: Parser LamExpr
expr = 
    do
        LamVar <$> var
    <|>
        LamMacro <$> macroName
    <|>
        do 
            e1 <- expr
            LamApp e1 <$> expr
    <|>
        do
            string "\\"
            v <- var
            string "->"
            LamAbs v <$> expr
    <|>
        do
            char '('
            e <- expr
            char ')'
            return e

macroName :: Parser String
macroName =
    do
        some upper 
        macroName
    <|>
        some upper

var :: Parser Int
var = 
    do
        char 'x'
        nat

parseLamMacro :: String -> Maybe LamMacroExpr
parseLamMacro _ = Nothing 