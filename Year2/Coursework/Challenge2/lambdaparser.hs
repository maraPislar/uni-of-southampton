import Parsing

data LamMacroExpr = LamDef [ (String , LamExpr) ] LamExpr deriving (Eq,Show,Read)
data LamExpr = LamMacro String | LamApp LamExpr LamExpr 
    | LamAbs Int LamExpr | LamVar Int deriving (Eq, Show, Read)

expr :: Parser LamExpr
expr =
    parseLamApp <|> parseLamAbs <|> parseLamVar <|> parseMacro

parseLamApp :: Parser LamExpr
parseLamApp = do
    e1 <- rmBrackets <|> parseLamVar
    space
    e2 <- rmBrackets <|> parseLamVar
    space
    ex <- many expr
    formatAppExpr e1 e2 ex

formatAppExpr :: LamExpr -> LamExpr -> [LamExpr] -> Parser LamExpr
formatAppExpr e1 e2 ex
    | null ex = return (LamApp e1 e2)
    | otherwise = return (LamApp (LamApp e1 e2) (head ex))

parseLamAbs :: Parser LamExpr
parseLamAbs = do
    symbol "\\"
    v <- var
    string " -> "
    LamAbs v <$> expr

parseLamVar :: Parser LamExpr
parseLamVar = do LamVar <$> var

parseMacro :: Parser LamExpr
parseMacro = do LamMacro <$> macroName

rmBrackets :: Parser LamExpr
rmBrackets = do
    space
    char '('
    e <- expr
    char ')'
    space
    return e

var :: Parser Int
var = 
    do
        char 'x'
        nat

macroName :: Parser String
macroName =
    do
        some upper 
        macroName
    <|>
        some upper

formPair :: Parser (String, LamExpr)
formPair =
    do
        string "def "
        mn <- macroName
        string " = "
        e <- expr
        string " in "
        return (mn, e)

parseLamMacro :: String -> LamMacroExpr
parseLamMacro s = LamDef macros ex
    where
        (macros, e) = head (parse (many formPair) s)
        (ex, _) = head (parse expr e)