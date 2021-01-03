import Parsing

data LamMacroExpr = LamDef [ (String , LamExpr) ] LamExpr deriving (Eq,Show,Read)
data LamExpr = LamMacro String | LamApp LamExpr LamExpr 
    | LamAbs Int LamExpr | LamVar Int deriving (Eq, Show, Read)

expr :: Parser LamExpr
expr =
    parseLamApp <|> parseLamAbs <|> parseLamVar <|> parseMacro

parseLamApp :: Parser LamExpr
parseLamApp = do
    e1 <- rmBrackets <|> parseLamVar <|> parseMacro
    space
    e2 <- rmBrackets <|> parseLamVar <|> parseMacro
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
        space
        string "in"
        space
        return (mn, e)

alreadyVisited :: String -> [String] -> Bool
alreadyVisited s [] = False 
alreadyVisited s (x:xs)
    | s == x = True 
    | otherwise = alreadyVisited s xs

hasDuplicates :: [(String, LamExpr)] -> [String] -> Bool
hasDuplicates [] _ = False 
hasDuplicates ((x, y):xs) visited
    | alreadyVisited x visited = True
    | otherwise = hasDuplicates xs (x : visited)

isClosed :: Int -> LamExpr -> Bool
isClosed a (LamAbs x e) = isClosed x e
isClosed a (LamApp e1 e2) = isClosed a e1 && isClosed a e2
isClosed a (LamVar x) = a == x

closed :: [(String, LamExpr)] -> Bool
closed [] = True 
closed ((x, y):macros)
    | isClosed (-1) y = closed macros
    | otherwise = False 

parseLamMacro :: String -> Maybe LamMacroExpr
parseLamMacro "" = Nothing
parseLamMacro s
    | null (parse expr e) = Nothing
    | rest == "" && not (hasDuplicates macros []) && closed macros = Just (LamDef macros ex)
    | otherwise = Nothing
    where
        (macros, e) = head (parse (many formPair) s)
        (ex, rest) = head (parse expr e)