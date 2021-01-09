import Parsing

data LamMacroExpr = LamDef [ (String , LamExpr) ] LamExpr deriving (Eq,Show,Read)
data LamExpr = LamMacro String | LamApp LamExpr LamExpr 
    | LamAbs Int LamExpr | LamVar Int deriving (Eq, Show, Read)

-- parser for any lambda expression
expr :: Parser LamExpr
expr =
    parseLamApp <|> parseLamAbs <|> parseLamVar <|> parseMacro

-- parser for lambda application
parseLamApp :: Parser LamExpr
parseLamApp = do
    e1 <- rmBrackets <|> parseLamVar <|> parseMacro
    space
    e2 <- rmBrackets <|> parseLamVar <|> parseMacro
    space
    ex <- many expr
    formatLamApp e1 e2 ex

-- format the lambda application depending on how many applications there are
formatLamApp :: LamExpr -> LamExpr -> [LamExpr] -> Parser LamExpr
formatLamApp e1 e2 ex
    | null ex = return (LamApp e1 e2)
    | otherwise = return (LamApp (LamApp e1 e2) (head ex))

-- parser for lambda abstractions
parseLamAbs :: Parser LamExpr
parseLamAbs = do
    symbol "\\"
    v <- var
    string " -> "
    LamAbs v <$> expr

-- parser for a lambda variable
parseLamVar :: Parser LamExpr
parseLamVar = do LamVar <$> var

-- parser for a lambda macro
parseMacro :: Parser LamExpr
parseMacro = do LamMacro <$> macroName

-- remove brackets to extract the expression
rmBrackets :: Parser LamExpr
rmBrackets = do
    space
    char '('
    e <- expr
    char ')'
    space
    return e

-- restrict variables to be of the form x1, x2, x23 etc
var :: Parser Int
var = 
    do
        char 'x'
        nat

-- restrict macro names to be of the form F, G, FG, etc
macroName :: Parser String
macroName =
    do
        some upper 
        macroName
    <|>
        some upper

-- form a pair between a lambda macro name and its corresponding expression
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

-- check is an element exista into a list
alreadyVisited :: Eq a => a -> [a] -> Bool
alreadyVisited s [] = False 
alreadyVisited s (x:xs)
    | s == x = True 
    | otherwise = alreadyVisited s xs

-- sanity check: check if there are repreted macros
hasDuplicates :: [(String, LamExpr)] -> [String] -> Bool
hasDuplicates [] _ = False 
hasDuplicates ((x, y):xs) visited
    | alreadyVisited x visited = True
    | otherwise = hasDuplicates xs (x : visited)

-- sanity check: check if a lambda expression is closed
isClosed :: Int -> LamExpr -> Bool
isClosed a (LamAbs x e) = isClosed x e
isClosed a (LamApp e1 e2) = isClosed a e1 && isClosed a e2
isClosed a (LamVar x) = a == x
isClosed a (LamMacro x) = True 

-- sanity check: check if all macro expressions are closed
closed :: [(String, LamExpr)] -> Bool
closed [] = True 
closed ((x, y):macros)
    | isClosed (-1) y = closed macros
    | otherwise = False 

-- parse a string to a Lambda Macro Expression
parseLamMacro :: String -> Maybe LamMacroExpr
-- empty string is not in the grammar
parseLamMacro "" = Nothing
parseLamMacro s
    -- if the parser returns an empty list => an error occured => Nothing
    | null (parse expr e) = Nothing
    -- sanity checks:
    -- if after parsing the last expression there is still a string to parse => Nothing
    -- is there are repeated macros => Nothing
    -- if there is a macro that is not closed => Nothing
    | rest == "" && not (hasDuplicates macros []) && closed macros = Just (LamDef macros ex)
    | otherwise = Nothing
    where
        -- parse the list of pairs of macros
        (macros, e) = head (parse (many formPair) s)
        -- get the expression restulted after parsing the macros
        (ex, rest) = head (parse expr e)