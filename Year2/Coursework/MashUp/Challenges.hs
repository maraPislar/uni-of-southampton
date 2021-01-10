{-# LANGUAGE DeriveGeneric #-}
-- comp2209 Functional Programming Challenges
-- (c) University of Southampton 2020
-- Skeleton code to be updated with your solutions
-- The dummy functions here simply return an arbitrary value that is usually wrong 

-- DO NOT MODIFY THE FOLLOWING LINES OF CODE
module Challenges (WordSearchGrid,Placement,Posn,Orientation(..),solveWordSearch, createWordSearch,
    LamMacroExpr(..),LamExpr(..),prettyPrint, parseLamMacro,
    cpsTransform,innerRedn1,outerRedn1,compareInnerOuter) where

-- Import standard library and parsing definitions from Hutton 2016, Chapter 13
-- We import System.Random - make sure that your installation has it installed - use stack ghci and stack ghc
import Data.Char
import Parsing
import Control.Monad
import Data.List
import GHC.Generics (Generic,Generic1)
import Control.DeepSeq
import System.IO
import System.Random
import Data.Maybe (isNothing)

instance NFData Orientation
instance NFData LamMacroExpr
instance NFData LamExpr

-- types for Part I
type WordSearchGrid = [[ Char ]]
type Placement = (Posn,Orientation)
type Posn = (Int,Int)
data Orientation = Forward | Back | Up | Down | UpForward | UpBack | DownForward | DownBack deriving (Eq,Ord,Show,Read,Generic)

-- types for Parts II and III
data LamMacroExpr = LamDef [ (String,LamExpr) ] LamExpr deriving (Eq,Show,Read,Generic)
data LamExpr = LamMacro String | LamApp LamExpr LamExpr  |
               LamAbs Int LamExpr  | LamVar Int deriving (Eq,Show,Read,Generic)

-- END OF CODE YOU MUST NOT MODIFY



-- ADD YOUR OWN CODE HERE

-- Challenge 1 --

-- directions saved into a list
directions :: [Orientation]
directions = [Forward, Back, Up, Down, UpForward, UpBack, DownForward, DownBack]

-- encrypt the direction in number of steps
encrypt :: Orientation -> Posn
encrypt o | o == Forward = (0,1)
          | o == Back = (0,-1)
          | o == Up = (-1,0)
          | o == Down = (1,0)
          | o == UpForward = (-1,1)
          | o == UpBack = (-1,-1)
          | o == DownForward = (1,1)
          | o == DownBack =(1,-1)  

-- decrypt number of steps in direction
decrypt :: Posn -> Orientation
decrypt (i,j)
   | i == 0 && j == 1 = Forward
   | i == 0 && j == -1 = Back
   | i == -1 && j == 0 = Up
   | i == 1 && j == 0 = Down
   | i == -1 && j == 1 = UpForward
   | i == -1 && j == -1 = UpBack
   | i == 1 && j == 1 = DownForward
   | i == 1 && j == -1 = DownBack

-- return a (i, j) element from a matrix
takeXthYth :: [[a]] -> Int -> Int -> a
takeXthYth xs x y = ( xs !! x ) !! y

-- check if position of element is within the board
isGood :: Posn -> Int -> Bool
isGood (i, j) n = i >= 0 && j >= 0 && i < n && j < n

-- check if word exists at a given position on an ecrypted direction
checkWord :: String -> WordSearchGrid -> Posn -> Posn -> Posn -> Maybe Placement
checkWord [] _ (x, y) (_, _) (a, b) = Just ((y, x), decrypt (a, b))
checkWord word grid (x,y) (i, j) (a, b)
    | isGood (i, j) (length grid) == False = Nothing
    | takeXthYth grid i j == head word = checkWord (tail word) grid (x,y) (i + a, j + b) (a, b)
    | otherwise = Nothing

-- find a direction on the grid where the word can continue
findDirection :: String -> WordSearchGrid -> Posn -> [Orientation] -> Maybe Placement
findDirection _ _ _ [] = Nothing
findDirection word grid (i, j) (d:dx)
   | checkWord word grid (i, j) (i, j) (encrypt d) == Nothing = findDirection word grid (i, j) dx
   | otherwise = checkWord word grid (i, j) (i, j) (encrypt d)

-- find where an individual word can be by checking all positions on the board
solveForWord :: String -> WordSearchGrid -> Int -> Maybe Placement
solveForWord word grid counter
    | word == "" = Nothing
    | counter >= n * n = Nothing
    | findDirection word grid (i, j) directions == Nothing = solveForWord word grid (counter + 1)
    | otherwise = findDirection word grid (i, j) directions
    where
        n = length grid
        i = counter `div` n
        j = counter `mod` n

-- sanity check: check if the grid is of type n x n
--               check if the grid is empty 
checkGrid :: Int -> [[Char]] -> Bool
checkGrid 0 _ = False
checkGrid _ [] = True
checkGrid len (line:grid)
    | len == length line = checkGrid len grid
    | otherwise = False

-- solve the board for each word on the board
solveWordSearch :: [String] -> WordSearchGrid -> [(String, Maybe Placement)]
solveWordSearch words grid 
    | not (checkGrid (length grid) grid) = []
    | otherwise = [(word, solveForWord word grid 0) | word <- words ]

-- Challenge 2 --

addWord :: String -> WordSearchGrid -> IO WordSearchGrid
addWord word grid =
    do
        ((i, j), dir) <- getRandomPlacement (length grid)
        if isSafe (length word) (length grid) (i, j) dir && canFit word grid (i, j) dir
            then return (updateGrid word grid (i, j) dir)
        else
            addWord word grid

isSafe :: Int -> Int -> Posn -> Orientation -> Bool
isSafe wordLen gridLen (i, j) Forward       = j + wordLen <= gridLen
isSafe wordLen gridLen (i, j) Back          = j - wordLen >= -1
isSafe wordLen gridLen (i, j) Up            = i - wordLen >= -1
isSafe wordLen gridLen (i, j) Down          = i + wordLen <= gridLen
isSafe wordLen gridLen (i, j) UpForward     = i - wordLen >= -1 && j + wordLen <= gridLen
isSafe wordLen gridLen (i, j) UpBack        = i - wordLen >= -1 && j - wordLen >= -1
isSafe wordLen gridLen (i, j) DownForward   = i + wordLen <= gridLen && j + wordLen <= gridLen
isSafe wordLen gridLen (i, j) DownBack      = i + wordLen <= gridLen && j - wordLen >= -1

canFit :: String -> [[Char]] -> Posn -> Orientation -> Bool
canFit [] _ _ _ = True
canFit (w:word) grid (i, j) dir
    | takeXthYth grid i j == w || takeXthYth grid i j == '-' = canFit word grid (i + x, j + y) dir
    | otherwise = False
    where
        (x, y) = encrypt dir

updateGrid :: String -> [[Char]] -> Posn -> Orientation -> [[Char]]
updateGrid [] grid _ _ = grid
updateGrid (w:word) grid (i, j) dir = updateGrid word newGrid (i + x, j + y) dir
    where
        newGrid = writeOneChar w grid 0 (i, j)
        (x, y) = encrypt dir

writeOneChar :: Char -> [[Char]] -> Int -> Posn -> [[Char]]
writeOneChar _ [] _ _ = []
writeOneChar w (row:grid) counter (i, j)
    | counter == i = insertAt w j row : writeOneChar w grid (counter + 1) (i, j)
    | otherwise = row : writeOneChar w grid (counter + 1) (i , j)

insertAt :: a -> Int -> [a] -> [a]
insertAt newElement _ [] = [newElement]
insertAt newElement i (a:as)
  | i <= 0 = newElement:as
  | otherwise = a : insertAt newElement (i - 1) as

getRandomChar :: [String] -> IO Char
getRandomChar words =
    do
        randomPos <- randomRIO (0, length words - 1)
        let word = words !! randomPos
        randomCh <- randomRIO (0, length word - 1)
        return (word !! randomCh)

getRandomPosition :: Int -> IO Posn
getRandomPosition len =
    do 
        ri <- randomRIO (0, len - 1)
        rj <- randomRIO (0, len - 1)

        return (ri, rj)

getRandomDirection :: IO Orientation
getRandomDirection =
    do
        pos <- randomRIO (0, 7)
        return (directions !! pos)

getRandomPlacement :: Int -> IO Placement
getRandomPlacement len =
    do
        pos <- getRandomPosition len
        dir <- getRandomDirection
        return (pos, dir)

fillGrid :: [String] -> WordSearchGrid -> WordSearchGrid -> IO WordSearchGrid
fillGrid _ [] acc = return acc
fillGrid words (row:grid) acc =
    do
        newRow <- fillRow row [] words
        fillGrid words grid (acc ++ [newRow])

fillRow :: [Char] -> [Char] -> [String] -> IO [Char]
fillRow [] acc _ = return acc
fillRow (w:row) acc words =
    do
        ch <- getRandomChar words
        if w == '-'
            then fillRow row (acc ++ [ch]) words
        else
            fillRow row (acc ++ [w]) words

createWordSearch :: [ String ] -> Double -> IO WordSearchGrid
createWordSearch words density =
    do
        let grid = replicate size (replicate size '-')
        newGrid <- foldM (flip addWord) grid words
        fillGrid words newGrid []
    where
        numberOfHiddenChars = foldr ((+) . length) 0 words
        size = round (sqrt (fromIntegral numberOfHiddenChars / density))


--- Convenience functions supplied for testing purposes
createAndSolve :: [ String ] -> Double -> IO [ (String, Maybe Placement) ]
createAndSolve words maxDensity =   do g <- createWordSearch words maxDensity
                                       let soln = solveWordSearch words g
                                       printGrid g
                                       return soln

printGrid :: WordSearchGrid -> IO ()
printGrid [] = return ()
printGrid (w:ws) = do putStrLn w
                      printGrid ws

-- Challenge 3 --

-- pretty print a lambda expression with macros
prettyPrint :: LamMacroExpr -> String 
prettyPrint (LamDef xs lam)
    -- if the list of macros is empty, simply evaluate the lambda expresion
    | null xs = printExpr lam
    -- if the list of macros is not empty, print all the formulas for 
    -- macros and and replace them in the formula accordingly
    | otherwise = printFormulas xs ++ replace (updateMacros xs xs) (printExpr lam)

-- pretty print a single expression and respect the parenthesis
-- the resulted string will parse to the same syntax tree
printExpr :: LamExpr -> String
printExpr e
    | (LamAbs x e1) <- e = "\\x" ++ show x ++ " -> " ++ printExpr e1
    | (LamApp x y) <- e     = putParenthesis (LamApp x y)
    | (LamVar x) <- e       = "x" ++ show x
    | (LamMacro x) <- e     = x

-- create a pretty print by adding parenthesis when needed so the expresion will parse to the same tree
putParenthesis :: LamExpr -> String
putParenthesis e
    | (LamApp e1@(LamApp _ _) e2@(LamApp _ _)) <- e = putParenthesis e1 ++ " (" ++ putParenthesis e2 ++ ")"
    | (LamApp e1@(LamVar _) e2@(LamApp _ _))   <- e = putParenthesis e1 ++ " (" ++ putParenthesis e2 ++ ")"
    | (LamApp e1@(LamMacro _) e2@(LamApp _ _)) <- e = putParenthesis e1 ++ " (" ++ putParenthesis e2 ++ ")"
    | (LamApp e1@(LamApp _ _) e2@(LamAbs _ _)) <- e = putParenthesis e1 ++ " (" ++ printExpr e2 ++ ")"
    | (LamApp e1@(LamVar _) e2@(LamAbs _ _))   <- e = putParenthesis e1 ++ " (" ++ printExpr e2 ++ ")"
    | (LamApp e1@(LamMacro _) e2@(LamAbs _ _)) <- e = putParenthesis e1 ++ " (" ++ printExpr e2 ++ ")"
    | (LamApp e1@(LamAbs _ _) e2@(LamApp _ _)) <- e = "(" ++ printExpr e1 ++ ") (" ++ putParenthesis e2 ++ ")"
    | (LamApp e1@(LamAbs _ _) e2@(LamAbs _ _)) <- e = "(" ++ printExpr e1 ++ ") (" ++ printExpr e2 ++ ")"
    | (LamApp e1@(LamApp _ _) e2@(LamVar _))   <- e = putParenthesis e1 ++ " " ++ putParenthesis e2
    | (LamApp e1@(LamApp _ _) e2@(LamMacro _)) <- e = putParenthesis e1 ++ " " ++ putParenthesis e2
    | (LamApp e1@(LamVar _) e2@(LamMacro _))   <- e = putParenthesis e1 ++ " " ++ putParenthesis e2
    | (LamApp e1@(LamMacro _) e2@(LamVar _))   <- e = putParenthesis e1 ++ " " ++ putParenthesis e2
    | (LamApp e1@(LamVar _) e2@(LamVar _))     <- e = putParenthesis e1 ++ " " ++ putParenthesis e2
    | (LamApp e1@(LamMacro _) e2@(LamMacro _)) <- e = putParenthesis e1 ++ " " ++ putParenthesis e2
    | (LamApp e1@(LamAbs _ _) e2@(LamVar _))   <- e = "(" ++ printExpr e1 ++ ") " ++ putParenthesis e2
    | (LamApp e1@(LamAbs _ _) e2@(LamMacro _)) <- e = "(" ++ printExpr e1 ++ ") " ++ putParenthesis e2
    | (LamVar x)                               <- e = "x" ++ show x
    | (LamMacro x)                             <- e = x

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

-- Challenge 4 --

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

-- Challenge 5

-- get a list of all the ints that have been used into an expression
visitedList :: LamExpr -> [Int]
visitedList (LamMacro x) = []
visitedList (LamVar x) = [x]
visitedList (LamAbs x e) = x : visitedList e
visitedList (LamApp e1 e2) = visitedList e1 ++ visitedList e2

-- get a list of all the ints that have been used in the macro definitions
visitedInMacros :: [(String, LamExpr)] -> [Int]
visitedInMacros [] = []
visitedInMacros ((x, y):macros) = visitedList y ++ visitedInMacros macros

-- find an int that hasn't yet been used in the expressions
checkForVar :: Int -> [Int] -> Int
checkForVar a xs
    | alreadyVisited a xs = checkForVar (a + 1) xs
    | otherwise = a

-- convert a single expression into CPS
-- always add the ints used to the visited list to keep track of the names used
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

-- convert the macros into CPS
-- keep track of the ints that have been used as names before
convertMacros :: [(String, LamExpr)] -> [(String, LamExpr)] -> [Int] -> [(String, LamExpr)]
convertMacros [] _ _ = []
convertMacros ((x, y):macros) acc visited = (x, converted) : convertMacros macros accMacros visitedM
    where
        Just converted = transformExpr y visited
        accMacros = (x, converted) : acc
        visitedM = visitedInMacros accMacros

-- transform a Lambda Macro Expression using CPS
-- update the visited list as you make the converssions
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

-- Challenge 6

innerRedn1 :: LamMacroExpr -> Maybe LamMacroExpr
innerRedn1 _ = Nothing

outerRedn1 :: LamMacroExpr -> Maybe LamMacroExpr
outerRedn1 _ = Nothing

compareInnerOuter :: LamMacroExpr -> Int -> (Maybe Int,Maybe Int,Maybe Int,Maybe Int)
compareInnerOuter _ _ = (Nothing,Nothing,Nothing,Nothing) 

-- Examples in the instructions

exId :: LamExpr
exId =  LamAbs 1 (LamVar 1)

-- (\x1 -> x1 x2)
ex6'1 :: LamMacroExpr
ex6'1 = LamDef [] (LamAbs 1 (LamApp (LamVar 1) (LamVar 2)))

--  def F = \x1 -> x1 in F  
ex6'2 :: LamMacroExpr
ex6'2 = LamDef [ ("F",exId) ] (LamMacro "F")

--  (\x1 -> x1) (\x2 -> x2)   
ex6'3 :: LamMacroExpr
ex6'3 = LamDef [] ( LamApp exId (LamAbs 2 (LamVar 2)))

--  (\x1 -> x1 x1)(\x1 -> x1 x1)  
wExp :: LamExpr
wExp = (LamAbs 1 (LamApp (LamVar 1) (LamVar 1)))
ex6'4 :: LamMacroExpr
ex6'4 = LamDef [] (LamApp wExp wExp)

--  def ID = \x1 -> x1 in def FST = (\x1 -> λx2 -> x1) in FST x3 (ID x4) 
ex6'5 :: LamMacroExpr
ex6'5 = LamDef [ ("ID",exId) , ("FST",LamAbs 1 (LamAbs 2 (LamVar 1))) ] ( LamApp (LamApp (LamMacro "FST") (LamVar 3)) (LamApp (LamMacro "ID") (LamVar 4)))

--  def FST = (\x1 -> λx2 -> x1) in FST x3 ((\x1 ->x1) x4))   
ex6'6 :: LamMacroExpr
ex6'6 = LamDef [ ("FST", LamAbs 1 (LamAbs 2 (LamVar 1)) ) ]  ( LamApp (LamApp (LamMacro "FST") (LamVar 3)) (LamApp (exId) (LamVar 4)))

-- def ID = \x1 -> x1 in def SND = (\x1 -> λx2 -> x2) in SND ((\x1 -> x1 x1) (\x1 -> x1 x1)) ID
ex6'7 :: LamMacroExpr
ex6'7 = LamDef [ ("ID",exId) , ("SND",LamAbs 1 (LamAbs 2 (LamVar 2))) ]  (LamApp (LamApp (LamMacro "SND") (LamApp wExp wExp) ) (LamMacro "ID") ) 
