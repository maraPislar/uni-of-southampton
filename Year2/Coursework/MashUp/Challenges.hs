{-# LANGUAGE DeriveGeneric #-}
-- ===================================================
-- (c) University of Southampton 2021 
-- Title :  comp2209 Functional Programming Challenges
-- Author:  Theodora-Mara Pislar
-- Date  :  14 Jan 2021
-- ===================================================

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
-- used to know the number of steps to make 
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
-- used to know the direction depending on the number of steps
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

-- return an element from a matrix at position (x, y)
takeXthYth :: [[a]] -> Int -> Int -> a
takeXthYth xs x y = ( xs !! x ) !! y

-- check if word exists at a given position on an ecrypted direction
checkWord :: String -> WordSearchGrid -> Posn -> Posn -> Posn -> Maybe Placement
checkWord [] _ (x, y) (_, _) (a, b) = Just ((y, x), decrypt (a, b))
checkWord word grid (x,y) (i, j) (a, b)
    -- check if position of element is within the board
    | not noBoard = Nothing
    -- check if the first char of the word is the same as the one on the board
    -- if yes => continue checking the rest of the chars of the word
    | takeXthYth grid i j == head word = checkWord (tail word) grid (x,y) (i + a, j + b) (a, b)
    -- if there is no match => the word wasn't found
    | otherwise = Nothing
    where
        noBoard = i >= 0 && j >= 0 && i < n && j < n
        n = length grid

-- find a direction on the grid where the word can continue
findDirection :: String -> WordSearchGrid -> Posn -> [Orientation] -> Maybe Placement
-- if there is no match with a direction => the word is not on that position
findDirection _ _ _ [] = Nothing
findDirection word grid (i, j) (d:dx)
    -- try each direction at a position in which the word can be checked
   | matchWord == Nothing = findDirection word grid (i, j) dx
   | otherwise = matchWord
   where
       matchWord = checkWord word grid (i, j) (i, j) (encrypt d)

-- find where an individual word can be by checking all positions on the board
solveForWord :: String -> WordSearchGrid -> Int -> Maybe Placement
solveForWord word grid counter
    | word == "" = Nothing
    -- if all positions are checked and none returned a Placement => 
        -- the owrd is not on the board
    | counter >= n * n = Nothing
    -- if no direction is found at that position, move to the next position
    | matchDirection == Nothing = solveForWord word grid (counter + 1)
    | otherwise = matchDirection
    where
        n = length grid
        -- (i, j) position
        i = counter `div` n
        j = counter `mod` n
        matchDirection = findDirection word grid (i, j) directions

-- sanity check: check if the grid is of type n x n
--               check if the grid is empty 
checkGrid :: Int -> [[Char]] -> Bool
checkGrid 0 _ = False
checkGrid _ [] = True
checkGrid len (line:grid)
    | len == length line = checkGrid len grid
    | otherwise = False

-- solve the board for each word in the given list
solveWordSearch :: [String] -> WordSearchGrid -> [(String, Maybe Placement)]
solveWordSearch words grid 
    -- if any sanity check fails => return an empty grid 
    | not (checkGrid (length grid) grid) = []
    -- else, solve the puzzle for each word and return its placement if found
    | otherwise = [(word, solveForWord word grid counter) | word <- words ]
    where
        counter = 0

-- Challenge 2 --

-- create a puzzle for the word search game
-- parameters: a list of words to be hidden into the puzzle
--             the density (to calculate the size of the board)
createWordSearch :: [ String ] -> Double -> IO WordSearchGrid
createWordSearch words density =
    do
        -- check if the density produces a smaller size for the board
        checkSize <- getMaximumLength words maxi
        if size < checkSize then
            do
                -- create a grid that only consists of dummy characters
                let grid = replicate checkSize (replicate (checkSize + 1) '-')
                -- add the words to be found randomly in the board
                newGrid <- foldM (flip addWord) grid words
                -- fill the rest of the board with random chars
                fillGrid words newGrid []
        else
            do
            -- create a grid that only consists of dummy characters
            let grid = replicate size (replicate size '-')
            -- add the words to be found randomly in the board
            newGrid <- foldM (flip addWord) grid words
            -- fill the rest of the board with random chars
            fillGrid words newGrid []
    where
        maxi = 0
        numberOfHiddenChars = foldr ((+) . length) 0 words
        size = round (sqrt (fromIntegral numberOfHiddenChars / density))

-- determine the biggest word from the list
getMaximumLength :: [String] -> Int -> IO Int
getMaximumLength [] maxi = return maxi
getMaximumLength (word:words) maxi
    | maxi < length word = getMaximumLength words (length word)
    | otherwise = getMaximumLength words maxi

-- add one word to the board at a random position and direction
addWord :: String -> WordSearchGrid -> IO WordSearchGrid
addWord word grid =
    do
        -- get a random position and direction
        ((i, j), dir) <- getRandomPlacement (length grid)
        -- sanity check: check if the word fits at that random direction and position
        if isSafe (length word) (length grid) (i, j) dir && canFit word grid (i, j) dir
            -- return the updateg board with that word written in it
            then return (updateGrid word grid (i, j) dir)
        else
            -- recursive call to find another random position and direction to check
            addWord word grid

-- sanity check: check if the length of a word fits into the board given a position and direction
isSafe :: Int -> Int -> Posn -> Orientation -> Bool
isSafe wordLen gridLen (i, j) Forward       = j + wordLen <= gridLen
isSafe wordLen gridLen (i, j) Back          = j - wordLen >= -1
isSafe wordLen gridLen (i, j) Up            = i - wordLen >= -1
isSafe wordLen gridLen (i, j) Down          = i + wordLen <= gridLen
isSafe wordLen gridLen (i, j) UpForward     = i - wordLen >= -1 && j + wordLen <= gridLen
isSafe wordLen gridLen (i, j) UpBack        = i - wordLen >= -1 && j - wordLen >= -1
isSafe wordLen gridLen (i, j) DownForward   = i + wordLen <= gridLen && j + wordLen <= gridLen
isSafe wordLen gridLen (i, j) DownBack      = i + wordLen <= gridLen && j - wordLen >= -1

-- sanity check: check for collisions of a word in the board with another word
canFit :: String -> [[Char]] -> Posn -> Orientation -> Bool
-- return true if the checking got to the end of the word without any 'False' calls
canFit [] _ _ _ = True
canFit (w:word) grid (i, j) dir
    -- accept only if the char on the board is the same as the char to add or if it's the dummy char
    | takeXthYth grid i j == w || takeXthYth grid i j == '-' = canFit word grid (i + x, j + y) dir
    | otherwise = False
    where
        -- the funciton encrypt was created in challenge 1
        (x, y) = encrypt dir

-- update the grid with a word that is safe to add and can fit into the board
updateGrid :: String -> [[Char]] -> Posn -> Orientation -> [[Char]]
updateGrid [] grid _ _ = grid
-- add each char to the grid
updateGrid (w:word) grid (i, j) dir = updateGrid word newGrid (i + x, j + y) dir
    where
        newGrid = writeOneChar w grid 0 (i, j)
        -- the function encrypt was created in challenge 1
        (x, y) = encrypt dir

-- add each char to the given pisition in the board
-- concatenate the rest of the rows
writeOneChar :: Char -> [[Char]] -> Int -> Posn -> [[Char]]
writeOneChar _ [] _ _ = []
writeOneChar w (row:grid) counter (i, j)
    | counter == i = insertAt w j row : writeOneChar w grid (counter + 1) (i, j)
    | otherwise = row : writeOneChar w grid (counter + 1) (i , j)

-- insert one element at a given position into a list
insertAt :: a -> Int -> [a] -> [a]
insertAt newElement _ [] = [newElement]
insertAt newElement i (a:as)
  | i <= 0 = newElement:as
  | otherwise = a : insertAt newElement (i - 1) as

-- generate a random placement 
getRandomPlacement :: Int -> IO Placement
getRandomPlacement len =
    do
        pos <- getRandomPosition len
        dir <- getRandomDirection
        return (pos, dir)

-- generate a random position within the board
getRandomPosition :: Int -> IO Posn
getRandomPosition len =
    do 
        ri <- randomRIO (0, len - 1)
        rj <- randomRIO (0, len - 1)

        return (ri, rj)

-- generate a random direction between the 8 directions available
getRandomDirection :: IO Orientation
getRandomDirection =
    do
        pos <- randomRIO (0, 7)
        return (directions !! pos)

-- fill the grid with randomly chosen chars from the set of the given words
fillGrid :: [String] -> WordSearchGrid -> WordSearchGrid -> IO WordSearchGrid
fillGrid _ [] acc = return acc
fillGrid words (row:grid) acc =
    do
        newRow <- fillRow row [] words
        fillGrid words grid (acc ++ [newRow])

-- fill the row with randomly chosen characters form the given set of words
fillRow :: [Char] -> [Char] -> [String] -> IO [Char]
fillRow [] acc _ = return acc
fillRow (w:row) acc words =
    do
        -- select a random char from the words
        ch <- getRandomChar words
        -- replace the char in the row if it is a dummy node
        if w == '-'
            -- replace the char with a random char
            then fillRow row (acc ++ [ch]) words
        else
            -- recursive call to check for the rest of the chars
            -- and to replace them 
            fillRow row (acc ++ [w]) words

-- generate a random character from the given set of words
getRandomChar :: [String] -> IO Char
getRandomChar words =
    do
        -- generate a random position within the length of the list
        randomPos <- randomRIO (0, length words - 1)
        -- select the word in the list based on the random position
        let word = words !! randomPos
        -- generate a random position within the length of the word selected
        randomCh <- randomRIO (0, length word - 1)
        -- return the character from the word at that random position
        return (word !! randomCh)

-- Challenge 3 --

-- pretty print a lambda expression with macros
prettyPrint :: LamMacroExpr -> String 
prettyPrint (LamDef xs lam)
    -- if the list of macros is empty, simply evaluate the lambda expresion
    | null xs = printExpr lam
    -- if the list of macros is not empty, print all the formulas for 
    -- macros and and replace them in the formula accordingly
    | otherwise = printFormulas xs ++ printExpr (updateExpr xs lam)

-- pretty print a single expression and respect the parenthesis
-- the resulted string will parse to the same syntax tree
printExpr :: LamExpr -> String
printExpr e
    | (LamAbs x e1) <- e = "\\x" ++ show x ++ " -> " ++ printExpr e1
    | (LamApp x y)  <- e = putParenthesis (LamApp x y)
    | (LamVar x)    <- e = "x" ++ show x
    | (LamMacro x)  <- e = x

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

-- update an expression by replacing lambda expressions from within with the
-- correspunding macro definition
-- if two macros overlap => the bigger one will be written in the expression
updateExpr :: [(String, LamExpr)] -> LamExpr -> LamExpr
updateExpr macros (LamVar x) = replace macros (LamVar x)
updateExpr macros (LamMacro x) = LamMacro x
updateExpr macros (LamAbs x e) 
    | isMacro macros (LamAbs x e) =  replace macros (LamAbs x e)
    | otherwise = LamAbs x (updateExpr macros e)
updateExpr macros (LamApp e1 e2) 
    | isMacro macros (LamApp e1 e2) = replace macros (LamApp e1 e2)
    | otherwise = LamApp (updateExpr macros e1) (updateExpr macros e2)

-- check if a given lambda expression is a definition of a macro
isMacro :: [(String, LamExpr)] -> LamExpr -> Bool
-- if the list of the macros finished without any True calls => 
-- there is no macro definition equal to that expression => False
isMacro [] _ = False
isMacro ((m, e):macros) lam
    | e == lam = True
    | otherwise = isMacro macros lam

-- replace a lambda expression with the matching macro name
replace :: [(String, LamExpr)] -> LamExpr -> LamExpr
-- if the list of macros finished and no LamMacro call was made =>
-- there is no macro matching and just return the pure lambda expression
replace [] lam = lam
replace ((m, e):macros) lam
    | e == lam = LamMacro m
    | otherwise = replace macros lam

-- Challenge 4 --

-- parser for any lambda expression
expr :: Parser LamExpr
expr =
    parseLamApp <|> parseLamAbs <|> parseLamVar <|> parseMacro

-- parser for lambda application
parseLamApp :: Parser LamExpr
parseLamApp = do
    -- remove brackets and get an expression OR parse a var OR parse a macro
    e1 <- rmBrackets <|> parseLamVar <|> parseMacro
    space
    e2 <- rmBrackets <|> parseLamVar <|> parseMacro
    space
    ex <- many expr
    formatLamApp e1 e2 ex

-- format the lambda application depending on how many applications there are
-- if there are many lambda application => create nested lambda applications
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

-- check if an element exists into a list
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
-- a is a starting point
checkForVar :: Int -> [Int] -> Int
checkForVar a xs
    | alreadyVisited a xs = checkForVar (a + 1) xs
    | otherwise = a

startPoint :: Int
startPoint = 0

-- convert a single expression into CPS
-- always add the ints used in defining expressions
-- to the visited list to keep track of the names used
-- apply the rules of CPS depending on the lambda expression
transformExpr :: LamExpr -> [Int] -> Maybe LamExpr

-- var
transformExpr (LamVar x) visited =
    do
        let k = checkForVar startPoint (x : visited)
        return (LamAbs k (LamApp (LamVar k) (LamVar x)))

-- macro
transformExpr (LamMacro x) visited =
    do
        return (LamMacro x)

-- abs
transformExpr (LamAbs x e) visited =
    do
        let k = checkForVar startPoint (x : visited)
        body <- transformExpr e (k : x : visited)
        return (LamAbs k (LamApp (LamVar k) (LamAbs x body)))

-- app
transformExpr (LamApp e1 e2) visited = 
    do
        let k = checkForVar startPoint visited
        let f = checkForVar startPoint (k : visited)
        let e = checkForVar startPoint (f : k : visited)
        func <- transformExpr e1 (k : f : e : visited)
        arg <- transformExpr e2 (k : f : e : (k + 1) : (f + 1) : (e + 1) : visited)
        return (LamAbs k (LamApp func (LamAbs f (LamApp arg (LamAbs e 
               (LamApp (LamApp (LamVar f) (LamVar e)) (LamVar k)))))))

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
-- key idea: keep the names of the variables added unique as
-- you update each expression
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

-- comapare the number of steps of different lambda reductions
compareInnerOuter :: LamMacroExpr -> Int -> ( Maybe Int, Maybe Int, Maybe Int, Maybe Int )
compareInnerOuter (LamDef [] expr) bound = (inner, outer, innerCPS, outerCPS)
    where
        -- transform a lambda macro expression in cps
        (LamDef [] cpsExpr) = cpsTransform (LamDef [] expr)
        cpsToCompare = LamDef [] (LamApp cpsExpr identity)
        -- perform the reductions of each type on the normal expression
        inner = reduce innerRedn1 (LamDef [] expr) bound step
        outer = reduce outerRedn1 (LamDef [] expr) bound step
        -- perform the reduction of each type on the cps expressions
        innerCPS = reduce innerRedn1 cpsToCompare bound step
        outerCPS = reduce outerRedn1 cpsToCompare bound step
        step = 0
compareInnerOuter (LamDef macros expr) bound = (inner, outer, innerCPS, outerCPS)
   where
        -- replace macros in expression
        newExpr = prelucrateExpr expr macros
        -- transform a lambda macro expression in cps
        (LamDef ms cpsExpr) = cpsTransform (LamDef macros newExpr)
        cpsToCompare = LamDef ms (LamApp cpsExpr identity)
        -- perform the reductions of each type on the normal expression
        inner = reduce innerRedn1 (LamDef macros (LamApp newExpr identity)) bound step
        outer = reduce outerRedn1 (LamDef macros (LamApp newExpr identity)) bound step
        -- perform the reduction of each type on the cps expressions
        innerCPS = reduce innerRedn1 cpsToCompare bound step
        outerCPS = reduce outerRedn1 cpsToCompare bound step
        step = 0

-- identity function to apply
identity :: LamExpr
identity = LamAbs 100 (LamVar 100)

-- reduce a lambda macro expression depending on the function passed to it
-- the bound represents the maximum number of reductions allowed
reduce :: (LamMacroExpr -> Maybe LamMacroExpr) -> LamMacroExpr -> Int -> Int -> Maybe Int
reduce f (LamDef macros e) bound step
   | step > bound = Nothing
   | not (hasRedex e) = Just step
   | otherwise = reduce f (LamDef macros currentReduction) bound (step + 1)
   where 
         Just (LamDef m currentReduction) = f (LamDef macros e)

-- check to see if a lambda expression has a reducible expression
hasRedex :: LamExpr -> Bool
hasRedex (LamApp (LamAbs x m) n) = True
hasRedex (LamApp m n) = (hasRedex m) || (hasRedex n)
hasRedex (LamAbs x m) = hasRedex m
hasRedex (LamVar x) = False
hasRedex (LamMacro x) = False

-- replace macro in final expression
prelucrateExpr :: LamExpr -> [(String, LamExpr)] -> LamExpr
prelucrateExpr (LamMacro x) macros = prelucrateExpr e macros
  where
    Just e = getMacroExpr x macros
prelucrateExpr (LamVar x) macros = LamVar x
prelucrateExpr (LamAbs x e) macros = LamAbs x (prelucrateExpr e macros)
prelucrateExpr (LamApp e1 e2) macros = LamApp (prelucrateExpr e1 macros) (prelucrateExpr e2 macros)

-- get macro expression
getMacroExpr :: String -> [(String, LamExpr)] -> Maybe LamExpr
getMacroExpr _ [] = Nothing
getMacroExpr macro ((m,e):macros)
  | macro == m = Just e
  | otherwise = getMacroExpr macro macros

-- one outer reduction for a simple macro expression
outerRedn1 :: LamMacroExpr -> Maybe LamMacroExpr
outerRedn1 (LamDef macros e) = Just (LamDef macros (oreduction e))

-- one inner reduction for a simple macro expression
innerRedn1 :: LamMacroExpr -> Maybe LamMacroExpr
innerRedn1 (LamDef macros e)= Just (LamDef macros (fst (ireduction e)))

-- perform one outer reduction on a lambda expression
oreduction :: LamExpr -> LamExpr
oreduction (LamVar x) = LamVar x
oreduction (LamAbs x e) = LamAbs x (oreduction e)
oreduction (LamApp (LamAbs x e1) e2) = substitute e1 x e2
oreduction (LamApp e1 e2) = LamApp (oreduction e1) e2

-- perform one inner reduction on a lambda expression
ireduction :: LamExpr -> (LamExpr, Bool)
ireduction (LamVar n) = (LamVar n, False)
ireduction (LamAbs n e) = (LamAbs n (fst (ireduction e)), False)
ireduction expr@(LamApp e1@(LamAbs n e) e2) 
  | snd result1 == False && snd result2 == False = (oreduction expr, False)
  where result1 = ireduction e1
        result2 = ireduction e2
ireduction expr@(LamApp e1 e2) 
  | snd result1 == False && snd result2 == False = (oreduction expr, False)
  where result1 = ireduction e1
        result2 = ireduction e2

-- substitutes expression three for (free) variable two in the expression given as parameter 1
substitute :: LamExpr -> Int ->  LamExpr -> LamExpr
substitute (LamVar n) m e | n == m = e
substitute (LamVar n) m e | n /= m = LamVar n
substitute (LamAbs n e1) m e  | n /= m && not (isFree n e)  = LamAbs n (substitute e1 m e)
substitute (LamAbs n e1) m e  | n /= m && isFree n e  = 
    substitute (LamAbs (n + 1) (substitute e1 n (LamVar (n + 1)))) m e
substitute (LamAbs n e1) m e  | n == m  = LamAbs n e1
substitute (LamApp e1 e2) m e = LamApp (substitute e1 m e) (substitute e2 m e) 

-- check to see whether a variable occurs free in the given lambda expression
isFree :: Int -> LamExpr -> Bool
isFree n (LamVar m) =  n == m
isFree n (LamAbs m e) | n == m = False
isFree n (LamAbs m e) | n /= m = isFree n e
isFree n (LamApp e1 e2) = (isFree n e1) || (isFree n e2)