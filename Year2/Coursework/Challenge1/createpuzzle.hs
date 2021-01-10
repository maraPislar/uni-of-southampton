import Data.List ()
import System.Random ( Random(randomRIO) )
import Control.Monad (guard)
import Control.Monad.List

type WordSearchGrid = [ [ Char ] ]
type Placement = (Posn,Orientation)
type Posn = (Int,Int)
data Orientation = Forward | Back | Up | Down | UpForward | UpBack | DownForward | DownBack deriving (Eq,Ord,Show,Read)

directions :: [Orientation]
directions = [Forward, Back, Up, Down, UpForward, UpBack, DownForward, DownBack]

takeXthYth :: [[Char]] -> Int -> Int -> Char
takeXthYth xs x y = ( xs !! x ) !! y

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

-- Challenge 2: create a puzzle

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

isSafe :: Int -> Int -> Posn -> Orientation -> Bool
isSafe wordLen gridLen (i, j) Forward       = j + wordLen <= gridLen
isSafe wordLen gridLen (i, j) Back          = j - wordLen >= -1
isSafe wordLen gridLen (i, j) Up            = i - wordLen >= -1
isSafe wordLen gridLen (i, j) Down          = i + wordLen <= gridLen
isSafe wordLen gridLen (i, j) UpForward     = i - wordLen >= -1 && j + wordLen <= gridLen
isSafe wordLen gridLen (i, j) UpBack        = i - wordLen >= -1 && i - wordLen >= -1
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
        newGrid = writeInGrid w grid 0 (i, j)
        (x, y) = encrypt dir

writeInGrid :: Char -> [[Char]] -> Int -> Posn -> [[Char]]
writeInGrid _ [] _ _ = []
writeInGrid w (row:grid) counter (i, j)
    | counter == i = insertAt w j row : writeInGrid w grid (counter + 1) (i, j)
    | otherwise = row : writeInGrid w grid (counter + 1) (i , j)

insertAt :: a -> Int -> [a] -> [a]
insertAt newElement _ [] = [newElement]
insertAt newElement i (a:as)
  | i <= 0 = newElement:as
  | otherwise = a : insertAt newElement (i - 1) as
    
generateRandom :: Int -> IO Placement
generateRandom len =
    do
        pos <- getRandomPosition len
        dir <- getRandomDirection
        return (pos, dir)

addWord :: String -> WordSearchGrid -> IO WordSearchGrid
addWord word grid =
    do
        ((i, j), dir) <- generateRandom (length grid)
        if isSafe (length word) (length grid) (i, j) dir && canFit word grid (i, j) dir
            then return (updateGrid word grid (i, j) dir)
        else
            addWord word grid

sizeofGrid :: [String] -> Double -> Int
sizeofGrid xs d = round (sqrt (fromIntegral numberOfHiddenChars / d))
    where
        numberOfHiddenChars = foldr ((+) . length) 0 xs

createWordSearch :: [ String ] -> Double -> IO WordSearchGrid
createWordSearch words density =
    do
        let grid = replicate size (replicate size '-')
        foldM (flip addWord) grid words
    where
        size = sizeofGrid words density

addWords :: [String] -> WordSearchGrid -> IO WordSearchGrid
addWords words grid = foldM (flip addWord) grid words

getRandomChar :: [String] -> IO Char
getRandomChar words =
    do
        randomPos <- randomRIO (0, length words - 1)
        let word = words !! randomPos
        randomCh <- randomRIO (0, length word - 1)
        return (word !! randomCh)

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