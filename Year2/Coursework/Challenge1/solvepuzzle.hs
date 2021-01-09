import Data.List ()

type WordSearchGrid = [ [ Char ] ]
type Placement = (Posn,Orientation)
type Posn = (Int,Int)
data Orientation = Forward | Back | Up | Down | UpForward | UpBack | DownForward | DownBack deriving (Eq,Ord,Show,Read)


-- Challenge 1: solve a puzzle

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
isGood (i, j) n = i >= 0 && j >= 0 && j < n && j < n

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
   | counter >= n * n = Nothing
   | findDirection word grid (i, j) directions == Nothing = solveForWord word grid (counter + 1)
   | otherwise = findDirection word grid (i, j) directions
   where
      n = length grid
      i = counter `div` n
      j = counter `mod` n

-- solve the board for each word on the board
solveSearch :: [String] -> WordSearchGrid -> [(String, Maybe Placement)]
solveSearch words grid = [(word, solveForWord word grid 0) | word <- words ]