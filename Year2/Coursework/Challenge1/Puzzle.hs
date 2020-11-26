import Data.List ()

type WordSearchGrid = [ [ Char ] ]
type Placement = (Posn,Orientation)
type Posn = (Int,Int)
data Orientation = Forward | Back | Up | Down | UpForward | UpBack | DownForward | DownBack deriving (Eq,Ord,Show,Read)

encrypt :: Orientation -> Posn
encrypt o | o == Forward = (0,1)
          | o == Back = (0,-1)
          | o == Up = (-1,0)
          | o == Down = (1,0)
          | o == UpForward = (-1,1)
          | o == UpBack = (-1,-1)
          | o == DownForward = (1,1)
          | o == DownBack =(1,-1)  

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

takeXthYth :: [[Char]] -> Int -> Int -> Char
takeXthYth xs x y = ( xs !! x ) !! y

isGood :: Posn -> Int -> Bool
isGood (i, j) n = i >= 0 && j >= 0 && j < n && j < n

checkWord :: String -> WordSearchGrid -> Posn -> Posn -> Posn -> Maybe Placement
checkWord [] _ (x, y) (_, _) (a, b) = Just ((y, x), decrypt (a, b))
checkWord word grid (x,y) (i, j)(a, b)
    | isGood (i, j) (length grid) == False = Nothing
    | takeXthYth grid i j == head word = checkWord (tail word) grid (x,y) (i + a, j + b) (a, b)
    | otherwise = Nothing

directions :: [Orientation]
directions = [Forward, Back, Up, Down, UpForward, UpBack, DownForward, DownBack]

func1 :: String -> WordSearchGrid -> Posn -> [Orientation] -> Maybe Placement
func1 _ _ _ [] = Nothing
func1 word grid (i, j) (d:dx)
   | checkWord word grid (i, j) (i, j) (encrypt d) == Nothing = func1 word grid (i, j) dx
   | otherwise = checkWord word grid (i, j) (i, j) (encrypt d)

solveForWord :: String -> WordSearchGrid -> Int -> Maybe Placement
solveForWord word grid counter
   | counter >= ( length grid) * ( length grid ) = Nothing
   | func1 word grid (i, j) directions == Nothing = solveForWord word grid (counter + 1)
   | otherwise = func1 word grid (i, j) directions
   where
      n = length grid
      i = counter `div` n
      j = counter `mod` n

solveSearch :: [String] -> WordSearchGrid -> [(String, Maybe Placement)]
solveSearch words grid = [(word, solveForWord word grid 0) | word <- words ]
