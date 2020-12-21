type WordSearchGrid = [ [ Char ] ]
type Placement = (Posn,Orientation)
type Posn = (Int,Int)
data Orientation = Forward | Back | Up | Down | UpForward | UpBack | DownForward | DownBack deriving (Eq,Ord,Show,Read)

-- utils

directions :: [Orientation]
directions = [Forward, Back, Up, Down, UpForward, UpBack, DownForward, DownBack]

sizeofGrid :: [String] -> Double -> Int
sizeofGrid xs d = ceiling (sqrt (fromIntegral numberOfHiddenChars / d))
    where
        numberOfHiddenChars = foldr ((+) . length) 0 xs

takeXthYth :: [[Char]] -> Posn -> Char
takeXthYth xs pos = (xs !! x) !! y
    where x = fst pos
          y = snd pos

encrypt :: Orientation -> Posn
encrypt o | o == Forward = (0,1)
          | o == Back = (0,-1)
          | o == Up = (-1,0)
          | o == Down = (1,0)
          | o == UpForward = (-1,1)
          | o == UpBack = (-1,-1)
          | o == DownForward = (1,1)
          | o == DownBack = (1,-1)  
          

generatePosition :: WordSearchGrid -> IO Posn
generatePosition grid =
    do 
        ri <- randomRIO (0, length grid - 1)
        rj <- randomRIO (0, length grid - 1)

        return (ri, rj)
 
-- actual code

isSafeDirection :: String -> Posn -> Orientation -> WordSearchGrid -> Bool -- verifica daca e inbounds si coliziunile
isSafeDirection [] _ _ _ _ = True
isSafeDirection (w:word) pos orientation grid = 
	| isInBounds pos gridLength && takeXthYth grid pos == '#' = isSafeDirection word newPos orientation grid
  | otherwise = False
  where
      gridLength = length grid
      dir = encrypt orientation 
      newPos = (fst pos + fst dir, snd pos + snd dir)

isInBounds :: Posn -> Int -> Bool
isInBounds (i, j) n = i >= 0 && j >= 0 && j < n && j < n 
        
generateGrid :: Int -> WordSearchGrid
generateGrid size = [generateRow size | i <- [0..(size-1)]

generateRow :: Int -> String
generateRow size = ["#" | x <- [0..(size - 1)]]

generatePlacements :: [String] -> Pos -> WordSearchGrid -> [Placement] -> [Placement]
generatePlacements (x:xs) pos grid acc |
	  | length acc == length = acc
    | result /= Nothing = generatePlacements xs pos grid (acc ++ (fromJust result))
    | otherwise = 
    where
          result = findWordPlacement x pos grid directions

findWordPlacement :: String -> Posn -> WordSearchGrid -> [Orientation] -> Maybe Placement
findWordPlacement _ _ _ [] = Nothing
findWordPlacement word pos grid (d:directions) 
    | check = Just (pos,d)
    | otherwise = fintWordPlacement word pos grid directions
    where check = isSafeDirection word pos d grid

createWordSearch :: [ String ] -> Double -> IO WordSearchGrid
createWordSearch words density = 
	do
  	let grid = generateGrid size
    (i, j) <- generatePositions grid
    generatePlacements words (i, j) grid []
  where
  	size = sizeOfGrid words density