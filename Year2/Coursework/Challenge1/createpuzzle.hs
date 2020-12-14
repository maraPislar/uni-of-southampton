import Data.List ()
import System.Random ( Random(randomRIO) )
import Control.Monad (guard)

type WordSearchGrid = [ [ Char ] ]
type Placement = (Posn,Orientation)
type Posn = (Int,Int)
data Orientation = Forward | Back | Up | Down | UpForward | UpBack | DownForward | DownBack deriving (Eq,Ord,Show,Read)

directions :: [Orientation]
directions = [Forward, Back, Up, Down, UpForward, UpBack, DownForward, DownBack]

takeXthYth :: [[Char]] -> Int -> Int -> Char
takeXthYth xs x y = ( xs !! x ) !! y

-- Challenge 2: create a puzzle

sizeofGrid :: [String] -> Double -> Int
sizeofGrid xs d = round (sqrt (fromIntegral numberOfHiddenChars / d))
    where
        numberOfHiddenChars = foldr ((+) . length) 0 xs

createWordSearch :: [ String ] -> Double -> IO Posn
createWordSearch words density =
    do
        let grid = replicate size (replicate size '-')
        findRandomPositions grid
    where
        size = sizeofGrid words density

findRandomPositions :: WordSearchGrid -> IO Posn
findRandomPositions grid =
    do 
        ri <- randomRIO (0, length grid - 1)
        rj <- randomRIO (0, length grid - 1)

        return (ri, rj)