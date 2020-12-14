import Data.List ()

type WordSearchGrid = [ [ Char ] ]
type Placement = (Posn,Orientation)
type Posn = (Int,Int)
data Orientation = Forward | Back | Up | Down | UpForward | UpBack | DownForward | DownBack deriving (Eq,Ord,Show,Read)


-- Challenge 2: create a puzzle

sizeofGrid :: [String] -> Double -> Int
sizeofGrid xs d = round (sqrt (fromIntegral numberOfHiddenChars / d))
    where
        numberOfHiddenChars = foldr ((+) . length) 0 xs

