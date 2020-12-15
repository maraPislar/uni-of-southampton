import Control.Monad
import Data.List

-- given n, "queens n" solves the n-queens problem, returning a list of all the
-- safe arrangements. each solution is a list of the columns where the queens are
-- located for each row
queens :: Int -> [[Int]]
queens n = map fst $ foldM oneMoreQueen ([],[1..n]) [1..n]  where 
 
  -- foldM :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m a
  -- foldM folds (from left to right) in the list monad, which is convenient for 
  -- "nondeterminstically" finding "all possible solutions" of something. the 
  -- initial value [] corresponds to the only safe arrangement of queens in 0 rows
 
  -- given a safe arrangement y of queens in the first i rows, and a list of 
  -- possible choices, "oneMoreQueen y _" returns a list of all the safe 
  -- arrangements of queens in the first (i+1) rows along with remaining choices 
  oneMoreQueen (y,d) _ = [(x:y, delete x d) | x <- d, safe x]  where
 
    -- "safe x" tests whether a queen at column x is safe from previous queens
    safe x = and [x /= c + n && x /= c - n | (n,c) <- zip [1..] y]
 
-- prints what the board looks like for a solution; with an extra newline
printSolution :: [ Int ] -> IO ()
printSolution y = do
     let n = length y
     let bars = replicate (4*n+1) '-'
     mapM_ (\x -> putStrLn (bars ++ ['\n'] ++ ('|': concatMap (\c -> [' ', c ,' ', '|']) [if z == x then '\x265B' else '.' | z <- [1..n]]))) y
     putStrLn bars
     putStrLn ""
 
-- prints all the solutions for N queens
main :: IO ()
main = do  putStrLn "Enter the number of queens to place : "
           num <- getLine
           let parsenum = read num
           mapM_ printSolution $ queens parsenum