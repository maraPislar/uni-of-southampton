-- exercise one

sequenceIO_ :: [IO a] -> IO ()
sequenceIO_ [] = return ()
sequenceIO_ (x:xs) =
    do
        x
        sequenceIO_ xs

putStrLn :: String -> IO ()
putStrLn s = sequenceIO_ $ map putChar s ++ [putChar '\n']

-- exercise two

adder :: IO ()
adder = 
    do
        putStr "How many numbers? "
        n <- getLine
        adderAux 0 (read n)

adderAux :: Int -> Int -> IO ()
adderAux total 0 = putStr ("The total is " ++ show total ++ "\n")
adderAux total n =
    do
        x <- getLine
        adderAux (total + read x) (n - 1)

-- exercise three

sequenceIO :: [IO a] -> IO [a]
sequenceIO [] = return []
sequenceIO (act:acts) =
    do
        x <- act
        xs <- sequenceIO acts
        return (x : xs)

adder2 :: IO ()
adder2 = 
    do
        putStr "How many numbers? "
        n <- getLine
        ns <- sequenceIO [getLine | _ <- [1..read n]]
        let total = sum $ map read ns
        putStr ("the total is " ++ show total ++ "\n")