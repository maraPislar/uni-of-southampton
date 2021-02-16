-- exercise 1

zipL :: ([a], [a]) -> [[a]]
zipL ([], []) = []
zipL (x:xs, y:ys)
    | length xs == length ys = [x, y] : zipL (xs, ys)
    | otherwise = []

unzipL :: [[a]] -> ([a], [a])
unzipL [] = ([], [])
unzipL ([a, b]:xs) = (a : fst (unzipL xs), b : snd (unzipL xs))