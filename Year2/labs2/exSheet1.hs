-- exercise 1

zipL :: ([a], [a]) -> [[a]]
zipL ([], []) = []
zipL (x:xs, y:ys)
    | length xs == length ys = [x, y] : zipL (xs, ys)
    | otherwise = []

unzipL :: [[a]] -> ([a], [a])
unzipL [] = ([], [])
unzipL ([a, b]:xs) = (a : fst (unzipL xs), b : snd (unzipL xs))

-- exercise 2

zipL2 :: ([a], [a]) -> [[a]]
zipL2 ([], []) = []
zipL2 (x:xs, []) = [x] : zipL2 (xs, [])
zipL2 ([], y : ys) = [y] : zipL2 ([], ys)
zipL2 (x: xs, y:ys) = [x, y] : zipL2 (xs, ys)

-- exercise 3

multiZipL :: [[a]] -> [[a]]
multiZipL = multiZip 0

multiZip :: Int -> [[a]] -> [[a]]
multiZip i xss
    | null (takePos i xss) = [] 
    | otherwise = takePos i xss : multiZip (i + 1) xss

takePos :: Int -> [[a]] -> [a]
takePos _ [] = []
takePos i (xs:xss)
    | i >= length xs = takePos i xss
    | otherwise = xs!!i : takePos i xss