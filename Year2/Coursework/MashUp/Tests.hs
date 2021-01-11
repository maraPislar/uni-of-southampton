import Parsing
import Challenges

-- Main program, testing constants and functions start here
--

-- examples to test for challenge 1
exGrid1'1 :: [[Char]]
exGrid1'1 = [ "HAGNIRTSH" , "SACAGETAK", "GCSTACKEL","MGHKMILKI","EKNLETGCN","TNIRTLETE","IRAAHCLSR","MAMROSAGD","GIZKDDNRG" ] 
exWords1'1 :: [[Char]]
exWords1'1 = [ "HASKELL","STRING","STACK","MAIN","METHOD"]
sol1'1 = [("HASKELL", Just ((0, 0), DownForward )), ("STRING",Just((7,0),Back)),("STACK", Just((2,2),Forward)),("MAIN",Just((2,7),Up)),("METHOD",Just((4,3),Down))]

exGrid1'2 :: [[Char]]
exGrid1'2 = ["ROBREUMBR", "AURPEPSAN", "UNLALMSEE", "YGAUNPYYP", "NLMNBGENA", "NBLEALEOR", "ALRYPBBLG", "NREPBEBEP", "YGAYAROMR"]
exWords1'2 :: [[Char]]
exWords1'2 = [ "BANANA", "ORANGE", "MELON", "RASPBERRY","APPLE","PLUM","GRAPE" ]
sol1'2 = [("BANANA",Just ((5,6),UpBack)),("ORANGE",Just ((1,0),DownForward)),("MELON",Just ((7,8),Up)),("RASPBERRY",Just ((8,0),DownBack)),("APPLE",Just ((2,8),UpForward)),("PLUM",Just ((5,1),DownBack)),("GRAPE",Just ((8,6),Up))]

exGrid1'3 :: [[Char]]
exGrid1'3 = ["WVERTICALL", "ROOAFFLSAB", "ACRILIATOA", "NDODKONWDC", "DRKESOODDK", "OEEPZEGLIW", "MSIIHOAERA", "ALRKRRIRER", "KODIDEDRCD", "HELWSLEUTH"]
exWords1'3 :: [[Char]]
exWords1'3 = ["Seek", "Find", "Random", "Sleuth", "Backward", "Vertical", "Diagonal", "WIKIPEDIA", "HORIZONTAL", "WORDSEARCH"]
sol1'3 = [("Seek",Nothing),("Find",Nothing),("Random",Nothing),("Sleuth",Nothing),("Backward",Nothing),("Vertical",Nothing),("Diagonal",Nothing),("WIKIPEDIA",Just ((3,9),Up)),("HORIZONTAL",Just ((0,9),UpForward)),("WORDSEARCH",Just ((0,0), DownForward))]

exGrid1'4 :: [[Char]]
exGrid1'4 = ["EBNREDOR", "DEGEUODR", "OICDEHOG", "CKBFORUR", "EOKUKCGR", "DOBDGHHG", "CCEGBFOH", "KFDERDOF"]
exWords1'4 :: [[Char]]
exWords1'4 = ["CODE", "COOKIE", "GREEN", "BUG", "FUDGE", "DOUGH", "DOG"]
sol1'4 = [("CODE",Just ((0,3),Up)),("COOKIE",Just ((1,6),Up)),("GREEN",Just ((6,4),UpBack)),("BUG",Just ((2,3),DownForward)),("FUDGE",Just ((3,3),Down)),("DOUGH",Just ((6,1),Down)),("DOG",Just ((5,7),UpForward))]

exGrid1'5 = ["WERFDGS", "SFDGASD", "SFGDTSR", "SFGDTS", "SFHDYA", "SFTDGSJ", "DFSGAJS"]

-- examples to test for challenge 3
ex3'1 :: LamMacroExpr
ex3'1 = LamDef [] (LamApp (LamAbs 1 (LamVar 1)) (LamAbs 1 (LamVar 1)))
ex3'2 :: LamMacroExpr
ex3'2 = LamDef [] (LamAbs 1 (LamApp (LamVar 1) (LamAbs 1 (LamVar 1))))
ex3'3 :: LamMacroExpr
ex3'3 = LamDef [ ("F", LamAbs 1 (LamVar 1) ) ] (LamAbs 2 (LamApp (LamVar 2) (LamMacro "F")))
ex3'4 :: LamMacroExpr
ex3'4 = LamDef [ ("F", LamAbs 1 (LamVar 1) ) ] (LamAbs 2 (LamApp (LamAbs 1 (LamVar 1)) (LamVar 2))) 
ex3'5 :: LamMacroExpr
ex3'5 = LamDef [("G", LamAbs 1 (LamAbs 2 (LamVar 1))), ("F", LamAbs 1 (LamAbs 2 (LamVar 2)))] 
    (LamAbs 1 (LamAbs 2 (LamApp (LamAbs 1 (LamAbs 2 (LamVar 1))) (LamApp (LamApp (LamAbs 1 (LamAbs 2 (LamVar 2))) (LamApp (LamVar 2) (LamVar 1))) (LamVar 2)))))
ex3'6 :: LamMacroExpr
ex3'6 = LamDef [("F", LamAbs 1 (LamVar 1)), ("G", LamAbs 1 (LamApp (LamAbs 1 (LamVar 1)) (LamVar 2)))] 
    (LamAbs 1 (LamApp (LamAbs 1 (LamApp (LamAbs 1 (LamVar 1)) (LamVar 2))) (LamVar 3)))
ex3'7 :: LamMacroExpr
ex3'7 = LamDef [] (LamApp (LamAbs 1 (LamApp (LamVar 1) (LamVar 1))) (LamAbs 2 (LamVar 2)))
ex3'8 :: LamMacroExpr
ex3'8 = LamDef [] (LamAbs 1 (LamApp (LamVar 1) (LamVar 1)))


-- examples to test for challenge 4
ex4'1 :: [Char]
ex4'1 = "x1 (x2 x3)"
ex4'2 :: [Char]
ex4'2 = "x1 x2 F"
ex4'3 :: [Char]
ex4'3 = "def F = \\x1 -> x1 in \\x2 -> x2 F"
ex4'4 :: [Char]
ex4'4 = "def F = \\x1 -> x1 (def G = \\x1 -> x1 in x1) in \\x2 -> x2"
ex4'5 :: [Char]
ex4'5 = "def F = \\x1 -> x1 in def F = \\x2 -> x2 x1 in x1"
ex4'6 :: [Char]
ex4'6 = "def F = x1 in F"
ex4'7 :: [Char]
ex4'7 = "def F = \\x1 -> x1 in \\x1 -> F G"
ex4'8 :: [Char]
ex4'8 = "def F = \\x1 -> x2 in F"
ex4'9 :: [Char]
ex4'9 = "def F = \\x1 -> x1 in \\x1 -> F g"
ex4'10 :: [Char]
ex4'10 = "\\x1 -> \\x2 -> \\x3 -> x1 x2 x3"
ex4'11 :: [Char]
ex4'11 = "\\x1 -> x1 x2"
ex4'12 :: [Char]
ex4'12 = ""
ex4'13 :: [Char]
ex4'13 = "\\x1 -> (\\x2 -> x3 (\\x4 -> x5)) x6"

-- examples to test for challenge 5
exId =  (LamAbs 1 (LamVar 1))
ex5'1 = LamDef [] (LamApp (LamVar 1) (LamVar 2))
ex5'2 = (LamDef [ ("F", exId) ] (LamVar 2) )
ex5'3 = (LamDef [ ("F", exId) ] (LamMacro "F") )
ex5'4 = (LamDef [ ("F", exId) ] (LamApp (LamMacro "F") (LamMacro "F")))
ex5'5 = LamDef [] exId
ex5'6 = LamDef [] (LamAbs 1 (LamApp (LamVar 1) (LamVar 2)))
ex5'7 = LamDef [] (LamApp (LamAbs 1 (LamVar 1)) (LamAbs 2 (LamVar 3)))
ex5'8 = LamDef [("F", LamAbs 1 (LamApp (LamVar 1) (LamVar 2))), ("G", exId)] (LamApp (LamMacro "F") (LamMacro "G"))

-- Test Suites, one per exercise
tests :: [(String, [(String, IO Bool)])]
tests = 
  [ 
  ("Challenge 1",
    [ 
      (
        "Test 1: check first 9 x 9 grid", 
        return (solveWordSearch exWords1'1 exGrid1'1 == sol1'1)
      ),
      (
        "Test 2: check second 9 x 9 grid",
        return (solveWordSearch exWords1'2 exGrid1'2 == sol1'2)
      ),
      (
        "Test 3: check third 10 x 10 grid",
        return (solveWordSearch exWords1'3 exGrid1'3 == sol1'3)
      ),
      (
        "Test 4: check fourth 8 x 8 grid",
        return (solveWordSearch exWords1'4 exGrid1'4 == sol1'4)
      ),
      (
        "Test 5: check empty list of words",
        return (solveWordSearch [""] exGrid1'1 == [("", Nothing)])
      ),
      (
        "Test 6: check grid is empty",
        return (null (solveWordSearch exWords1'4 []))
      ),
      (
        "Test 7: check grid is not of type n x n",
        return (null (solveWordSearch exWords1'4 exGrid1'5))
      )
    ]
  ),
  ("Challenge 2",
    [
      (
        "Test 1: create a word search puzzle with few words and low density",
        createAndSolve ["WORD"] 0.3
      ),
      (
        "Test 2: create word puzzle with more words and large density",
        createAndSolve ["WORD", "MALL", "CALL", "USED"] 0.7
      ),
      (
        "Test 3: create word search with one word and low density",
        createAndSolve ["WORD"] 0.001
      ),
      (
        "Test 4: create word search with many words",
        createAndSolve ["WORD", "VANILLA", "CHOCOLATE", "MAYO", "WELLDONE", "HELLO", "WORLD", "WISH", "ENCYCLOPEDIA"] 0.1
      ),
      (
        "Test 5: create word search with no words",
        createAndSolve [] 0.1
      ),
      (
        "Test 6: create word search with high density",
        createAndSolve ["HELLO", "HOW", "ARE", "YOU", "TODAY", "TELL", "ME"] 0.7
      )
    ]
  ), 
  ("Challenge 3",
    [
      (
        "Test 1: check pretty printing a lambda application",
        return (prettyPrint ex3'1 == "(\\x1 -> x1) (\\x1 -> x1)")
      ),
      (
        "Test 2: check pretty printing a lambda application",
        return (prettyPrint ex3'7 == "(\\x1 -> x1 x1) (\\x2 -> x2)")
      ),
      (
        "Test 3: check pretty printing a lambda abstraction",
        return (prettyPrint ex3'2 == "\\x1 -> x1 (\\x1 -> x1)")
      ),
      (
        "Test 4: check pretty printing a lambda abstraction",
        return (prettyPrint ex3'8 == "\\x1 -> x1 x1")
      ),
      (
        "Test 5: check pretty printing with macro definitions",
        return (prettyPrint ex3'3 == "def F = \\x1 -> x1 in \\x2 -> x2 F")
      ),
      (
        "Test 6: check pretty printing with macro defined but not in the expression",
        return (prettyPrint ex3'4 == "def F = \\x1 -> x1 in \\x2 -> F x2")
      ),
      (
        "Test 7: check pretty printing with macro overlapping another macro",
        return (prettyPrint ex3'6 == "def F = \\x1 -> x1 in def G = \\x1 -> (\\x1 -> x1) x2 in \\x1 -> G x3")
      ),
      (
        "Test 8: check pretty printing with nested macros",
        return (prettyPrint ex3'5 == "def G = \\x1 -> \\x2 -> x1 in def F = \\x1 -> \\x2 -> x2 in \\x1 -> \\x2 -> G (F (x2 x1) x2)")
      )
    ]
  ), 
  ("Challenge 4",
    [
      (
        "Test 1: parse nested lambda application with parenthesis",
        return (parseLamMacro ex4'1 == Just (LamDef [] (LamApp (LamVar 1) (LamApp (LamVar 2) (LamVar 3)))))
      ),
      (
        "Test 2: parse simple lambda aaplication",
        return (parseLamMacro ex4'2 == Just (LamDef [] (LamApp (LamApp (LamVar 1) (LamVar 2)) (LamMacro "F"))))
      ),
      (
        "Test 3: parse definitions of macros and expressions",
        return (parseLamMacro ex4'3 == Just (LamDef [("F",LamAbs 1 (LamVar 1))] (LamAbs 2 (LamApp (LamVar 2) (LamMacro "F")))))
      ),
      (
        "Test 4: parse lambda macro that is not in grammar",
        return (parseLamMacro ex4'4 == Nothing)
      ),
      (
        "Test 5: parse lambda macro expression with repeted macro definitions",
        return (parseLamMacro  ex4'5 == Nothing)
      ),
      (
        "Test 6: parse lambda expression with unclosed macros",
        return (parseLamMacro ex4'6 == Nothing)
      ),
      (
        "Test 7: parse lambda expression with undefined macro",
        return (parseLamMacro ex4'7 == Just (LamDef [("F",LamAbs 1 (LamVar 1))] (LamAbs 1 (LamApp (LamMacro "F") (LamMacro "G")))))
      ),
      (
        "Test 8: parse lambda expression with unclosed macro",
        return (parseLamMacro ex4'8 == Nothing)
      ),
      (
        "Test 9: parse lambda expression with unparsed rest of expression",
        return (parseLamMacro ex4'9 == Nothing)
      ),
      (
        "Test 10: parse lambda expression with nested abstractions",
        return (parseLamMacro ex4'10 == Just (LamDef [] (LamAbs 1 (LamAbs 2 (LamAbs 3 (LamApp (LamApp (LamVar 1) (LamVar 2)) (LamVar 3)))))))
      ),
      (
        "Test 11: parse lambda expression with lambda abstraction and application",
        return (parseLamMacro ex4'11 == Just (LamDef [] (LamAbs 1 (LamApp (LamVar 1) (LamVar 2)))))
      ),
      (
        "Test 12: parse empty string",
        return (parseLamMacro ex4'12 == Nothing)
      ),
      (
        "Test 13: parse lambda expression with multiple parenthesis",
        return (parseLamMacro ex4'13 == Just (LamDef [] (LamAbs 1 (LamApp (LamAbs 2 (LamApp (LamVar 3) (LamAbs 4 (LamVar 5)))) (LamVar 6)))))
      )
    ]
  ), 
  ("Challenge 5",
    [
      (
        "Test 1: cps in a lambda application",
        return (cpsTransform ex5'1 == LamDef [] (LamAbs 0 (LamApp (LamAbs 5 (LamApp (LamVar 5) (LamVar 1))) (LamAbs 3 (LamApp (LamAbs 6 (LamApp (LamVar 6) (LamVar 2))) (LamAbs 4 (LamApp (LamApp (LamVar 3) (LamVar 4)) (LamVar 0))))))))
      ),
      (
        "Test 2: cps in a lambda abstraction",
        return (cpsTransform ex5'5 == LamDef [] (LamAbs 0 (LamApp (LamVar 0) (LamAbs 1 (LamAbs 2 (LamApp (LamVar 2) (LamVar 1)))))))
      ),
      (
        "Test 3: cps in a lambda abstraction with application",
        return (cpsTransform ex5'6 == LamDef [] (LamAbs 0 (LamApp (LamVar 0) (LamAbs 1 (LamAbs 3 (LamApp (LamAbs 6 (LamApp (LamVar 6) (LamVar 1))) (LamAbs 4 (LamApp (LamAbs 7 (LamApp (LamVar 7) (LamVar 2))) (LamAbs 5 (LamApp (LamApp (LamVar 4) (LamVar 5)) (LamVar 3)))))))))))
      ),
      (
        "Test 4: cps in a lambda application with abstractions",
        return (cpsTransform ex5'7 == LamDef [] (LamAbs 0 (LamApp (LamAbs 6 (LamApp (LamVar 6) (LamAbs 1 (LamAbs 7 (LamApp (LamVar 7) (LamVar 1)))))) (LamAbs 4 (LamApp (LamAbs 7 (LamApp (LamVar 7) (LamAbs 2 (LamAbs 8 (LamApp (LamVar 8) (LamVar 3)))))) (LamAbs 5 (LamApp (LamApp (LamVar 4) (LamVar 5)) (LamVar 0))))))))
      ),
      (
        "Test 5: cps with macro definition",
        return (cpsTransform ex5'2 == LamDef [("F",LamAbs 0 (LamApp (LamVar 0) (LamAbs 1 (LamAbs 3 (LamApp (LamVar 3) (LamVar 1))))))] (LamAbs 4 (LamApp (LamVar 4) (LamVar 2))))
      ),
      (
        "Test 6: cps with macro definition in expression",
        return (cpsTransform ex5'3 == LamDef [("F",LamAbs 0 (LamApp (LamVar 0) (LamAbs 1 (LamAbs 2 (LamApp (LamVar 2) (LamVar 1))))))] (LamMacro "F"))
      ),
      (
        "Test 7: cps with macros and application between macro",
        return (cpsTransform ex5'4 == LamDef [("F",LamAbs 0 (LamApp (LamVar 0) (LamAbs 1 (LamAbs 2 (LamApp (LamVar 2) (LamVar 1))))))] (LamAbs 3 (LamApp (LamMacro "F") (LamAbs 4 (LamApp (LamMacro "F") (LamAbs 5 (LamApp (LamApp (LamVar 4) (LamVar 5)) (LamVar 3))))))))
      ),
      (
        "Test 8: cps with simple abstraction",
        return (cpsTransform ex5'5 == LamDef [] (LamAbs 0 (LamApp (LamVar 0) (LamAbs 1 (LamAbs 2 (LamApp (LamVar 2) (LamVar 1)))))))
      )
    ]
  ),
  ("Challenge 6",
    []
  ) 
  ]

-- Main program checks the results of the tests and produces scores
main :: IO ()
main = 
  do
    putStr ""
    testSuite tests

testSuite :: [(String, [(String, IO Bool)])] -> IO ()
testSuite [] = putStr ""
testSuite ((s,tc):ts) =
  do
    mes <- message tc 0
    putStrLn (outPrefix (s ++ ": " ++ mes)) 
    testCases tc
    testSuite ts

testCases :: [(String, IO Bool)] -> IO ()
testCases [] = putStr ""
testCases ((s, b):ts) =
  do
    bo <- b
    if bo then
     testCases ts
    else do
     putStr (outPrefix "Did not satisfy assertion: ") 
     putStrLn s 
     testCases ts
      

-- Auxiliary functions to support testing and scoring
outPrefix :: [Char] -> [Char]
outPrefix msg = "  " ++ msg

message :: [(String,IO Bool)] -> Int -> IO String
message [] count 
  | count /= 0 = return ("failed " ++ show count)
  | count == 0 = return "all test cases passed"

message ((s, b):ts) count =
  do
    bo <- b
    if not bo then
      message ts (count + 1)
    else
      message ts count

createAndSolve :: [ String ] -> Double -> IO Bool
createAndSolve words maxDensity =   do g <- createWordSearch words maxDensity
                                       let soln = solveWordSearch words g
                                       return (checkWordsExist soln)

checkWordsExist :: [(String, Maybe Placement)] -> Bool
checkWordsExist [] = True
checkWordsExist ((s, p):ps)
  | p == Nothing = False 
  | otherwise = checkWordsExist ps