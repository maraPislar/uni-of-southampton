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

-- Test Suites, one per exercise
tests :: [(String, [(String, Bool)])]
tests = 
  [ 
  ("Challenge 1",
    [ 
      (
        "Test 1: check first 9 x 9 grid", 
        solveWordSearch exWords1'1 exGrid1'1 == sol1'1
      ),
      (
        "Test 2: check second 9 x 9 grid",
        solveWordSearch exWords1'2 exGrid1'2 == sol1'2
      ),
      (
        "Test 3: check third 10 x 10 grid",
        solveWordSearch exWords1'3 exGrid1'3 == sol1'3
      ),
      (
        "Test 4: check fourth 8 x 8 grid",
        solveWordSearch exWords1'4 exGrid1'4 == sol1'4
      ),
      (
        "Test 5: check empty list of words",
        solveWordSearch [""] exGrid1'1 == error "Empty string is not a word"
      ),
      (
        "Test 6: check grid is empty",
        solveWordSearch exWords1'4 [] == error "The grid is not well formed"
      ),
      (
        "Test 7: check grid is not of type n x n",
        solveWordSearch exWords1'4 exGrid1'5 == error "The grid is not well formed"
      )
    ]
  ),
  ("Challenge 2",
    []
  ), 
  ("Challenge 3",
    [
      (
        "Test 1: check pretty printing a lambda application",
        prettyPrint ex3'1 == "(\\x1 -> x1) \\x1 -> x1"
      ),
      (
        "Test 2: check pretty printing a lambda application",
        prettyPrint ex3'7 == "(\\x1 -> x1 x1) \\x2 -> x2"
      )
      (
        "Test 3: check pretty printing a lambda abstraction",
        prettyPrint ex3'2 == "\\x1 -> x1 \\x1 -> x1"
      ),
      (
        "Test 4: check pretty printing a lambda abstraction",
        prettyPrint ex3'8 == "\\x1 -> x1 x1"
      )
      (
        "Test 5: check pretty printing with macro definitions",
        prettyPrint ex3'3 == "def F = \\x1 -> x1 in \\x2 -> x2 F" -- okay
      ),
      (
        "Test 6: check pretty printing with macro defined but not in the expression",
        prettyPrint ex3'4 == "def F = \\x1 -> x1 in \\x2 -> F x2"
      ),
      (
        "Test 7: check pretty printing with macro overlapping another macro",
        prettyPrint ex3'6 == "def F = \\x1 -> x1 in def G = \\x1 -> (\\x1 -> x1) x2 in \x1 -> G x3"
      ),
      (
        "Test 8: check pretty printing with nested macros",
        prettyPrint ex3'5 == "def G = \\x1 -> \\x2 -> x1 in def F = \\x1 -> \\x2 -> x2 in \\x1 -> \\x2 -> G (F x2 x1) x2" -- okay
      )
    ]
  ), 
  ("Challenge 4",
    []
  ), 
  ("Challenge 5",
    []
  ) 
  ]

-- Main program checks the results of the tests and produces scores
main :: IO ()
main = 
  do
    putStr ""
    testSuite tests

testSuite :: [(String, [(String,Bool)])] -> IO ()
testSuite [] = putStr ""
testSuite ((s,tc):ts) =
  do
    putStrLn (outPrefix (s ++ ": " ++ (message tc))) 
    testCases tc
    testSuite ts

testCases :: [(String,Bool)] -> IO ()
testCases [] = putStr ""
testCases ((s,False):ts) =
  do
    putStr (outPrefix "Did not satisfy assertion: ")
    putStrLn s 
    testCases ts
testCases ((s,True):ts) =
  do
    testCases ts

-- Auxiliary functions to support testing and scoring
outPrefix msg = "  " ++ msg

message :: [(String,Bool)] -> String
message ts =
  let failures = [(s,b) | (s,b) <- ts , b == False] in
  if failures == [] then "all test cases passed"
  else "failed " ++ (show (length failures)) ++ " out of " ++ (show (length ts)) ++ " test cases"


-- lambda calculus expressions test values 
lambdaExpr1 = LamApp (LamAbs 1 (LamVar 1)) (LamAbs 1 (LamVar 1))
lambdaExpr2 = LamApp (LamAbs 1 (LamAbs 2 (LamVar 1))) (LamApp (LamAbs 3 (LamVar 3)) (LamAbs 4 (LamVar 4)))
lambdaExpr3 = LamApp lambdaExpr2 lambdaExpr1
lambdaExpr4 = LamApp lambdaExpr1 lambdaExpr2
lambdaExpr5 = (LamApp (LamAbs 1 (LamAbs 2 (LamVar 1))) (LamVar 3))
lambdaExpr6 = LamApp lambdaExpr5 (LamApp (LamAbs 4 (LamVar 4)) (LamVar 5)) 
-- Smullyan's mockingbird(s)
lambdaOmega = LamAbs 1 (LamApp (LamVar 1) (LamVar 1))
lambdaOmega1 = LamApp lambdaOmega lambdaOmega
-- lambda calculus propositional logic constants and functions
lambdaTrue = LamAbs 1 (LamAbs 2 (LamVar 1))
lambdaFalse = LamAbs 1 (LamAbs 2 (LamVar 2))
lambdaAnd = LamAbs 1 (LamAbs 2 (LamApp (LamApp (LamVar 2) (LamVar 1)) (LamVar 2)))
lambdaAnd1 = LamApp (LamApp lambdaAnd lambdaFalse) lambdaFalse
lambdaAnd2 = LamApp (LamApp lambdaAnd lambdaTrue) lambdaTrue
-- test cases for the church numerals
lambdaZero = LamAbs 1 (LamAbs 2 (LamVar 2))
lambdaSucc = LamAbs 1 (LamAbs 2 (LamAbs 3 (LamApp (LamVar 2) (LamApp (LamApp (LamVar 1) (LamVar 2)) (LamVar 3)))))
lambdaOne = LamAbs 1 (LamAbs 2 (LamApp (LamVar 1) (LamVar 2)))
lambdaSuccZero = LamApp lambdaSucc lambdaZero

-- test for equivalent lambda expressions
equivLam :: LamExpr -> LamExpr -> Bool
-- may need to be replaced by some more sophisticated test
-- such as checking for alpha equivalence
equivLam m n = m == n

-- test for equivalent lambda expressions where the first may be Nothing
equivLam2 :: Maybe LamExpr -> LamExpr -> Bool
-- may need to be replaced by some more sophisticated test
-- such as checking for beta equivalence
equivLam2 Nothing n = False
equivLam2 (Just m) n = m == n

-- test for two let strings being equivalent
equivLetString :: String -> String -> Bool
-- at present just check string equality modulo extra spaces
-- may need to be replaced by some more sophisticated test
equivLetString s t = remSpaces(s) == remSpaces(t)

-- test for two let expressions being equivalent, where the first may be Nothing
-- may need to be replaced by some more sophisticated test
equivLet :: Maybe Expr -> Expr -> Bool
equivLet Nothing e = False
equivLet (Just d) e = d == e

-- removed duplicated spaces
remSpaces :: String -> String
remSpaces "" = ""
remSpaces (' ':' ':s) = remSpaces (' ':s)
remSpaces (c:s) = c:(remSpaces s)