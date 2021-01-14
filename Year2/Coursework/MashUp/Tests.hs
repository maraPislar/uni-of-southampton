-- ==================================================================
-- (c) University of Southampton 2021 
-- Title :  Test Cases for comp2209 Functional Programming Challenges
-- Author:  Theodora-Mara Pislar
-- Date  :  14 Jan 2021
-- ==================================================================

import Parsing
import Challenges

-- Test cases for each challenge

-- examples to test for challenge 1
exGrid1'1 :: [[Char]]
exGrid1'1 = [ "HAGNIRTSH" , "SACAGETAK", "GCSTACKEL","MGHKMILKI","EKNLETGCN",
              "TNIRTLETE","IRAAHCLSR","MAMROSAGD","GIZKDDNRG" ] 
exWords1'1 :: [[Char]]
exWords1'1 = [ "HASKELL","STRING","STACK","MAIN","METHOD"]
sol1'1 :: [([Char], Maybe ((Int, Int), Orientation))]
sol1'1 = [("HASKELL", Just ((0, 0), DownForward )), ("STRING",Just((7,0),Back)),
          ("STACK", Just((2,2),Forward)),("MAIN",Just((2,7),Up)),("METHOD",Just((4,3),Down))]

exGrid1'2 :: [[Char]]
exGrid1'2 = ["ROBREUMBR", "AURPEPSAN", "UNLALMSEE", "YGAUNPYYP", "NLMNBGENA", 
             "NBLEALEOR", "ALRYPBBLG", "NREPBEBEP", "YGAYAROMR"]
exWords1'2 :: [[Char]]
exWords1'2 = [ "BANANA", "ORANGE", "MELON", "RASPBERRY","APPLE","PLUM","GRAPE" ]
sol1'2 :: [([Char], Maybe ((Int, Int), Orientation))]
sol1'2 = [("BANANA",Just ((5,6),UpBack)),("ORANGE",Just ((1,0),DownForward)),
          ("MELON",Just ((7,8),Up)),("RASPBERRY",Just ((8,0),DownBack)),
          ("APPLE",Just ((2,8),UpForward)),("PLUM",Just ((5,1),DownBack)),("GRAPE",Just ((8,6),Up))]

exGrid1'3 :: [[Char]]
exGrid1'3 = ["WVERTICALL", "ROOAFFLSAB", "ACRILIATOA", "NDODKONWDC", "DRKESOODDK", 
             "OEEPZEGLIW", "MSIIHOAERA", "ALRKRRIRER", "KODIDEDRCD", "HELWSLEUTH"]
exWords1'3 :: [[Char]]
exWords1'3 = ["Seek", "Find", "Random", "Sleuth", "Backward", "Vertical", 
              "Diagonal", "WIKIPEDIA", "HORIZONTAL", "WORDSEARCH"]
sol1'3 :: [([Char], Maybe ((Int, Int), Orientation))]
sol1'3 = [("Seek",Nothing),("Find",Nothing),("Random",Nothing),("Sleuth",Nothing),
          ("Backward",Nothing),("Vertical",Nothing),("Diagonal",Nothing),("WIKIPEDIA",Just ((3,9),Up)),
          ("HORIZONTAL",Just ((0,9),UpForward)),("WORDSEARCH",Just ((0,0), DownForward))]

exGrid1'4 :: [[Char]]
exGrid1'4 = ["EBNREDOR", "DEGEUODR", "OICDEHOG", "CKBFORUR", "EOKUKCGR", "DOBDGHHG", "CCEGBFOH", "KFDERDOF"]
exWords1'4 :: [[Char]]
exWords1'4 = ["CODE", "COOKIE", "GREEN", "BUG", "FUDGE", "DOUGH", "DOG"]
sol1'4 :: [([Char], Maybe ((Int, Int), Orientation))]
sol1'4 = [("CODE",Just ((0,3),Up)),("COOKIE",Just ((1,6),Up)),
          ("GREEN",Just ((6,4),UpBack)),("BUG",Just ((2,3),DownForward)),
          ("FUDGE",Just ((3,3),Down)),("DOUGH",Just ((6,1),Down)),("DOG",Just ((5,7),UpForward))]
exGrid1'5 :: [[Char]]
exGrid1'5 = ["WERFDGS", "SFDGASD", "SFGDTSR", "SFGDTS", "SFHDYA", "SFTDGSJ", "DFSGAJS"]

-- examples to test for challenge 2

exWords2'1 :: [[Char]]
exWords2'1 = ["WORD"]
exWords2'2 :: [[Char]]
exWords2'2 = ["WORD", "MALL", "CALL", "USED"]
exWords2'3 :: [[Char]]
exWords2'3 = ["WORD", "VANILLA", "CHOCOLATE", "MAYO", "WELLDONE", "HELLO", "WORLD", "WISH", "ENCYCLOPEDIA"]
exWords2'4 :: [[Char]]
exWords2'4 = ["HELLO", "HOW", "ARE", "YOU", "TODAY", "TELL", "ME"]

-- examples to test for challenge 3
ex3'1 :: LamMacroExpr
ex3'1 = LamDef [] (LamApp (LamAbs 1 (LamVar 1)) (LamAbs 1 (LamVar 1)))
sol3'1 :: [Char]
sol3'1 = "(\\x1 -> x1) (\\x1 -> x1)"
ex3'2 :: LamMacroExpr
ex3'2 = LamDef [] (LamAbs 1 (LamApp (LamVar 1) (LamAbs 1 (LamVar 1))))
sol3'2 :: [Char]
sol3'2 = "\\x1 -> x1 (\\x1 -> x1)"
ex3'3 :: LamMacroExpr
ex3'3 = LamDef [ ("F", LamAbs 1 (LamVar 1) ) ] (LamAbs 2 (LamApp (LamVar 2) (LamMacro "F")))
sol3'3 :: [Char]
sol3'3 = "def F = \\x1 -> x1 in \\x2 -> x2 F"
ex3'4 :: LamMacroExpr
ex3'4 = LamDef [ ("F", LamAbs 1 (LamVar 1) ) ] (LamAbs 2 (LamApp (LamAbs 1 (LamVar 1)) (LamVar 2))) 
sol3'4 :: [Char]
sol3'4 = "def F = \\x1 -> x1 in \\x2 -> F x2"
ex3'5 :: LamMacroExpr
ex3'5 = LamDef [("G", LamAbs 1 (LamAbs 2 (LamVar 1))), ("F", LamAbs 1 (LamAbs 2 (LamVar 2)))] 
        (LamAbs 1 (LamAbs 2 (LamApp (LamAbs 1 (LamAbs 2 (LamVar 1))) (LamApp (LamApp (LamAbs 1 
        (LamAbs 2 (LamVar 2))) (LamApp (LamVar 2) (LamVar 1))) (LamVar 2)))))
sol3'5 :: [Char]
sol3'5 = "def G = \\x1 -> \\x2 -> x1 in def F = \\x1 -> \\x2 -> x2 in \\x1 -> \\x2 -> G (F (x2 x1) x2)"
ex3'6 :: LamMacroExpr
ex3'6 = LamDef [("F", LamAbs 1 (LamVar 1)), ("G", LamAbs 1 (LamApp (LamAbs 1 (LamVar 1)) (LamVar 2)))] 
        (LamAbs 1 (LamApp (LamAbs 1 (LamApp (LamAbs 1 (LamVar 1)) (LamVar 2))) (LamVar 3)))
sol3'6 :: [Char]
sol3'6 = "def F = \\x1 -> x1 in def G = \\x1 -> (\\x1 -> x1) x2 in \\x1 -> G x3"
ex3'7 :: LamMacroExpr
ex3'7 = LamDef [] (LamApp (LamAbs 1 (LamApp (LamVar 1) (LamVar 1))) (LamAbs 2 (LamVar 2)))
sol3'7 :: [Char]
sol3'7 = "(\\x1 -> x1 x1) (\\x2 -> x2)"
ex3'8 :: LamMacroExpr
ex3'8 = LamDef [] (LamAbs 1 (LamApp (LamVar 1) (LamVar 1)))
sol3'8 :: [Char]
sol3'8 = "\\x1 -> x1 x1"
ex3'9 :: LamMacroExpr
ex3'9 = LamDef [("F", LamVar 5), ("G", LamApp (LamVar 5) (LamVar 2))] 
        (LamAbs 5 (LamApp (LamApp (LamVar 5) (LamVar 2)) (LamVar 5)))
sol3'9 :: [Char]
sol3'9 = "def F = x5 in def G = x5 x2 in \\x5 -> G F"
ex3'10 :: LamMacroExpr
ex3'10 = LamDef [("F", LamVar 5)] (LamAbs 5 (LamApp (LamVar 5) (LamApp (LamVar 5) (LamVar 5))))
sol3'10 :: [Char]
sol3'10 = "def F = x5 in \\x5 -> F (F F)"

-- examples to test for challenge 4
ex4'1 :: [Char]
ex4'1 = "x1 (x2 x3)"
sol4'1 :: Maybe LamMacroExpr
sol4'1 = Just (LamDef [] (LamApp (LamVar 1) (LamApp (LamVar 2) (LamVar 3))))
ex4'2 :: [Char]
ex4'2 = "x1 x2 F"
sol4'2 :: Maybe LamMacroExpr
sol4'2 = Just (LamDef [] (LamApp (LamApp (LamVar 1) (LamVar 2)) (LamMacro "F")))
ex4'3 :: [Char]
ex4'3 = "def F = \\x1 -> x1 in \\x2 -> x2 F"
sol4'3 :: Maybe LamMacroExpr
sol4'3 = Just (LamDef [("F",LamAbs 1 (LamVar 1))] (LamAbs 2 (LamApp (LamVar 2) (LamMacro "F"))))
ex4'4 :: [Char]
ex4'4 = "def F = \\x1 -> x1 (def G = \\x1 -> x1 in x1) in \\x2 -> x2"
sol4'4 :: Maybe a
sol4'4 = Nothing
ex4'5 :: [Char]
ex4'5 = "def F = \\x1 -> x1 in def F = \\x2 -> x2 x1 in x1"
sol4'5 :: Maybe a
sol4'5 = Nothing
ex4'6 :: [Char]
ex4'6 = "def F = x1 in F"
sol4'6 :: Maybe a
sol4'6 = Nothing
ex4'7 :: [Char]
ex4'7 = "def F = \\x1 -> x1 in \\x1 -> F G"
sol4'7 :: Maybe LamMacroExpr
sol4'7 = Just (LamDef [("F",LamAbs 1 (LamVar 1))] (LamAbs 1 (LamApp (LamMacro "F") (LamMacro "G"))))
ex4'8 :: [Char]
ex4'8 = "def F = \\x1 -> x2 in F"
sol4'8 :: Maybe a
sol4'8 = Nothing
ex4'9 :: [Char]
ex4'9 = "def F = \\x1 -> x1 in \\x1 -> F g"
sol4'9 :: Maybe a
sol4'9 = Nothing
ex4'10 :: [Char]
ex4'10 = "\\x1 -> \\x2 -> \\x3 -> x1 x2 x3"
sol4'10 :: Maybe LamMacroExpr
sol4'10 = Just (LamDef [] (LamAbs 1 (LamAbs 2 (LamAbs 3 (LamApp (LamApp (LamVar 1) (LamVar 2)) (LamVar 3))))))
ex4'11 :: [Char]
ex4'11 = "\\x1 -> x1 x2"
sol4'11 :: Maybe LamMacroExpr
sol4'11 = Just (LamDef [] (LamAbs 1 (LamApp (LamVar 1) (LamVar 2))))
ex4'12 :: [Char]
ex4'12 = ""
sol4'12 :: Maybe a
sol4'12 = Nothing
ex4'13 :: [Char]
ex4'13 = "\\x1 -> (\\x2 -> x3 (\\x4 -> x5)) x6"
sol4'13 :: Maybe LamMacroExpr
sol4'13 = Just (LamDef [] (LamAbs 1 (LamApp (LamAbs 2 (LamApp (LamVar 3) (LamAbs 4 (LamVar 5)))) (LamVar 6))))

-- examples to test for challenge 5
exId :: LamExpr
exId =  LamAbs 1 (LamVar 1)
ex5'1 :: LamMacroExpr
ex5'1 = LamDef [] (LamApp (LamVar 1) (LamVar 2))
sol5'1 :: LamMacroExpr
sol5'1 = LamDef [] (LamAbs 0 (LamApp (LamAbs 5 (LamApp (LamVar 5) (LamVar 1))) 
         (LamAbs 3 (LamApp (LamAbs 6 (LamApp (LamVar 6) (LamVar 2))) (LamAbs 4 
         (LamApp (LamApp (LamVar 3) (LamVar 4)) (LamVar 0)))))))
ex5'2 :: LamMacroExpr
ex5'2 = LamDef [ ("F", exId) ] (LamVar 2)
sol5'2 :: LamMacroExpr
sol5'2 = LamDef [("F",LamAbs 0 (LamApp (LamVar 0) (LamAbs 1 (LamAbs 3 (LamApp 
        (LamVar 3) (LamVar 1))))))] (LamAbs 4 (LamApp (LamVar 4) (LamVar 2)))
ex5'3 :: LamMacroExpr
ex5'3 = LamDef [ ("F", exId) ] (LamMacro "F")
sol5'3 :: LamMacroExpr
sol5'3 = LamDef [("F",LamAbs 0 (LamApp (LamVar 0) (LamAbs 1 
         (LamAbs 2 (LamApp (LamVar 2) (LamVar 1))))))] (LamMacro "F")
ex5'4 :: LamMacroExpr
ex5'4 = LamDef [ ("F", exId) ] (LamApp (LamMacro "F") (LamMacro "F"))
ex5'5 :: LamMacroExpr
ex5'5 = LamDef [] exId
sol5'5 :: LamMacroExpr
sol5'5 = LamDef [] (LamAbs 0 (LamApp (LamVar 0) (LamAbs 1 
         (LamAbs 2 (LamApp (LamVar 2) (LamVar 1))))))
sol5'4 :: LamMacroExpr
sol5'4 = LamDef [("F",LamAbs 0 (LamApp (LamVar 0) (LamAbs 1 (LamAbs 2 
         (LamApp (LamVar 2) (LamVar 1))))))] (LamAbs 3 (LamApp 
         (LamMacro "F") (LamAbs 4 (LamApp (LamMacro "F") (LamAbs 5 
         (LamApp (LamApp (LamVar 4) (LamVar 5)) (LamVar 3)))))))
ex5'6 :: LamMacroExpr
ex5'6 = LamDef [] (LamAbs 1 (LamApp (LamVar 1) (LamVar 2)))
sol5'6 :: LamMacroExpr
sol5'6 = LamDef [] (LamAbs 0 (LamApp (LamVar 0) (LamAbs 1 (LamAbs 3 (LamApp 
         (LamAbs 6 (LamApp (LamVar 6) (LamVar 1))) (LamAbs 4 (LamApp 
         (LamAbs 7 (LamApp (LamVar 7) (LamVar 2))) (LamAbs 5 (LamApp (LamApp 
         (LamVar 4) (LamVar 5)) (LamVar 3))))))))))
ex5'7 :: LamMacroExpr
ex5'7 = LamDef [] (LamApp (LamAbs 1 (LamVar 1)) (LamAbs 2 (LamVar 3)))
sol5'7 :: LamMacroExpr
sol5'7 = LamDef [] (LamAbs 0 (LamApp (LamAbs 6 (LamApp (LamVar 6) (LamAbs 1 
         (LamAbs 7 (LamApp (LamVar 7) (LamVar 1)))))) (LamAbs 4 (LamApp (LamAbs 7 
         (LamApp (LamVar 7) (LamAbs 2 (LamAbs 8 (LamApp (LamVar 8) (LamVar 3)))))) 
         (LamAbs 5 (LamApp (LamApp (LamVar 4) (LamVar 5)) (LamVar 0)))))))
ex5'8 :: LamMacroExpr
ex5'8 = LamDef [("F", LamAbs 1 (LamApp (LamVar 1) (LamVar 2))), ("G", exId)] 
        (LamApp (LamMacro "F") (LamMacro "G"))
sol5'8 :: LamMacroExpr
sol5'8 = LamDef [("F",LamAbs 0 (LamApp (LamVar 0) (LamAbs 1 (LamAbs 3 (LamApp 
         (LamAbs 6 (LamApp (LamVar 6) (LamVar 1))) (LamAbs 4 (LamApp (LamAbs 7 
         (LamApp (LamVar 7) (LamVar 2))) (LamAbs 5 (LamApp (LamApp (LamVar 4) (LamVar 5)) (LamVar 3
         )))))))))),("G",LamAbs 8 (LamApp (LamVar 8) (LamAbs 1 (LamAbs 9 (LamApp (LamVar 9) 
         (LamVar 1))))))] (LamAbs 10 (LamApp (LamMacro "F") (LamAbs 11 (LamApp (LamMacro "G") 
         (LamAbs 12 (LamApp (LamApp (LamVar 11) (LamVar 12)) (LamVar 10)))))))

-- examples to test for challenge 6

-- (\x1 -> x1 x2)
ex6'1 :: LamMacroExpr
ex6'1 = LamDef [] (LamAbs 1 (LamApp (LamVar 1) (LamVar 2)))
--  def F = \x1 -> x1 in F  
ex6'2 :: LamMacroExpr
ex6'2 = LamDef [ ("F",exId) ] (LamMacro "F")
--  (\x1 -> x1) (\x2 -> x2)   
ex6'3 :: LamMacroExpr
ex6'3 = LamDef [] ( LamApp exId (LamAbs 2 (LamVar 2)))
--  (\x1 -> x1 x1)(\x1 -> x1 x1)  
wExp :: LamExpr
wExp = (LamAbs 1 (LamApp (LamVar 1) (LamVar 1)))
ex6'4 :: LamMacroExpr
ex6'4 = LamDef [] (LamApp wExp wExp)
--  (\\x1 -> x1 x3) (\\x4 -> x4 x5)
ex6'5 :: LamMacroExpr
ex6'5 = LamDef [] (LamApp (LamAbs 1 (LamApp (LamVar 1) (LamVar 2))) (LamAbs 4 (LamApp (LamVar 4) (LamVar 5))))
-- \\x1 -> \\x2 -> (\\x3 -> x3 x2) (\\x4 -> x4 x1)
ex6'6 :: LamMacroExpr
ex6'6 = LamDef [] (LamAbs 1 (LamAbs 2 (LamApp (LamAbs 3 (LamApp (LamVar 3) (LamVar 2))) (LamAbs 4 (LamApp (LamVar 4) (LamVar 1))))))

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
        createAndSolve exWords2'1 0.3
      ),
      (
        "Test 2: create word puzzle with more words and large density",
        createAndSolve exWords2'2 0.7
      ),
      (
        "Test 3: create word search with one word and low density",
        createAndSolve exWords2'1 0.001
      ),
      (
        "Test 4: create word search with many words",
        createAndSolve exWords2'3 0.1
      ),
      (
        "Test 5: create word search with no words",
        createAndSolve [] 0.1
      ),
      (
        "Test 6: create word search with high density",
        createAndSolve exWords2'4 0.7
      ),
      (
        "Test 7: check size of grid with few words",
        checkBoardSize (createWordSearch exWords2'1 0.3) 4
      ),
      (
        "Test 8: check size of grid with many words",
        checkBoardSize (createWordSearch exWords2'4 0.7) 6
      )
    ]
  ), 
  ("Challenge 3",
    [
      (
        "Test 1: check pretty printing a lambda application",
        return (prettyPrint ex3'1 == sol3'1)
      ),
      (
        "Test 2: check pretty printing a lambda application",
        return (prettyPrint ex3'7 == sol3'7)
      ),
      (
        "Test 3: check pretty printing a lambda abstraction",
        return (prettyPrint ex3'2 == sol3'2)
      ),
      (
        "Test 4: check pretty printing a lambda abstraction",
        return (prettyPrint ex3'8 == sol3'8)
      ),
      (
        "Test 5: check pretty printing with macro definitions",
        return (prettyPrint ex3'3 == sol3'3)
      ),
      (
        "Test 6: check pretty printing with macro defined but not in the expression",
        return (prettyPrint ex3'4 == sol3'4)
      ),
      (
        "Test 7: check pretty printing with macro overlapping another macro",
        return (prettyPrint ex3'6 == sol3'6)
      ),
      (
        "Test 8: check pretty printing with nested macros",
        return (prettyPrint ex3'5 == sol3'5)
      ),
      (
        "Test 9: check nested macros and repeated small macro",
        return (prettyPrint ex3'9 == sol3'9)
      ),
      (
        "Test 10: check repeated macros into an expression",
        return (prettyPrint ex3'10 == sol3'10)
      )
    ]
  ), 
  ("Challenge 4",
    [
      (
        "Test 1: parse nested lambda application with parenthesis",
        return (parseLamMacro ex4'1 == sol4'1)
      ),
      (
        "Test 2: parse simple lambda aaplication",
        return (parseLamMacro ex4'2 == sol4'2)
      ),
      (
        "Test 3: parse definitions of macros and expressions",
        return (parseLamMacro ex4'3 == sol4'3)
      ),
      (
        "Test 4: parse lambda macro that is not in grammar",
        return (parseLamMacro ex4'4 == sol4'4)
      ),
      (
        "Test 5: parse lambda macro expression with repeted macro definitions",
        return (parseLamMacro  ex4'5 == sol4'5)
      ),
      (
        "Test 6: parse lambda expression with unclosed macros",
        return (parseLamMacro ex4'6 == sol4'6)
      ),
      (
        "Test 7: parse lambda expression with undefined macro",
        return (parseLamMacro ex4'7 == sol4'7)
      ),
      (
        "Test 8: parse lambda expression with unclosed macro",
        return (parseLamMacro ex4'8 == sol4'8)
      ),
      (
        "Test 9: parse lambda expression with unparsed rest of expression",
        return (parseLamMacro ex4'9 == sol4'9)
      ),
      (
        "Test 10: parse lambda expression with nested abstractions",
        return (parseLamMacro ex4'10 == sol4'10)
      ),
      (
        "Test 11: parse lambda expression with lambda abstraction and application",
        return (parseLamMacro ex4'11 == sol4'11)
      ),
      (
        "Test 12: parse empty string",
        return (parseLamMacro ex4'12 == sol4'12)
      ),
      (
        "Test 13: parse lambda expression with multiple parenthesis",
        return (parseLamMacro ex4'13 == sol4'13)
      )
    ]
  ), 
  ("Challenge 5",
    [
      (
        "Test 1: cps in a lambda application",
        return (cpsTransform ex5'1 == sol5'1)
      ),
      (
        "Test 2: cps in a lambda abstraction",
        return (cpsTransform ex5'5 == sol5'5)
      ),
      (
        "Test 3: cps in a lambda abstraction with application",
        return (cpsTransform ex5'6 == sol5'6)
      ),
      (
        "Test 4: cps in a lambda application with abstractions",
        return (cpsTransform ex5'7 == sol5'7)
      ),
      (
        "Test 5: cps with macro definition",
        return (cpsTransform ex5'2 == sol5'2)
      ),
      (
        "Test 6: cps with macro definition in expression",
        return (cpsTransform ex5'3 == sol5'3)
      ),
      (
        "Test 7: cps with macros and application between macro",
        return (cpsTransform ex5'4 == sol5'4)
      ),
      (
        "Test 8: cps with macros",
        return (cpsTransform ex5'8 == sol5'8)
      )
    ]
  ),
  ("Challenge 6",
    [
      (
        "Test 1: compare simple application",
        return (compareInnerOuter ex6'1 10 == (Just 0, Just 0, Just 6, Just 6))
      ),
      (
        "Test 2: compare complex application",
        return (compareInnerOuter ex6'3 10 == (Just 1, Just 1, Just 8, Just 8)) 
      ),
      (
        "Test 3: compare simple macro definition",
        return (compareInnerOuter ex6'2 10 == (Just 1, Just 1, Just 2, Just 2))
      ),
      (
        "Test 5: test for non exhaustive lambda application",
        return (compareInnerOuter ex6'4 10 == (Nothing, Nothing, Nothing, Nothing))
      ),
      (
        "Test 6: test for lambda application",
        return (compareInnerOuter ex6'5 20 == (Just 2, Just 2, Just 17, Just 17))
      ),
      (
        "Test 7: test for lambda abstraction",
        return (compareInnerOuter ex6'6 20 == (Just 2, Just 2, Nothing, Nothing))
      )
    ]
  ) 
  ]

-- Main program checks the results of the tests and produces scores
main :: IO ()
main = 
  do
    putStr ""
    testSuite tests

-- checks each test case for each challenge
testSuite :: [(String, [(String, IO Bool)])] -> IO ()
testSuite [] = putStr ""
testSuite ((s,tc):ts) =
  do
    -- the heading of the challenge
    mes <- message tc 0
    putStrLn (getHeading (s ++ ": " ++ mes))
    -- check which tests failed
    testCases tc
    testSuite ts

-- keelp score of the failed tests
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

-- helper function to support testing and scoring
getHeading :: [Char] -> [Char]
getHeading msg = "  " ++ msg

-- check which tests failed
testCases :: [(String, IO Bool)] -> IO ()
testCases [] = putStr ""
testCases ((s, b):ts) =
  do
    bo <- b
    if bo then
     testCases ts
    else do
     putStr (getHeading "Did not satisfy assertion: ") 
     putStrLn s 
     testCases ts

-- create a word puzzle and solve it
-- check if all the words hidden are also found by the solver
createAndSolve :: [ String ] -> Double -> IO Bool
createAndSolve words maxDensity =
  do 
    g <- createWordSearch words maxDensity
    let soln = solveWordSearch words g
    return (checkWordsExist soln)

-- check if all the words were found by the solver
checkWordsExist :: [(String, Maybe Placement)] -> Bool
checkWordsExist [] = True
checkWordsExist ((s, p):ps)
  -- if a placement of one word is Nothing => is not on the board
  | p == Nothing = False 
  | otherwise = checkWordsExist ps

-- check if the board size is as expected when a puzzle is created
checkBoardSize :: IO [[Char]] -> Int -> IO Bool
checkBoardSize grid expectedSize = 
  do
    g <- grid
    return (length g == expectedSize)