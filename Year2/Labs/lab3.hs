{-# LANGUAGE DeriveGeneric #-}

module Exercises (evalInst,Instruction(..),Stack,SMProg) where

import GHC.Generics (Generic,Generic1)
import Control.DeepSeq


data Instruction = Add | Sub | Mul | Div | Dup | Pop deriving (Eq,Ord,Show,Generic)
type Stack = [Maybe Int]
type SMProg = [Instruction] 

instance NFData (Instruction)

-- Exercise A7

eval :: Stack -> Instruction -> Stack
eval [] _ = error "Cannot operate on an empty stack"

eval stack Pop = tail stack

eval stack Dup = [a] ++ stack
    where a = head stack

eval stack Add
    | length stack == 1 = error "Not enough elements to perform instruction"
    | head stack == Nothing || head (eval stack Pop) == Nothing = [Nothing] ++ newStack
    | otherwise = [Just (a + b)] ++ newStack
    where
        (Just a, Just b) = (head stack, head (eval stack Pop))
        newStack = eval (eval stack Pop) Pop

eval stack Sub
    | length stack == 1 = error "Not enough elements to perform instruction"
    | head stack == Nothing || head (eval stack Pop) == Nothing = [Nothing] ++ newStack
    | otherwise = [Just (a - b)] ++ (eval (eval stack Pop) Pop)
    where
        (Just a, Just b) = (head stack, head (eval stack Pop))
        newStack = eval (eval stack Pop) Pop

eval stack Mul
    | length stack == 1 = error "Not enough elements to perform instruction"
    | head stack == Nothing || head (eval stack Pop) == Nothing = [Nothing] ++ newStack
    | otherwise = [Just (a * b)] ++ (eval (eval stack Pop) Pop)
    where
        (Just a, Just b) = (head stack, head (eval stack Pop))
        newStack = eval (eval stack Pop) Pop

eval stack Div
    | length stack == 1 = error "Not enough elements to perform instruction"
    | head stack == Nothing || head (eval stack Pop) == Nothing = [Nothing] ++ newStack
    | b == 0 = [Nothing] ++ newStack
    | otherwise = [Just (a `div` b)] ++ (eval (eval stack Pop) Pop)
    where
        (Just a, Just b) = (head stack, head (eval stack Pop))
        newStack = eval (eval stack Pop) Pop


evalInst :: Stack -> SMProg -> Stack
evalInst [] _ = error "Cannot operate on an empty stack"
evalInst x [] = x
evalInst stack (i:is) = evalInst (eval stack i) is

-- Exercise A8

findAllSMProgs :: Stack -> SMProg -> [SMProg]
findAllSMProgs [_] p = [p]
findAllSMProgs stack p = add ++ mul ++ sub ++ div ++ pop
    where
        add = findAllSMProgs (evalInst stack [Add]) ([Add] ++ p)
        sub = findAllSMProgs (evalInst stack [Mul]) ([Mul] ++ p)
        mul = findAllSMProgs (evalInst stack [Sub]) ([Sub] ++ p)
        div = findAllSMProgs (evalInst stack [Div]) ([Div] ++ p)
        pop = findAllSMProgs (evalInst stack [Pop]) ([Pop] ++ p)

createPairs :: Stack -> [SMProg] -> [(Maybe Int, SMProg)]
createPairs _ [] = []
createPairs stack (p:ps) = (x, p) : createPairs stack ps
    where
        x = head (evalInst stack p)

findMaxValue :: [(Maybe Int, SMProg)] -> Int -> Int
findMaxValue [] a = a
findMaxValue ((x, _):xs) maxi
    | x == Nothing = findMaxValue xs maxi
    | maxi < a = findMaxValue xs a
    | otherwise = findMaxValue xs maxi
    where
        Just a = x

findInst :: [(Maybe Int, SMProg)] -> Int -> [SMProg]
findInst [] _ = []
findInst ((x, y):xs) b
    | x == Nothing = findInst xs b
    | a == b = y : findInst xs b 
    | otherwise = findInst xs b
    where
        Just a = x

findMaxReducers :: Stack -> [SMProg]
findMaxReducers [] = []
findMaxReducers stack = findInst pairs maxi
    where
        pairs = createPairs stack (findAllSMProgs stack [])
        maxi = findMaxValue pairs (-1000000)
        
-- Exercise A9

evalPowerSequence :: Stack -> SMProg -> Stack
evalPowerSequence stack [] = stack
evalPowerSequence stack (p:ps)
    | p == Dup = evalPowerSequence (eval stack p) ps
    | p == Mul && length stack /= 1 = evalPowerSequence (eval stack p) ps
    | otherwise = []

createList :: Int -> [Instruction]
createList l = replicate l Dup ++ replicate l Mul

per :: Eq a => [a] -> [[a]]
per xs = permutations xs

prelucrate :: [[Instruction]] -> [[Instruction]]
prelucrate [] = []
prelucrate (x : xs)
    | (head x == Dup) && (x!!(length x - 1) == Mul) = x : prelucrate xs
    | otherwise = prelucrate xs

findSolution :: [SMProg] -> Stack -> Int -> Bool
findSolution [] _ _ = False
findSolution (p:ps) stack k
    | length s == 1 && result == y = True
    | otherwise = findSolution ps stack k
    where
        Just result = head s
        s = evalPowerSequence stack p
        y = 2 ^ k

isPossiblePower :: Int -> Int -> Bool
isPossiblePower k l
    | l == k - 1 = True
    | l >= k = False
    | l < 0 = False
    | otherwise = findSolution (prelucrate (per (createList l))) [Just 2] k
