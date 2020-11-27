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
