# Labs

## Lab 3

### A7

This part writes an evaluator for a simple stack based machine. Programs for the machine are given as lists of instructions and the stack is represented as a list of ```Maybe Int```.

The function ```evalInst :: Stack -> SMProg -> Stack``` returns the resulting stack after evaluating the program on the given input stack.
***The members of the stack which are ```Nothing``` will be treated as a special case.***

Example: 
```
> evalInst [Just 1, Just 2, Just 3] [Add, Mul]
> [Just 9]
```
### A8

We say that a program for SM is a reducer for a given input stack S, if it does not use any Dup instructions and, after evaluating the program on S, the resulting stack contains a single value.

A maximal reducer for a stack S is a reducer where the resulting single value is maximum among all possible reducers for S. That is, the reducer returns the highest possible value for that input stack assuming the default ordering on Maybe Int.

The function ```findMaxReducers :: Stack -> [SMProg]``` returns all maximal reucers for the given input stack. 

Example:
```
> findMaxReducers [Just 0, Just 1, Just 4, Just 5]
> [[Add, Add, Mul], [Pop, Add Mul]]
```
