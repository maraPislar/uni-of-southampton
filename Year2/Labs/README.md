# Labs

## Lab 3

### A7

Writes an evaluator for a simple stack based machine. Programs for the machine are given as lists of instructions and the stack is represented as a list of ```Maybe Int```.

The function ```evalInst :: Stack -> SMProg -> Stack``` returns the resulting stack after evaluating the program on the given input stack.
***The members of the stack which are ```Nothing``` will be treated as a special case.***

Example: 
```
> evalInst [Just 1, Just 2, Just 3] [Add, Mul]
> [Just 9]
```
