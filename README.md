##PLAI - Interpreter
Implemetation of an interpreter from [this assignment](https://cs.brown.edu/courses/cs173/2010/Assignments/extended-interpreter.html) from the 2010 offerring of CS1730 in Brown university.

The language implemeted has the following BNF:
```
<CFWAE> ::= <num>
    | {+ <CFWAE> <CFWAE>}
    | {- <CFWAE> <CFWAE>}
    | {* <CFWAE> <CFWAE>}
    | {/ <CFWAE> <CFWAE>}
    | <id>
    | {if0 <CFWAE> <CFWAE> <CFWAE>}
    | {with {{<id> <CFWAE>} ...} <CFWAE>}
    | {fun {<id> ...} <CFWAE>}
    | {<CFWAE> <CFWAE> ...}
```

#####Features:
The implemtation has the uses/supports the following:

1. Closures for function definitions 
2. First class functions: The implementation allows for functions to passed as paramters and allows returning functions from other functions. 
3. Mutli-argument functions: The implementation allows for functions to have multiple arguments
4. Let bindings: Implementation allows for identifiers to be bound to values for functions 

Note: A variety of test cases, to test the above mentioned functionality are included in the program. They make use of the (test ... ...) syntax defined in PLAI for racket.


####To do
- Make it more usable by creating an interactive environment
