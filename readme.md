# Hsat - A simple SAT Solver written in Haskell

This should not be seen as a serious attempt at implementing an efficient SAT solver. At the moment it's just a most simple implementation featuring unit propagation and branching. No optimization has been done.

Example of usage:
```
$ ghci sat.hs
*Hsat> solve [[1,2], [-1,-2,-3], [-1,2,-3], [-2,-3], [-1,3]] 3 []
(Just True,[(1,False),(2,True),(3,False)])
```

In this example we solved the formula:
(a or b) and (-a or -b or -c) and (-a or b or -c) and (-b or -c) and (-a or c)
And got the solution:
Formula is SAT: a = false, b = true, c = false

Things that are on the todo list:
* Implement pure literal elimination
* Add a heuristic for choosing the next branching variable
* Look for some possible optimizations
* How can it be parallelized using Haskell?
