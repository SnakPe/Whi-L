Whi-L
====================================================

What is this?
---
**Whi-L** is an interpreter for the [while language](https://en.wikipedia.org/wiki/While_loop#cite_ref-3), a language primarily used in theoretical computer science. It also has a REPL for quick experimentation. 

How to Use?
---

1. Compile `Whi-l.hs` using ghc:
```
ghc Whi-l.hs
```
2. For the REPL, just use the resulting executable:
```
Whi-l
```
3. To execute a file, add the file path at the end, e.g.
```
Whi-l "examples/multiplyXandY.wh"
```
4. Using the `--help` option gives you a brief oveview as well
```
Whi-l --help
```

Formal Grammar
---
The while language has the following grammar
"$\left<\mathrm{INDENT}\right>$ A" means that every new statement in A needs to have an added tab character  
\n stands for a new line

Program $\to$ Statement (; Statement)*
IndentedProgram $\to$ $\left<\mathrm{INDENT}\right>$ Program 

Statement $\to$ While | If | Ass | Skip
While $\to$ while Boolean do \n IndentedProgram 
If $\to$ if Boolean then \n IndentedProgram \n else \n IndentedProgram 
Ass $\to$ Variable := Expression
Skip $\to$ skip

Expression $\to$ Boolean | Arithmetic | Variable

Boolean $\to$ true | false | Variable | Not | And | Or | Bigger | Smaller | Equals | ( Boolean )
Not $\to$ ! Boolean
And $\to$ Boolean & Boolean
Or $\to$ Boolean | Boolean
Bigger $\to$ Arithmetic > Arithmetic
Smaller $\to$ Arithmetic < Arithmetic
Equals $\to$ Arithmetic = Arithmetic

Arithmetic $\to$ Number | Variable | Plus | Minus | (Arithmetic)
Plus $\to$ Arithmetic + Arithmetic
Minus $\to$ Arithmetic - Arithmetic
Number $\to$ [0-9]+

Variables $\to$ ([a-z] | [A-Z])([a-z] | [A-Z] | [0-9])*

Inline comments can be made by using "--", for example 
```
x := 69 -- HAHA 69 so funny lol
```   

