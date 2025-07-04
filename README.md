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
The while language has the following grammar</br>
$\left<\mathrm{INDENT}\right>$ A means that every new statement in A needs to have an added tab character</br>
\n stands for a new line

Program $\to$ Statement (; Statement)*</br>
IndentedProgram $\to$ $\left<\mathrm{INDENT}\right>$ Program 

Statement $\to$ While | If | Ass | Skip </br>
While $\to$ while Boolean do \n IndentedProgram </br>
If $\to$ if Boolean then \n IndentedProgram \n else \n IndentedProgram </br>
Ass $\to$ Variable := Expression </br>
Skip $\to$ skip

Expression $\to$ Boolean | Arithmetic | Variable

Boolean $\to$ true | false | Variable | Not | And | Or | Bigger | Smaller | Equals | ( Boolean ) </br>
Not $\to$ ! Boolean </br>
And $\to$ Boolean & Boolean </br>
Or $\to$ Boolean | Boolean </br>
Bigger $\to$ Arithmetic > Arithmetic </br>
Smaller $\to$ Arithmetic < Arithmetic </br>
Equals $\to$ Arithmetic = Arithmetic

Arithmetic $\to$ Number | Variable | Plus | Minus | (Arithmetic) </br>
Plus $\to$ Arithmetic + Arithmetic </br>
Minus $\to$ Arithmetic - Arithmetic </br>
Number $\to$ [0-9]+

Variables $\to$ ([a-z] | [A-Z])([a-z] | [A-Z] | [0-9])*

Inline comments can be made by using "--", for example 
```
x := 69 -- HAHA 69 so funny lol
```   

Bugs/Weird Behaviour
---
 - If a line only contains a comment with some space before the `--`, that space is not allowed to contain tabs, because that would indicate the start of a subprogram.
 - A semicolon is only placed if there is following statement in the same subprogramm. Noticably, the last statement cannot end in a semicolon, as well as the last statement in the first block `B1` of an `if ... then B1 else B2`.
 - The minus operation is right associative, so e.g. 1-1-1 = 1-(1-1) = 1-0 = 1, and not -1 as one might expect. This can be seen as a feature or as a bug ig, but adds to the cursedness of this language implementation.
