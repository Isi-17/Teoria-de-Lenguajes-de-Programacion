# The Structural Operational Semantics of WHILE

# Files

## Read only file (do not modify them)

Aexp.hs          - abstract syntax and semantics of Aexp
Bexp.hs          - abstract syntax and semantics of Bexp
State.hs         - implementation of State
While.hs         - abstract syntax for WHILE
WhileLexer.hs    - lexer for WHILE
WhileParser.hs   - parser for WHILE
WhileExamples.hs - simple WHILE programs written in abstract syntax
Examples/*.w     - simple WHILE programs written in concrete syntax

## Editable files (to be completed)

StructuralSemantics.hs - Structural Operational Semantics for WHILE
Exercises03.hs         - Exercises 1 to 5

# Plan

1. Complete the implementation of the Structural Operational Semantics (StructuralSemantics.hs)
2. Complete exercise 1
3. At this point, you have implemented an interpreter for WHILE based on its
   Structural Operational Semantics. Use exercise 1 to test your interpreter.
   Feel free to further experiment with a few more WHILE programs.
4. Complete exercises 2, 3 and 4 to extend the WHILE language with `abort` and
   new looping statements.
5. Complete exercise 5 to define and implement a Structural Operational Semantics for Aexp.
