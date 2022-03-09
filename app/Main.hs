module Main where

import CodeGen ( compile )
import Parser ( parseSource )
import Data ( Prog(Assign, While, Seqn), Expr(Val, App, Var), Bop(Sub, Mul) )
import VM ( execute )
import System.Environment ( getArgs )

{- Source language
The source language will be a simple imperative programming language:
There are very few keywords: begin, while, do, and end. It supports assignment (:=)
multiplication (*), addition (+) and substraction (-). And every statement end with a semicolon (;).
It should be simple enough to easily convert into equivalent internal source representation.
-}

{- Internal source representation
The internal source representation will capture presedence rules
and break down assignments into their constituent parts. It should
be simple enough to easily convert into equivalent bytecode.
-}

{- Target Language (Bytecode)
Our target language is a simple Bytecode instruction set. It support the following operations:

LOAD_VAL Int    -- Pushes an integer into the stack
READ_VAR Name   -- Pushes the value of a variable into the stack
WRITE_VAR Name  -- Remove value from top of stack and place into variable 
OP Bop          -- Performs binary operation on 2 topmost values on stack
JUMP Label      -- Jumps to instruction on Label
JUMPZ Label     -- Pops top value of stack and Jumps to label if value is zero
LABEL Label     -- A Label to indentify starting and end-points.
-}

{-
Example code: (factorial)
 ___________________________________ __________________________________________________________________________________________________________________ 
|                                   |     Internal representation of source language        |           Target Language                                |
|       begin {                     |-------------------------------------------------------|----------------------------------------------------------|
|           n := 5; a := 1;         |   Seqn [Assign 'n' (Val 5), Assign 'a' (Val 1),       |   [LOAD_VAL 5, WRITE_VAR 'n', LOAD_VAL 1, WRITE_VAR 'a', |                        |
|           b := n;                 |         Assign 'b' (Val 9),                           |    LOAD_VAL n, WRITE_VAR 'b',                            |
|           while (b) do {          |         While (Var 'b') (Seqn [                       |    LABEL 0,                                              |
|               begin {             |           Assign 'a' (App Mul (Var 'a') (Var 'b')),   |    READ_VAR 'b', JUMPZ 1,                                |
|                   a := a*b;       |           Assign 'b' (App Sub (Var 'b') (Val 1))])]   |    READ_VAR 'a', READ_VAR 'b', OP MUL, WRITE_VAR 'a',    |
|                   b := b-1; }     |                                                       |    READ_VAR 'b', LOAD_VAL 1, OP SUB, WRITE_VAR 'b',      |
|               end; };             |                                                       |    JUMP 0,                                               |
|           end; }                  |                                                       |    LABEL 1]                                              |
|       end;                        |                                                       |                                                          |
|                                   |                                                       |                                                          |
 ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾ ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾ 

-}

main :: IO ()
main = do
  filePath:_ <- getArgs 
  src        <- readFile filePath
  print $ execute (compile (parseSource src))
