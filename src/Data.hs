module Data where

import Data.Map as M ( Map )

-- | AST
data Prog = Assign Name Expr
          | If Expr Prog Prog -- 0 is FALSE and any non zero int is TRUE
          | While Expr Prog
          | Seqn [Prog]
  deriving (Eq, Show)

-- | An expression can only be one of the following three things:
data Expr = Val Int           -- An integer value
          | Var Name          -- A variable that stores an integer value
          | App Bop Expr Expr -- The application of a binary opreration
                              -- on two expressions
  deriving (Eq, Show)
  
-- | Variable names are just characters
type Name = Char

-- | Supported binary operations
data Bop  = Add
          | Sub
          | Mul
          | Div
  deriving (Show, Eq)

-- | Bytecode

-- | The virtual machine (which executes the target language)
--   is composed of 3 things:
type Stack       = [Int]          -- A stack of integer values
type Environment = M.Map Name Int -- An environment of variables
type Commands    = [ByteCode]     -- A list of bytecode instructions

-- | The virtual machine has the following instructions set:
data ByteCode = LOAD_VAL Int 
              | READ_VAR Name 
              | WRITE_VAR Name 
              | OP Bop 
              | JUMP Label
              | JUMPZ Label 
              | LABEL Label
  deriving (Eq, Show)

-- | A Label is an int
type Label = Int