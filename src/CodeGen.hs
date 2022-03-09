module CodeGen where

import Control.Monad.Writer ( MonadWriter(tell), execWriter, Writer )
import Control.Monad.State ( StateT(runStateT), MonadState(put, get), evalStateT )
import Data
    ( Prog(..),
      Label,
      ByteCode(..),
      Commands,
      Environment,
      Stack,
      Bop(..),
      Expr(..) )

-- | Converts internal representation code to bytecode
{-
For this task, we require 2 things:
  * Some state to store the program and the current label
  * Some way to generate code as we traverse the AST

Thankfully there exist 2 monads that can do just this: Writer and State.
In order to combine their functionality we can 'compose' them by means
of monad transformers. A free implementation would also work and would
actually prove more elegant and almost as efficient (if not as efficient) 
(if we use a freer encoding, in order to avoid linear time complexity in the bind operator)
-}
compile :: Prog -> Commands
compile prog = execWriter $ runStateT go (prog, 0)
  where go :: StateT (Prog, Label) (Writer Commands) ()
        go = do (p,l) <- get
                case p of
                  Assign name expr    ->    tell (readExpr expr <> [WRITE_VAR name])
                  If expr prog1 prog2 -> do tell (readExpr expr)
                                            tell [JUMPZ l]
                                            put (prog2, l) >> go
                                            tell [LABEL l]
                                            put (prog1, l) >> go
                  While expr prog     -> do tell [LABEL l]
                                            tell (readExpr expr)
                                            tell [JUMPZ (l+1)]
                                            put (prog,  l) >> go
                                            tell [JUMP l]
                                            tell [LABEL (l+1)]
                  Seqn progs          -> if null progs then tell []
                                         else do put (head progs, l) >> go
                                                 put (Seqn $ tail progs, l) >> go

-- | Converts Expr to bytecode equivalent
readExpr :: Expr -> [ByteCode]
readExpr (Val n)              = [LOAD_VAL n]
readExpr (Var name)           = [READ_VAR name]
readExpr (App op expr1 expr2) = readExpr expr1 <> readExpr expr2 <> [OP op]