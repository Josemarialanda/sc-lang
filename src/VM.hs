module VM where

import Data.Map as M ( Map, alter, empty, lookup, member )
import Control.Monad.State ( StateT(runStateT), MonadState(put, get), evalStateT )
import Control.Monad.Except ( MonadError(throwError) )
import Data.Maybe ( fromJust, isNothing )
import Data
    ( Prog(..),
      Label,
      ByteCode(..),
      Commands,
      Environment,
      Stack,
      Bop(..),
      Expr(..) )

{- 
The virtual machine needs to store the following things throughout execution:
   * Commands, which is just a list of bytecode instructions, in fact type Commands = [ByteCode]
   * Stack, a stack of integer values
   * Environment, where type Environment = M.Map Name Int
-}

type VMState = (Commands, Stack, Environment)

execute :: Commands -> Environment
execute code = case evalStateT runByteCode (code, [], M.empty) of
  Left e    -> error e
  Right env -> env

runByteCode :: StateT VMState (Either String) Environment
runByteCode = do s <- get
                 case executeInstruction s of
                    Left e             -> throwError e
                    Right ([], _, env) -> return env
                    Right s'           -> put s' >> runByteCode

executeInstruction :: VMState -> Either String VMState
executeInstruction (LOAD_VAL n     : cs, stack, env) = return (cs, n:stack, env)
executeInstruction (READ_VAR name : cs, stack, env) = maybe (throwError $ "READ_VAR -> " ++ name : " is not in memory!") return ((\n -> (cs, n:stack, env)) <$> M.lookup name env)
executeInstruction (WRITE_VAR name   : cs, stack, env) | not (null stack)  = return (cs, tail stack, M.alter ((const . Just) . head $ stack) name env)
                                                 | otherwise         = throwError $ "WRITE_VAR -> " ++ name : " is not in memory!"
executeInstruction (OP op      : cs, stack, env) | length stack >= 2 = let a = head stack
                                                                           b = (head.tail) stack
                                                                        in if b == 0 && op == Div then throwError "Division by zero! " -- show details
                                                                           else return (cs, applyOp op b a : (tail.tail) stack, env)
                                                 | otherwise         = throwError $ "Not enough elements in stack to perform " ++ show op
executeInstruction (LABEL l    : cs, stack, env) = let (cs1, cs2) = span (\instruction -> instruction /= JUMP l) cs 
                                                       cs'        = cs1 ++ (LABEL l:cs1) ++ cs2
                                                    in return (cs', stack, env)
executeInstruction (JUMP  l    : cs, stack, env) = let cs' = goto l cs
                                                    in if isNothing cs' then throwError $ "Jump to label " ++ show l ++ " not possible because " ++ show l ++ " doesn't exist"
                                                       else return (fromJust cs', stack, env)
executeInstruction (JUMPZ l    : cs, stack, env) | not (null stack) = if head stack == 0 then executeInstruction (JUMP l : cs, stack, env)
                                                                      else return (cs, tail stack, env) 
executeInstruction env                           = return env

goto :: Label -> Commands -> Maybe Commands
goto l code = if null xs then Nothing else Just $ tail xs
   where xs = dropWhile (\instruction -> instruction /= LABEL l) code

applyOp :: Bop -> (Int -> Int -> Int)
applyOp Add = (+)
applyOp Sub = (-)
applyOp Mul = (*)
applyOp Div = div