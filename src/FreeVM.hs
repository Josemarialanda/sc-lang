{-# LANGUAGE DeriveFunctor #-}

module FreeVM where

import Control.Monad.Free (Free, liftF, iterM, foldFree)
import Data (Bop, Label, Name, Stack, Environment)
import Control.Monad (forever)
import Data.Functor (($>))

data BCF next = LOAD_VAL  Int   next
              | READ_VAR  Name  next
              | WRITE_VAR Name  next
              | OP        Bop   next
              | JUMP      Label next
              | JUMPZ     Label next
              | LABEL     Label next
    deriving (Functor, Show)

type BC = Free BCF

loadVal :: Int -> BC ()
loadVal = undefined

readVar :: Name -> BC ()
readVar = undefined

writeVar :: Name -> BC ()
writeVar = undefined

op :: Bop -> BC ()
op = undefined

jump :: Label -> BC ()
jump = undefined

jumpz :: Label -> BC ()
jumpz = undefined

label :: Label -> BC ()
label = undefined

-- Example

data TeletypeF next
    = PrintLine String next
    | ReadLine (String -> next)
    deriving Functor

type Teletype = Free TeletypeF

printLine :: String -> Teletype ()
printLine str = liftF (PrintLine str ())

readLine :: Teletype String
readLine = liftF (ReadLine id)

echo :: Teletype ()
echo = readLine >>= printLine

foreverEcho :: Teletype a
foreverEcho = forever echo

interpretTeletype :: Teletype a -> IO a
interpretTeletype = foldFree run where
  run :: TeletypeF a -> IO a
  run (PrintLine str x) = putStrLn str $> x
  run (ReadLine f) = fmap f getLine