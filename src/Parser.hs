{-# LANGUAGE LambdaCase #-}

module Parser ( parseSource ) where

import Data.Char ( isDigit )
import Control.Monad ( MonadPlus(..) )
import Control.Applicative ( Alternative(..) )
import Data (Prog(..), Expr(..), Bop(..) )

newtype Parser a = Parser { parse :: String -> [(a,String)] }

runParser :: Parser a -> String -> a
runParser m s =
  case parse m s of
    [(res, [])] -> res -- We successfully parsed the input stream
    [(_, rs)]   -> error "Parser did not consume entire stream."
    _           -> error "Parser error."

instance Functor Parser where
  fmap f (Parser cs) = Parser (\s -> [(f a, b) | (a, b) <- cs s])

instance Monad Parser where
  return a = Parser (\s -> [(a,s)])
  p >>= f  = Parser $ \s -> concatMap (\(a, s') -> parse (f a) s') $ parse p s

instance Applicative Parser where
  pure = return
  (Parser cs1) <*> (Parser cs2) = Parser (\s -> [(f a, s2) | (f, s1) <- cs1 s, (a, s2) <- cs2 s1])

instance Alternative Parser where
  empty = mzero
  (<|>) = option

instance MonadPlus Parser where
  mzero = Parser (const [])
  mplus p q = Parser (\s -> parse p s ++ parse q s)

item :: Parser Char
item = Parser $ \case
   []     -> []
   (c:cs) -> [(c,cs)]

option :: Parser a -> Parser a -> Parser a
option  p q = Parser $ \s ->
  case parse p s of
    []     -> parse q s
    res    -> res

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = item >>= \c ->
  if p c
  then return c
  else mzero

oneOf :: [Char] -> Parser Char
oneOf s = satisfy (`elem` s)

sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep element = fmap (:) element <*> many (sep *> element) <|> pure []

chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p op a = (p `chainl1` op) <|> return a

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainl1` op = do {a <- p; rest a}
  where rest a = (do f <- op
                     b <- p
                     rest (f a b))
                 <|> return a

char :: Char -> Parser Char
char c = satisfy (c ==)

natural :: Parser Integer
natural = read <$> some (satisfy isDigit)

string :: String -> Parser String
string [] = return []
string (c:cs) = do { char c; string cs; return (c:cs)}

token :: Parser a -> Parser a
token p = do { a <- p; spaces ; return a}

reserved :: String -> Parser String
reserved s = token (string s)

spaces :: Parser String
spaces = many $ oneOf " \n\r"

digit :: Parser Char
digit = satisfy isDigit

identifier :: Parser Char
identifier = oneOf ['a'..'z']

number :: Parser Int
number = do
  s <- string "-" <|> return []
  cs <- some digit
  return $ read (s ++ cs)

parens :: Parser a -> Parser a
parens m = do
  reserved "("
  n <- m
  reserved ")"
  return n

curly :: Parser a -> Parser a
curly m = do
  reserved "{"
  n <- m
  reserved "}"
  return n

parseProg :: Parser Prog
parseProg = parseAssign <|> parseIf <|> parseWhile <|> parseSeqn

parseAssign :: Parser Prog
parseAssign = do id   <- token identifier
                 token (string ":=")
                 expr <- parseExpr
                 reserved ";"
                 return $ Assign id expr

parseIf :: Parser Prog
parseIf = do reserved "if"
             expr     <- parens parseExpr
             reserved "then"
             progIf   <- curly parseProg
             reserved ";"
             reserved "else"
             progElse <- curly parseProg
             reserved ";"
             return $ If expr progIf progElse

parseWhile :: Parser Prog
parseWhile = do reserved "while"
                expr <- parseExpr
                reserved "do"
                prog <- curly parseProg
                reserved ";"
                reserved "end"
                reserved ";"
                return $ While expr prog

parseSeqn :: Parser Prog
parseSeqn = do reserved "begin"
               progs <- curly (many parseProg)
               spaces
               reserved "end"
               reserved ";"
               return $ Seqn progs

parseExpr :: Parser Expr
parseExpr = parseTerm `chainl1` addop

parseTerm :: Parser Expr
parseTerm = parseFactor `chainl1` (mulop <|> divop)

parseFactor :: Parser Expr
parseFactor = Val <$> number <|>
              Var <$> oneOf ['a'..'z'] <|>
              parens parseExpr

infixOp :: String -> (a -> a -> a) -> Parser (a -> a -> a)
infixOp x f = reserved x >> return f

addop :: Parser (Expr -> Expr -> Expr)
addop = infixOp "+" (App Add) <|> infixOp "-" (App Sub)

mulop :: Parser (Expr -> Expr -> Expr)
mulop = infixOp "*" (App Mul)

divop :: Parser (Expr -> Expr -> Expr)
divop = infixOp "/" (App Div)

parseSource :: String -> Prog
parseSource = runParser parseProg


-- tests

assignTest1 :: Bool
assignTest1 = parse parseAssign "a:=1;"
                == [(Assign 'a' (Val 1),"")]

ifTest1 :: Bool
ifTest1 = parse parseIf "if (1+1) then {a:=1;}; else {a:=2;};"
            ==  [(If (App Add (Val 1) (Val 1)) (Assign 'a' (Val 1)) (Assign 'a' (Val 2)),"")]

whileTest1 :: Bool
whileTest1 = parse parseWhile "while (1+1) do {a:=1;}; end;"
                == [(While (App Add (Val 1) (Val 1)) (Assign 'a' (Val 1)),"")]

seqnTest1 :: String
seqnTest1 = "begin { a := 1; b := 2; } end;"