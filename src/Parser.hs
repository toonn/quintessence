module Parser (
  -- parseExpr
) where

import Text.Megaparsec
import Text.Megaparsec.String

import Syntax
import Lexer

variable :: Parser Expr
variable = do
  x <- identifier
  return (Var x)

number :: Parser Expr
number = do
  x <- nat
  return (Lit (LNat (fromIntegral x)))

bool :: Parser Expr
bool = (reserved "True" >> return (Lit (LBool True)))
    <|> (reserved "False" >> return (Lit (LBool False)))

lambda :: Parser Expr
lambda = do
  reservedOp "\\"
  args <- many identifier
  reservedOp "->"
  body <- expr
  return $ foldr Lam body args

term :: Parser Expr
term = parens expr
    <|> bool
    <|> number
    <|> variable
    <|> lambda

expr :: Parser Expr
expr = do
  es <- some term
  return (foldl1 App es)
