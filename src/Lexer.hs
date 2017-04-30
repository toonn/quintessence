module Lexer where

import Control.Monad (void)
import Text.Megaparsec
import Text.Megaparsec.Expr
import Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as L

whitespace :: Parser ()
whitespace = L.space (void spaceChar) lineComment blockComment
  where lineComment = L.skipLineComment "--"
        blockComment = L.skipBlockComment "{-" "-}"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme whitespace

symbol :: String -> Parser String
symbol = L.symbol whitespace

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

nat :: Parser Integer
nat = lexeme L.integer -- L.integer parses unsigned integers

semicolon :: Parser String
semicolon = symbol ";"

reserved :: String -> Parser ()
reserved w = string w *> notFollowedBy alphaNumChar *> whitespace

reservedWords :: [String]
reservedWords = [ "case", "class", "data", "default", "deriving", "do", "else"
                , "foreign", "if", "import", "in", "infix", "infixl", "infixr"
                , "instance", "let", "module", "newtype", "of", "then", "type"
                , "where", "_"]

reservedOp :: String -> Parser ()
reservedOp op = string op *> notFollowedBy alphaNumChar *> whitespace

reservedOps :: [String]
reservedOps = ["..", ":", "::", "=", "\\", "|", "<-", "->", "@", "~", "=>"]

identifier :: Parser Char -> Parser String
identifier initialChar = (lexeme . try) (p >>= check)
  where
    p = (:) <$> initialChar <*> many alphaNumChar
    check x = if x `elem` reservedWords
                then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                else return x

fileContents :: Parser a -> Parser a
fileContents p = do
  whitespace
  r <- p
  eof
  return r
