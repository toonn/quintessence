module Lexer where

import Data.List
import Control.Monad (void)
import Text.Megaparsec
import Text.Megaparsec.Expr
import Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as L

program :: Parser () -> Parser [()]
program p = many (lexeme p <|> whitespace)

-- lexeme -> qvarid | qconid | qvarsym | qconsym | literal | special
--         | reservedop | reservedid
lexeme :: Parser a -> Parser a
lexeme = L.lexeme whitespace

literal :: Parser Integer
literal = lexeme L.integer -- L.integer parses unsigned integers

specialChars :: String
specialChars = "(),;[]`{}"

special :: Parser Char
special = oneOf specialChars <?> "special"

whitespace :: Parser ()
whitespace = L.space (void spaceChar) lineComment blockComment
  where lineComment = L.skipLineComment "--"
        blockComment = L.skipBlockComment "{-" "-}"

small :: Parser Char
small = lowerChar <|> char '_'

large :: Parser Char
large = upperChar

symbolChars :: String
symbolChars = '\\':"!#$%&*+./<=>?@^|-~:"

symbol :: Parser Char
symbol = try ((oneOf symbolChars <|> symbolChar <|> punctuationChar) >>= check)
  where check = checkFail (`elem` ('_':'"':'\'':specialChars))
                          [ "character '", "' cannot be a symbol" ]

digit :: Parser Char
digit = numberChar

identifierBody :: Parser String
identifierBody = many (small <|> large <|> digit <|> char '\'')

varid :: Parser String
varid = try (((:) <$> small <*> identifierBody) >>= check)
  where check = checkFail (`elem` reservedIds)
                          [ "keyword \"", "\" cannot be an identifier"]

conid :: Parser String
conid = (:) <$> large <*> identifierBody

reservedIds :: [String]
reservedIds = [ "case", "class", "data", "default", "deriving", "do", "else"
              , "foreign", "if", "import", "in", "infix", "infixl", "infixr"
              , "instance", "let", "module", "newtype", "of", "then", "type"
              , "where", "_"]

reservedid :: Parser String
reservedid = choice (noncomposed <$> reservedIds)
  where
    noncomposed w = string w <* notFollowedBy identifierBody

varsym :: Parser String
varsym = try ((((:) <$> (symbol >>= checkColon) <*> many symbol)
              >>= checkReservedOp) >>= checkDashes)
  where
    checkColon = checkFail (== ':')
                           [ "value operator cannot start with '"
                           , "' symbol" ]
    checkReservedOp = checkFail (`elem` reservedOps)
                                [ "reserved operator \""
                                , "\" cannot be an operator" ]
    checkDashes x = case x of
      ('-':'-':dashes) ->
        checkFail (all (== '-'))
                  [ "operator \""
                  , "\" cannot be distinguished from a line comment" ]
                  dashes
      _ -> return x

consym :: Parser String
consym = try (((:) <$> char ':' <*> many symbol) >>= checkReservedOp)
  where
    checkReservedOp = checkFail (`elem` reservedOps)
                                [ "reserved operator \""
                                , "\" cannot be an operator" ]

reservedOp :: String -> Parser ()
reservedOp op = string op *> notFollowedBy alphaNumChar *> whitespace

reservedOps :: [String]
reservedOps = ["..", ":", "::", "=", "\\", "|", "<-", "->", "@", "~", "=>"]

tyvar :: Parser String
tyvar = varid

tycon :: Parser String
tycon = conid

tycls :: Parser String
tycls = conid

checkFail :: (Eq a, Show a, Monad m) => (a -> Bool) -> [String] -> (a -> m a)
checkFail predicate failureMessage = check
  where check x = if predicate x
                  then fail $ intercalate (show x) failureMessage
                  else return x

{-
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

identifier :: Parser Char -> Parser String
identifier initialChar = (lexeme . try) (p >>= check)
  where
    p = (:) <$> initialChar <*> many alphaNumChar
    check x = if x `elem` reservedIds
                then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                else return x

fileContents :: Parser a -> Parser a
fileContents p = do
  whitespace -- Layout rule?
  r <- p
  eof
  return r
-}
