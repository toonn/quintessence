module Lexer
  ( whitespace
  , varid
  , qvarid
  , tyvar
  , conid
  , qconid
  , tycon
  , qtycon
  , tycls
  , qtycls
  , reservedid
  , varsym
  , qvarsym
  , consym
  , qconsym
  , reservedop
  , modid
  )
where

import Data.List
import Data.Maybe
import Data.Scientific
import Control.Monad (void)
import Text.Megaparsec as Mp
import Text.Megaparsec.Expr
import Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as L

program :: Parser [String]
program = many (lexeme <|> ("" <$ whitespace))

lexeme :: Parser String
lexeme = qvarid <|> qconid <|> qvarsym <|> qconsym
     <|> (show <$> literal) <|> (show <$> special)
     <|> reservedop <|> reservedid

literal :: Parser Integer
literal = integer -- <|> float <|> char <|> string

specialChars :: String
specialChars = "(),;[]`{}"

special :: Parser Char
special = oneOf specialChars <?> "special"

whitespace :: Parser ()
whitespace = L.space (void whitechar) lineComment blockComment
  where lineComment = L.skipLineComment "--"
        blockComment = L.skipBlockComment "{-" "-}"

whitechar :: Parser Char
whitechar = spaceChar

space :: Parser Char
space = Mp.char ' '

graphic :: Parser Char
graphic = small <|> large <|> symbol <|> digit <|> special <|> doublequote
      <|> apostrophe

small :: Parser Char
small = lowerChar <|> Mp.char '_'

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
identifierBody = many (small <|> large <|> digit <|> apostrophe)

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
    noncomposed w = Mp.string w <* notFollowedBy identifierBody

varsym :: Parser String
varsym = try ((((:) <$> (symbol >>= checkColon) <*> many symbol)
              >>= checkReservedOp) >>= checkDashes)
  where
    checkColon = checkFail (== ':')
                           [ "value operator cannot start with '"
                           , "' symbol" ]
    checkDashes x = case x of
      ('-':'-':dashes) ->
        checkFail (all (== '-'))
                  [ "operator \""
                  , "\" cannot be distinguished from a line comment" ]
                  dashes
      _ -> return x

consym :: Parser String
consym = try (((:) <$> Mp.char ':' <*> many symbol) >>= checkReservedOp)

reservedops :: [String]
reservedops = ["..", ":", "::", "=", "\\", "|", "<-", "->", "@", "~", "=>"]

reservedop :: Parser String
reservedop = choice (Mp.string <$> reservedops) <* notFollowedBy alphaNumChar <* whitespace

tyvar :: Parser String
tyvar = varid

tycon :: Parser String
tycon = conid

tycls :: Parser String
tycls = conid

modid :: Parser String
modid = (++) <$> (concat <$> many dotted) <*> conid
  where dotted = (++) <$> conid <*> ((:[]) <$> Mp.char '.')

qvarid :: Parser String
qvarid = qualified varid

qconid :: Parser String
qconid = qualified conid

qtycon :: Parser String
qtycon = qualified tycon

qtycls :: Parser String
qtycls = qualified tycls

qvarsym :: Parser String
qvarsym = qualified varsym

qconsym :: Parser String
qconsym = qualified consym

decimal :: Parser Integer
decimal = L.integer

octal :: Parser Integer
octal = L.octal

hexadecimal :: Parser Integer
hexadecimal = L.hexadecimal

integer :: Parser Integer
integer = decimal <|> hexadecimal <|> octal

float :: Parser Scientific
float = L.scientific

exponent :: Parser Integer
exponent = try (char' 'e') *> L.signed nospace decimal
  where nospace = return ()

char :: Parser Char
char = apostrophe *> (L.charLiteral >>= checkApostrophe) <* apostrophe
  where checkApostrophe = checkFail (== '\'')
                                    ["empty character literal"]

string :: Parser String
string = catMaybes <$> (doublequote *> manyTill ch doublequote)
  where ch = (Just <$> L.charLiteral)
         <|> (Nothing <$ Mp.string "\\&")
         <|> (Nothing <$ gap)

gap :: Parser ()
gap = Mp.char '\\' *> skipSome whitechar <* Mp.char '\\'


qualified :: Parser String -> Parser String
qualified p = (++) <$> option "" modid <*> p

apostrophe :: Parser Char
apostrophe = Mp.char '\''

doublequote :: Parser Char
doublequote = Mp.char '"'

checkFail :: (Eq a, Show a, Monad m) => (a -> Bool) -> [String] -> (a -> m a)
checkFail predicate failureMessage = check
  where check x = if predicate x
                  then fail $ intercalate (show x) failureMessage
                  else return x

checkReservedOp :: Monad m => String -> m String
checkReservedOp = checkFail (`elem` reservedops)
                            [ "reserved operator \""
                            , "\" cannot be an operator" ]


parens :: Parser a -> Parser a
parens = between (Mp.char '(') (Mp.char ')')

braces :: Parser a -> Parser a
braces = between (Mp.char '{') (Mp.char '}')

brackets :: Parser a -> Parser a
brackets = between (Mp.char '[') (Mp.char ']')

backticks :: Parser a -> Parser a
backticks = between (Mp.char '`') (Mp.char '`')

semicolon :: Parser Char
semicolon = Mp.char ';'

fileContents :: Parser a -> Parser a
fileContents p = do
  whitespace -- Layout rule?
  r <- p
  eof
  return r
