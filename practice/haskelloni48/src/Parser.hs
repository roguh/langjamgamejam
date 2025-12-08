{-# LANGUAGE OverloadedStrings #-}  

module Parser (
    readExpr,
    readExprFile
) where

import LispVal
import Text.Parsec
import Text.Parsec.Text
import qualified Text.Parsec.Token as Tok
import qualified Text.Parsec.Language as Lang
import qualified Data.Text as T
import Data.Functor.Identity (Identity)
import Control.Monad (mzero)

import Data.Functor (($>))
import Data.Char (digitToInt)


-- Monad protips:
-- m x $> f      -- apply function f to contents x
-- m z *> m x    -- overwrite contents of first monad z with x
-- m1 x <|> m2 y -- try to apply m1, if that fails try m2; for Parser.Parser this tries one parser, then another in case the first fails
-- f <$> m x     -- apply function f to contents x
-- TODO <?>
-- TODO <$
-- TODO <*

-- Tokenizer
lexer :: Tok.GenTokenParser T.Text () Identity
lexer = Tok.makeTokenParser $ Lang.emptyDef {
    Tok.commentStart = "{-"
    , Tok.commentEnd = "-}"
    , Tok.commentLine = "--"
    , Tok.opStart = mzero
    , Tok.opLetter = mzero
    , Tok.identStart = letter <|> oneOf "!$%&*/:<=>?^_~,"
    , Tok.identLetter = digit <|> letter <|> oneOf "!$%&*/:<=>?^_~+-.@"
    , Tok.reservedNames = ["__scope__"]
}

whitespace :: Parser ()
whitespace = Tok.whiteSpace lexer

parens :: Parser a -> Parser a
parens = Tok.parens lexer

braces :: Parser a -> Parser a
braces = Tok.braces lexer

lexeme :: Parser a -> Parser a
lexeme = Tok.lexeme lexer

quoted :: Parser a -> Parser a
quoted p = try (char '\'') *> p

identifier :: Parser T.Text
identifier = T.pack <$> (Tok.identifier lexer <|> specialIdentifier) <?> "identifier"
    where specialIdentifier = lexeme $ try $
            string "-" <|> string "+" <|> string "..."


type Radix = (Integer, Parser Char)

numberWithRadix :: Radix -> Parser Integer
numberWithRadix (base, baseDigit) = do
    -- A decimal possibly followed by an n
    digits <- many1 baseDigit
    let n = foldl' (\x d -> base * x + toInteger (digitToInt d)) 0 digits
    seq n (return n)

double :: Parser Double
double = Tok.float lexer
decimal :: Parser Integer
decimal = Tok.decimal lexer

sign :: Parser (Integer -> Integer)
sign =   char '-' $> negate
     <|> char '+' $> id
     <|> return id

intRadix :: Radix -> Parser Integer
intRadix r = sign <*> numberWithRadix r

textLiteral :: Parser T.Text
textLiteral = T.pack <$> Tok.stringLiteral lexer

nil :: Parser ()
nil = try (char '\'' *> string "()") *> return () <?> "nil"

hashVal :: Parser LispVal
hashVal = lexeme $ char '#'
    *> (char 't' $> Bool True
    <|> char 'f' $> Bool False
    <|> char 'b' *> (Number <$> intRadix (2, oneOf "01") <*> isBigInt)
    <|> char 'o' *> (Number <$> intRadix (8, octDigit) <*> isBigInt)
    <|> char 'd' *> (Number <$> intRadix (10, digit) <*> isBigInt)
    <|> char 'x' *> (Number <$> intRadix (16, hexDigit) <*> isBigInt)
    <|> oneOf "ei" *> fail "Unsupported: exactness"
    <|> Vector <$> parens manyLispVal
    <|> char '\\' *> fail "Unsupported: char")

isBigInt :: Parser Bool
isBigInt = try (char 'n') *> return True <|> return False

lispVal :: Parser LispVal
lispVal = hashVal
    <|> Dict <$> braces dictItems
    <|> Nil <$ nil
    -- The tty is essential to avoid eating input the decimal parser needs
    <|> Double <$> try double
    <|> Number <$> try (sign <*> decimal) <*> isBigInt
    <|> Atom  <$> identifier
    <|> String <$> textLiteral
    <|> _Quote <$> quoted lispVal
    <|> List <$> parens manyLispVal

manyLispVal :: Parser [LispVal]
manyLispVal = lispVal `sepBy` whitespace

notColon :: LispVal -> Bool
notColon (Atom ":") = False
notColon (Atom ",") = False
notColon _ = True

stripColon :: LispVal -> LispVal
stripColon (Atom s) = Atom . maybe s id $ T.stripSuffix ":" s
stripColon v = v

dictItems :: Parser [LispVal]
dictItems = map stripColon <$> filter notColon <$> lispVal `sepBy` whitespace 

_Quote :: LispVal -> LispVal
_Quote x = List [Atom "quote", x]

contents :: Parser a -> Parser a
contents p = whitespace *> lexeme p <* eof

readExpr :: T.Text -> Either ParseError LispVal
readExpr = parse (contents lispVal) "<stdin>"

readExprFile :: SourceName -> T.Text -> Either ParseError LispVal
readExprFile = parse (contents (List <$> manyLispVal))
