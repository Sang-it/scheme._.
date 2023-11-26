module Parser where

import Control.Applicative ((<|>))
import Internal (Primitive (Atom, Bool, DottedList, List, Number, String))
import qualified Text.ParserCombinators.Parsec as P

symbol :: P.Parser Char
symbol = P.oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: P.Parser ()
spaces = P.skipMany1 P.space

parseString :: P.Parser Primitive
parseString = do
    P.char '"'
    x <- P.many (P.noneOf "\"")
    P.char '"'
    return $ String x

parseAtom :: P.Parser Primitive
parseAtom = do
    first <- P.letter <|> symbol
    rest <- P.many $ P.letter <|> P.digit <|> symbol
    let atom = first : rest
    return $
        case atom of
            "#t" -> Bool True
            "#f" -> Bool False
            _ -> Atom atom

parseNumber :: P.Parser Primitive
parseNumber = Number . read <$> P.many1 P.digit

parseList :: P.Parser Primitive
parseList = List <$> P.sepBy parseExpression spaces

parseDottedList :: P.Parser Primitive
parseDottedList = do
    head <- P.endBy parseExpression spaces
    tail <- P.char '.' >> spaces >> parseExpression
    return $ DottedList head tail

parseQuoted :: P.Parser Primitive
parseQuoted = do
    P.char '\''
    x <- parseExpression
    return $ List [Atom "quote", x]

parseExpression :: P.Parser Primitive
parseExpression =
    parseAtom
        <|> parseString
        <|> parseNumber
        <|> parseQuoted
        <|> do
            P.char '('
            x <- P.try parseList <|> parseDottedList
            P.char ')'
            return x
