module SchemeParser (LispVal(..), ThrowsError(..), Env(..), IOThrowsError(..), LispError(..), parseExpr, spaces) where

import Text.ParserCombinators.Parsec hiding (spaces)
import qualified Text.ParserCombinators.Parsec as P
import Numeric (readFloat, readOct, readHex)
import Data.Ratio
import Data.Complex
import Data.Array
import Control.Monad
import Control.Monad.Error
import Data.IORef
import System.IO (Handle (..))

data LispVal = Atom String
            | List [LispVal]
            | DottedList [LispVal] LispVal
            | Vector (Array Int LispVal)
            | Number Integer
            | Float Double
            | Ratio Rational
            | Complex (Complex Double)
            | String String
            | Bool Bool
            | Character Char
            | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
            | IOFunc ([LispVal] -> IOThrowsError LispVal)
            | Func { params :: [String], vararg :: (Maybe String),
                     body :: [LispVal], closure :: Env }
            | Port Handle

data LispError = NumArgs Integer [LispVal]
            | TypeMismatch String LispVal
            | Parser ParseError
            | BadSpecialForm String LispVal
            | NotFunction String String
            | UnboundVar String String
            | Default String

type ThrowsError = Either LispError
type Env = IORef [(String, IORef LispVal)]
type IOThrowsError = ErrorT LispError IO

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

escapedChars :: Parser Char
escapedChars = do char '\\'
                  x <- oneOf "\\\"nrt"
                  return $ case x of
                    '\\' -> x
                    '"'  -> x
                    'n'  -> '\n'
                    'r'  -> '\r'
                    't'  -> '\t'

parseString :: Parser LispVal
parseString =  do
    char '"'
    x <- many $ escapedChars <|> noneOf "\"\\"
    char '"'
    return $ String x

parseAtom :: Parser LispVal
parseAtom = do
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    let atom = first:rest
    return $ case atom of
        "#t" -> Bool True
        "#f" -> Bool False
        _    -> Atom atom

parseDecimal1 :: Parser LispVal
parseDecimal1 = many1 digit >>= (return . Number . read)

parseDecimal2 :: Parser LispVal
parseDecimal2 = do
    try $ string "#d"
    x <- many1 digit
    (return . Number . read) x

parseDecimal :: Parser LispVal
parseDecimal = parseDecimal1 <|> parseDecimal2

parseHex :: Parser LispVal
parseHex = do
    try $ string "#x"
    x <- many1 hexDigit
    return $ Number (hex2dig x)

parseOct :: Parser LispVal
parseOct = do
    try $ string "#o"
    x <- many1 octDigit
    return $ Number (oct2dig x)

parseBin :: Parser LispVal
parseBin = do
    try $ string "#b"
    x <- many1 (oneOf "10")
    return $ Number (bin2dig x)

oct2dig x = fst $ readOct x !! 0
hex2dig x = fst $ readHex x !! 0
bin2dig  = bin2dig' 0
bin2dig' digint "" = digint
bin2dig' digint (x:xs) = let old = 2 * digint + (if x == '0' then 0 else 1) in
                         bin2dig' old xs

parseNumber :: Parser LispVal
parseNumber = parseDecimal <|> parseHex
    <|> parseOct <|> parseBin

parseFloat :: Parser LispVal
parseFloat = do
    x <- many1 digit
    char '.'
    y <- many1 digit
    return $ Float (fst.head $ readFloat (x ++ "." ++ y))

parseRatio :: Parser LispVal
parseRatio = do
    x <- many1 digit
    char '/'
    y <- many1 digit
    return $ Ratio ((read x) % (read y))

parseComplex :: Parser LispVal
parseComplex = do
    x <- (try parseFloat <|> parseDecimal)
    char '+'
    y <- (try parseFloat <|> parseDecimal)
    char 'i'
    return $ Complex (toDouble x :+ toDouble y)
    where
        toDouble (Float f) = realToFrac f
        toDouble (Number n) = fromIntegral n

parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]

parseQuasiQuoted :: Parser LispVal
parseQuasiQuoted = do
    char '`'
    x <- parseExpr
    return $ List [Atom "quasiquoted", x]

parseUnQuote :: Parser LispVal
parseUnQuote = do
    char ','
    x <- parseExpr
    return $ List [Atom "unqoute", x]

parseBool :: Parser LispVal
parseBool = do
    char '#'
    (char 't' >> return (Bool True)) <|> (char 'f' >> return (Bool False))

parseCharacter :: Parser LispVal
parseCharacter = do
    try $ string "#\\"
    value <- try (string "newline" <|> string "space")
         <|> do { x <- anyChar; notFollowedBy alphaNum ; return [x] }
    return $ Character $ case value of
        "space"     -> ' '
        "newline"   -> '\n'
        otherwise   -> (value !! 0)

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
    head <- endBy parseExpr spaces
    tail <- char '.' >> spaces >> parseExpr
    return $ DottedList head tail

parseVector :: Parser LispVal
parseVector = do
    arrayValues <- sepBy parseExpr spaces
    return $ Vector (listArray (0,(length arrayValues - 1)) arrayValues)

parseExpr :: Parser LispVal
parseExpr = parseAtom
         <|> parseString
         <|> try parseComplex
         <|> try parseFloat
         <|> try parseRatio
         <|> try parseNumber
         <|> try parseBool
         <|> try parseCharacter
         <|> parseQuoted
         <|> parseQuasiQuoted
         <|> parseUnQuote
         <|> try (do
             string "#("
             x <- parseVector
             char ')'
             return x)
         <|> do char '('
                x <- try parseList <|> parseDottedList
                char ')'
                return x
