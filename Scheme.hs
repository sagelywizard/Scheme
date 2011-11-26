import Text.ParserCombinators.Parsec hiding (spaces)
import Text.Parsec.Prim (ParsecT)
import Data.Functor.Identity as DFI
import Control.Monad
import Numeric

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

lispCharacter :: ParsecT [Char] u DFI.Identity Char
lispCharacter = try (noneOf ['\\', '\"'])
            <|> try (string "\\\\" >> return '\\')
            <|> try (string "\\\"" >> return '"')
            <|> try (string "\\n" >> return '\n')

parseString :: Parser LispVal
parseString = do
    char '"'
    x <- many lispCharacter
    char '"'
    return $ String x

parseNumber :: Parser LispVal
parseNumber = do
    base <- try (string "0x")
        <|> try (string "0o")
        <|> return ""
    rest <- case base of
          "0x" -> (many1 $ oneOf "0123456789abcdef")
          "0o" -> (many1 $ oneOf "01234567")
          _    -> (many1 $ oneOf "0123456789")
    return $ Number . read $ rest

parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol
               rest <- many (letter <|> digit <|> symbol)
               let atom = first:rest
               return $ case atom of
                               "#t" -> Bool True
                               "#f" -> Bool False
                               _    -> Atom atom

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
    head <- endBy parseExpr spaces
    tail <- char '.' >> spaces >> parseExpr
    return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]

parseExpr :: Parser LispVal
parseExpr = parseString
        <|> parseNumber
        <|> parseAtom
        <|> parseQuoted
        <|> do char '('
               x <- try parseList <|> parseDottedList
               char ')'
               return x

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value"

main :: IO ()
main = do putStrLn "Ready!"
          arg <- getLine
          putStrLn ""
          putStrLn (readExpr $ arg)
