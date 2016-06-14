--
-- S-expression parser.
--

-- had to do cabal install parsec to get this to run:
-- http://stackoverflow.com/questions/9058914/cant-find-parsec-modules-in-ghci

module Sexpr where

import Text.Parsec
import Text.Parsec.String

----------------------------------------------------------------------
-- Datatypes.
----------------------------------------------------------------------
-- added StringA constructor
data Atom =
    BoolA  Bool
  | IntA   Integer
  | FloatA Double
  | IdA    String  -- identifier
  | StringA String
  deriving (Show)

data Sexpr =
    AtomS Atom
  | ListS [Sexpr]
  deriving (Show)

----------------------------------------------------------------------
-- Parsers.
----------------------------------------------------------------------

-- no change, part of Atom data type
parseBool :: Parser Bool
parseBool =
  char '#' >>
  ((char 'f' >> return False)
   <|> (char 't' >> return True))
  <?> "boolean"

-- no change, part of the Atom data type
parseInt :: Parser Integer
parseInt = do
  sign <- option "" (string "-")
  digits <- many1 digit  -- many1 (oneOf "0123456789")
  return (read (sign ++ digits) :: Integer)
  <?> "integer"

parseExp :: Parser [Char]
parseExp = do
         exp <- oneOf "eE"
         sign <- option "" (string "+" <|> string "-")
         -- sign <- oneOf "+-" -- I tried option, but didn't know how to nest them such that they didn't override
         digits <- many1 digit
         return ([exp] ++ sign ++ digits)

-- modified to make it parse floating-point #s with exponents
parseFloat :: Parser Double
parseFloat = do
  ints <- parseInt
  char '.'
  f <- many1 digit
  exp <- option "" parseExp 
  -- return (read (ints ++ "." ++ f ++ exp) :: Double)
  return (read (show ints ++ "." ++ f ++ exp) :: Double)
  <?> "floating-point number"

-- no change
parseId :: Parser String
parseId = many1 (alphaNum <|> oneOf "_+-*/=?!") <?> "identifier"

-- added parseString, before inserting inside parseAtom
parseString :: Parser String
parseString = do
            char '\"'
            str <- many (noneOf "\"") -- idk why double quotes, it gave me error on single..
            char '\"'
            return str
            <?> "string"

-- added parseString
parseAtom :: Parser Atom
parseAtom =
  (parseBool >>= return . BoolA)
  <|> try (parseFloat >>= return . FloatA)
  <|> try (parseInt >>= return . IntA)
  <|> try (parseString >>= return . StringA) 
  <|> (parseId >>= return . IdA)
  <?> "atom"

-- no change
parseComment :: Parser ()
parseComment = do
  char ';'
  many (noneOf "\n")
  char '\n'
  return ()

-- no change
parseWhitespace :: Parser ()
parseWhitespace = many1 space >> return ()

-- no change
-- Parse a separator (whitespace or comment).
parseSep :: Parser ()
parseSep = 
  many1 (parseComment <|> parseWhitespace) >> return ()
  <?> "separator"


-- helper function for parseList
parseDelim :: Char -> Char -> Parser [Sexpr]
parseDelim c1 c2 = do
           char c1
           optional parseSep
           ss <- parseSexpr `sepEndBy` parseSep
           char c2
           return ss
           <?> "de-delimited S-expressions"

-- modified in Q2 and explained in Q3
-- Parse a list of S-expressions, delimited by parentheses,
-- separated by whitespace/comments.
parseList :: Parser [Sexpr]
parseList = do
  parseDelim '(' ')'
  <|> parseDelim '[' ']'
  <|> parseDelim '{' '}'
  <?> "list of S-expressions"
-- Question 3: You don't need to do a try because of the <|> operator, which is an or statement. This will try itself for all three possibilitites, and match on one.

-- modified, cannot keep as a QuotesS Sexpr, instead followed the QuoteS constructor
-- ListS [AtomS (IdA "quote"), AtomS (IdA "a")], returning a list
-- Parse a quoted expression.
parseQuote :: Parser Sexpr
parseQuote = do
           char '\''
           x <- parseSexpr
           return (ListS [AtomS (IdA "quote"), x])
  <?> "quoted S-expression"

-- modified, such that we can write it in list (ListS) form
-- Parse a single S-expressions.
parseSexpr :: Parser Sexpr
parseSexpr = 
  (parseAtom >>= return . AtomS)
  <|> (parseList >>= return . ListS)
  <|> parseQuote
  <?> "S-expression"

-- no change
-- Parse a series of Sexprs from a string representing the entire contents of a
-- file.
parseSexprsFromFile :: Parser [Sexpr]
parseSexprsFromFile = do
  optional parseSep
  ss <- parseSexpr `sepEndBy` parseSep
  eof
  return ss
  <?> "file of S-expressions"

----------------------------------------------------------------------
-- Pretty-printer.
----------------------------------------------------------------------

indent :: Int -> String
indent i = replicate i ' '

-- modified, removed QuoteS
-- Pretty-print a Sexpr.
ppSexpr :: Int -> Sexpr -> String
ppSexpr i (AtomS a)  = indent i ++ show a
ppSexpr i (ListS ss) = 
  indent i
  ++ "ListS[\n" 
  ++ concatMap (\s -> ppSexpr (i + 2) s ++ "\n") ss
  ++ indent i ++ "]"

-- no change
-- Parse all expressions in a file and run the pretty-printer on them.
runPpSexpr :: FilePath -> IO ()
runPpSexpr f = do
  p <- parseFromFile parseSexprsFromFile f
  case p of
    Left err -> putStrLn $ "ERROR: " ++ show err
    Right ss -> 
      mapM_ (\s -> do
        putStrLn (ppSexpr 0 s)
        putStrLn "") ss

----------------------------------------------------------------------
-- Tests.
----------------------------------------------------------------------

test :: IO ()
test = runPpSexpr "test.scm"

