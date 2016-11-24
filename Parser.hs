module Parser where
  import Text.ParserCombinators.Parsec hiding (spaces)
  import Control.Monad
  import Numeric    (readFloat, readOct, readHex, readDec, readInt)
  import Data.Char  (digitToInt)
  import Data.Maybe (listToMaybe)

  -- | LispVal is a lisp value
  data LispVal = Atom String
               | List [LispVal]
               | DottedList [LispVal] LispVal
               -- | Quasiquote
               -- | Unquote
               | Number Integer
               | Float Float
               | String String
               | Bool Bool
               | Character Char

  instance Show LispVal where show = showVal

  -- | showVal function used in Show instance of LispVal
  showVal :: LispVal -> String
  showVal (String xs)      = "\"" ++ xs ++ "\""
  showVal (Atom name)      = name
  showVal (Number i)       = show i
  showVal (Bool True)      = "#t"
  showVal (Bool False)     = "#f"
  showVal (List xs)        = "(" ++ unwordsList xs ++ ")"
  showVal (DottedList h t) = "(" ++ unwordsList h ++ " . " ++ showVal t ++ ")"
  showVal (Character c)    = show c

  unwordsList :: [LispVal] -> String
  unwordsList = unwords . map showVal

  -- | Parses a single symbol, one of the characters of the String
  symbol :: Parser Char
  symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

  -- | Parses an expression that results in a LispVal
  -- | TODO add full numeric tower : http://www.schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-9.html#%_sec_6.2.1
  parseExpr :: Parser LispVal
  parseExpr = parseQuoted
           <|> (try parseChar <|> parseAtom)
           <|> parseString
           <|> parseNumber
           -- <|> parsefloat
          --  <|> parseQuasiquotation
           <|> do
             _ <- char '('
             x <- try parseList <|> parseDottedList
             _ <- char ')'
             return x

  -- | TODO fix sepBy is probably not the right solution
  parseQuasiquotation :: Parser LispVal
  parseQuasiquotation = do
    _     <- char '`'
    _     <- char '('
    qlist <- sepBy parseExpr quasiquatation
    _     <- char ')'
    return $ List qlist

  quasiquatation :: Parser [LispVal]
  quasiquatation = undefined

  -- | Parses the lists that made Lisp famous
  parseList :: Parser LispVal
  parseList = liftM List $ sepBy parseExpr spaces

  -- | Parses a dotted list, a list where the last value is a seperate field
  parseDottedList :: Parser LispVal
  parseDottedList = do
    h <- endBy parseExpr spaces
    t <- char '.' >> spaces >> parseExpr
    return $ DottedList h t

  -- | Parses a quoted list to add support for the single-quote sugar of Scheme
  parseQuoted :: Parser LispVal
  parseQuoted = do
    _ <- char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]

  -- | Parses a one or many spaces
  spaces :: Parser ()
  spaces = skipMany1 space

  -- | Parses a single Character
  -- | TODO add support for #\space #\newline etc..
  parseChar :: Parser LispVal
  parseChar = liftM Character $ char '#' >> char '\\' >> (letter <|>
                                                          symbol <|>
                                                          digit)

  -- | Parses a single String including escape characters
  parseString :: Parser LispVal
  parseString = do
    char '"'
    x <- many (noneOf "\"")
         <|> string "\\\""
         <|> string "\\n"
         <|> string "\\r"
         <|> string "\\t"
         <|> string "\\\\"
    char '"'
    return $ String x

  -- | Parses an Atom (a letter or symbol followed by any number of letters,
  -- | digits or symbols)
  parseAtom :: Parser LispVal
  parseAtom = do
    first <- letter <|> symbol
    rest  <- many (letter <|> digit <|> symbol)
    let atom = first:rest
    return $ case atom of
      "#t" -> Bool True
      "#f" -> Bool False
      _    -> Atom atom

  -- | Parses a number to the Number data type
  parseNumber :: Parser LispVal
  parseNumber = liftM (Number . read) (many1 digit)
    -- many1 digit >>= (return . Number . read)
    -- do
    -- digits <- many1 digit
    -- return $ Number (read digits)

  -- | TODO fix throws errors at readRadixVal
  parseNumberRadix :: Parser LispVal
  parseNumberRadix = do
    radix  <- do
       _      <- char '#'
       radix' <- oneOf "bodx"
       _      <- space
       return radix'
    number <- many1 digit
    return $ case radix of
      'b' -> readRadixVal (readInt 2 (`elem` "01") digitToInt) Number number
      'h' -> readRadixVal readHex Number number
      'o' -> readRadixVal readOct Number number
      'd' -> readRadixVal readDec Number number

  readRadixVal :: (String -> [(a, String)]) -> (a -> LispVal) -> String -> LispVal
  readRadixVal f g xs = case readRadix f xs of
    Just a  -> g a
    Nothing -> error "Parse error on bin, hex, oct or decimal"

  readRadix :: (String -> [(a, String)]) -> String -> Maybe a
  readRadix f = fmap fst . listToMaybe . f

  -- | TODO needs fixing fails at parsereadRadixVal
  parseFloat :: Parser LispVal
  parseFloat = do
    before <- many1 digit
    dot    <- char '.'
    after  <- many1 digit
    return (readRadixVal readFloat Float (before ++ [dot] ++ after))
