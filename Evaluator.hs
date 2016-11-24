module Evaluator where
  import Parser
  import Control.Monad.Except
  import Text.ParserCombinators.Parsec (ParseError)

  data LispError = NumArgs Integer [LispVal]
                 | TypeMismatch String LispVal
                 | ParseErr ParseError
                 | BadSpecialForm String LispVal
                 | NotFunction String String
                 | UnboundVar String String
                 | Default String

  showError :: LispError -> String
  showError (UnboundVar message varname)  = message ++ ": " ++ varname
  showError (BadSpecialForm message form) = message ++ ": " ++ show form
  showError (NotFunction message func)    = message ++ ": " ++ show func
  showError (ParseErr parseErr)             = "Parse error at " ++ show parseErr
  showError (Default message)             = "Error: " ++ message
  showError (NumArgs expected found)      = "Expected " ++ show expected
                          ++ " args; found values " ++ unwordsList found
  showError (TypeMismatch expected found)  = "Invalid type: expected " ++ expected
                          ++ ", found " ++ show found

  instance Show LispError where show = showError

  type ThrowsError = Either LispError

  trapError :: (Show e, MonadError e m) => m String -> m String
  trapError action = catchError action (return . show)

  -- | Only used after trap error (fail fast)
  extractValue :: ThrowsError a -> a
  extractValue (Right val) = val

  eval :: LispVal -> ThrowsError LispVal
  eval val@(String _)             = return val
  eval val@(Number _)             = return val
  eval val@(Bool   _)             = return val
  eval val@(Float  _)             = return val
  eval val@(Atom "#f")            = return val
  eval val@(Atom "#t")            = return val
  eval (List [Atom "quote", val]) = return val
  eval (List (Atom func : args )) = mapM eval args >>= apply func
  eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

  apply :: String -> [LispVal] -> ThrowsError LispVal
  apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func)
                          ($ args)
                          (lookup func primitives)

  primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
  primitives = [("+"             , numericBinop (+) ),
                ("-"             , numericBinop (-) ),
                ("*"             , numericBinop (*) ),
                ("/"             , numericBinop div ),
                ("mod"           , numericBinop mod ),
                ("quotient"      , numericBinop quot),
                ("remainder"     , numericBinop rem ),
                ("symbol?"       , isSymbol         ),
                ("dlist?"        , isDottedList     ),
                ("list?"         , isList           ),
                ("character?"    , isCharacter      ),
                ("number?"       , isNumber         ),
                ("string?"       , isString         ),
                ("boolean?"      , isBoolean        ),
                ("symbol->string", symbolString     ),
                ("string->symbol", stringSymbol     )]

  numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
  numericBinop _ []            = throwError $ NumArgs 2 []
  numericBinop _ singleVal@[_] = throwError $ NumArgs 2 singleVal
  numericBinop op params       = liftM (Number . foldl1 op) (mapM unpackNum params)

  unpackNum :: LispVal -> ThrowsError Integer
  unpackNum (Number n) = return n
  unpackNum (String n) = let parsed = reads n in
                            if null parsed
                              then throwError $ TypeMismatch "number" $ String n
                              else return $ fst $ head parsed
  unpackNum (List [n]) = unpackNum n
  unpackNum notNum     = throwError $ TypeMismatch "number" notNum

  isDottedList :: [LispVal] -> ThrowsError LispVal
  isDottedList [DottedList _ _] = return $ Bool True
  isDottedList _                = return $ Bool False

  isList :: [LispVal] -> ThrowsError LispVal
  isList [List _] = return $ Bool True
  isList _        = return $ Bool False

  isCharacter :: [LispVal] -> ThrowsError LispVal
  isCharacter [Character _] = return $ Bool True
  isCharacter _             = return $ Bool False

  isNumber :: [LispVal] -> ThrowsError LispVal
  isNumber [Number _] = return $ Bool True
  isNumber _          = return $ Bool False

  isString :: [LispVal] -> ThrowsError LispVal
  isString [String _] = return $ Bool True
  isString _          = return $ Bool False

  isBoolean :: [LispVal] -> ThrowsError LispVal
  isBoolean [Bool _] = return $ Bool True
  isBoolean _        = return $ Bool False

  isSymbol :: [LispVal] -> ThrowsError LispVal
  isSymbol [List (Atom _ : _)] = return $ Bool True
  isSymbol [Atom _ ]           = return $ Bool True
  isSymbol _                   = return $ Bool False

  symbolString :: [LispVal] -> ThrowsError LispVal
  symbolString [Atom n] = return $ String n
  symbolString [x]      = throwError $ TypeMismatch "symbol" x
  symbolString xs       = throwError $ NumArgs 1 xs

  stringSymbol :: [LispVal] -> ThrowsError LispVal
  stringSymbol [String s] = return $ Atom s
  stringSymbol [x]        = throwError $ TypeMismatch "string" x
  stringSymbol xs         = throwError $ NumArgs 1 xs
