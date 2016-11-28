{-# LANGUAGE ExistentialQuantification #-}

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

  data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

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
  eval (List [Atom "if", pred' , conseq, alt]) =
    do result <- eval pred'
       case result of
            Bool False -> eval alt
            _          -> eval conseq
  eval (List (Atom func : args )) = mapM eval args >>= apply func
  eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

  apply :: String -> [LispVal] -> ThrowsError LispVal
  apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func)
                          ($ args)
                          (lookup func primitives)

  primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
  primitives = [("equal?"        , equal             ),
                ("eqv?"          , eqv               ),
                ("eq?"           , eqv               ),
                ("car"           , car               ),
                ("cdr"           , cdr               ),
                ("cons"          , cons              ),
                ("="             , numBoolBinop (==) ),
                ("<"             , numBoolBinop (<)  ),
                (">"             , numBoolBinop (>)  ),
                ("/="            , numBoolBinop (/=) ),
                (">="            , numBoolBinop (>=) ),
                ("<="            , numBoolBinop (<=) ),
                ("&&"            , boolBoolBinop (==)),
                ("||"            , boolBoolBinop (||)),
                ("string=?"      , strBoolBinop (==) ),
                ("string<?"      , strBoolBinop (<)  ),
                ("string>?"      , strBoolBinop (>)  ),
                ("string<=?"     , strBoolBinop (<=) ),
                ("string>=?"     , strBoolBinop (>=) ),
                ("+"             , numericBinop (+)  ),
                ("-"             , numericBinop (-)  ),
                ("*"             , numericBinop (*)  ),
                ("/"             , numericBinop div  ),
                ("mod"           , numericBinop mod  ),
                ("quotient"      , numericBinop quot ),
                ("remainder"     , numericBinop rem  ),
                ("symbol?"       , isSymbol          ),
                ("dlist?"        , isDottedList      ),
                ("list?"         , isList            ),
                ("character?"    , isCharacter       ),
                ("number?"       , isNumber          ),
                ("string?"       , isString          ),
                ("boolean?"      , isBoolean         ),
                ("symbol->string", symbolString      ),
                ("string->symbol", stringSymbol      )]


  unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
  unpackEquals arg1 arg2 (AnyUnpacker unpacker) =
               do unpacked1 <- unpacker arg1
                  unpacked2 <- unpacker arg2
                  return $ unpacked1 == unpacked2
            `catchError` (const $ return False)

  equal :: [LispVal] -> ThrowsError LispVal
  equal [arg1, arg2] = do
        primitiveEquals <- liftM or $ mapM (unpackEquals arg1 arg2)
                           [AnyUnpacker unpackNum,
                            AnyUnpacker unpackStr,
                            AnyUnpacker unpackBool]
        eqvEquals       <- eqv [arg1, arg2]
        return $ Bool (primitiveEquals || let (Bool x) = eqvEquals in x)
  equal badArgList = throwError $ NumArgs 2 badArgList

  -- | Returns the head of a list or dotted list
  -- | Contents of the Address part of Register number
  car :: [LispVal] -> ThrowsError LispVal
  car [List (x:_)]         = return x
  car [DottedList (x:_) _] = return x
  car [badArg]             = throwError $ TypeMismatch "pair" badArg
  car badArgList           = throwError $ NumArgs 1 badArgList

  -- | Returns the tail of a list or dotted list
  -- | Contents of the Decrement part of Register number
  cdr :: [LispVal] -> ThrowsError LispVal
  cdr [List (_:xs)]         = return $ List xs
  cdr [DottedList [_] x]    = return x
  cdr [DottedList (_:xs) x] = return $ DottedList xs x
  cdr [badArg]              = throwError $ TypeMismatch "pair" badArg
  cdr badArgList            = throwError $ NumArgs 1 badArgList

  -- | Concatenating two lists together
  cons :: [LispVal] -> ThrowsError LispVal
  cons [x, List []]          = return $ List [x]
  cons [x, List xs]          = return $ List (x:xs)
  cons [x, DottedList xs xd] = return $ DottedList (x:xs) xd
  cons [xs, ys]              = return $ DottedList [xs] ys
  cons badArgList            = throwError $ NumArgs 2 badArgList

  eqv :: [LispVal] -> ThrowsError LispVal
  eqv [Bool arg1      , Bool arg2]       = return $ Bool $ arg1 == arg2
  eqv [Number arg1    , Number arg2]     = return $ Bool $ arg1 == arg2
  eqv [String arg1    , String arg2]     = return $ Bool $ arg1 == arg2
  eqv [Atom arg1      , Atom arg2]       = return $ Bool $ arg1 == arg2
  eqv [DottedList xs x, DottedList ys y] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
  eqv [List arg1      , List arg2]       = return $ Bool $ length arg1 == length arg2 &&
                                                 (all eqvPair $ zip arg1 arg2)
      where eqvPair (x1, x2) = case eqv [x1, x2] of
                                  Left _           -> False
                                  Right (Bool val) -> val
  eqv [_, _]                                 = return $ Bool False
  eqv badArgList                             = throwError $ NumArgs 2 badArgList

  boolBinop :: (LispVal -> ThrowsError a) ->
               (a -> a -> Bool)           ->
               [LispVal]                  ->
               ThrowsError LispVal
  boolBinop unpacker op args = if length args /= 2
                                then throwError $ NumArgs 2 args
                                else do
                                  left  <- unpacker $ args !! 0
                                  right <- unpacker $ args !! 1
                                  return $ Bool $ left `op` right

  numBoolBinop :: (Integer -> Integer -> Bool) -> [LispVal] -> ThrowsError LispVal
  numBoolBinop = boolBinop unpackNum

  strBoolBinop :: (String -> String -> Bool) -> [LispVal] -> ThrowsError LispVal
  strBoolBinop = boolBinop unpackStr

  boolBoolBinop :: (Bool -> Bool -> Bool) -> [LispVal] -> ThrowsError LispVal
  boolBoolBinop = boolBinop unpackBool

  unpackStr :: LispVal -> ThrowsError String
  unpackStr (String s) = return s
  unpackStr (Number s) = return $ show s
  unpackStr (Bool   s) = return $ show s
  unpackStr notString  = throwError $ TypeMismatch "string" notString

  unpackBool :: LispVal -> ThrowsError Bool
  unpackBool (Bool b) = return b
  unpackBool notBool  = throwError $ TypeMismatch "bool" notBool

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
