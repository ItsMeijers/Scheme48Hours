module Evaluator where
  import Parser
  import Data.Char (toLower)

  eval :: LispVal -> LispVal
  eval val@(String _)  = val
  eval val@(Number _)  = val
  eval val@(Bool   _)  = val
  eval val@(Float  _)  = val
  eval val@(Atom "#f") = val
  eval val@(Atom "#t") = val
  eval (List [Atom "quote", val])              = val
  eval (List [Atom "boolean?", Bool _])        = Bool True
  eval (List [Atom "boolean?", _ ])            = Bool False
  eval (List [Atom "string?", String _])       = Bool True
  eval (List [Atom "string?", _ ])             = Bool False
  eval (List [Atom "number?", Number _])       = Bool True
  eval (List [Atom "number?", _ ])             = Bool False
  eval (List [Atom "character?", Character _]) = Bool True
  eval (List [Atom "character?", _ ])          = Bool False
  eval (List [Atom "list?", List _])           = Bool True
  eval (List [Atom "list?", _ ])               = Bool False
  eval (List [Atom "dlist?", DottedList _ _])  = Bool True
  eval (List [Atom "dlist?", _ ])              = Bool False
  eval (List [Atom "symbol->string", List [Atom "quote", Atom name]]) =
    String name
  eval (List [Atom "string->symbol", String s]) = Atom s
  eval (List [Atom "symbol?", Atom _])         = Bool True
  eval (List [Atom "symbol?", List (Atom _ : _)]) = Bool True
  eval (List [Atom "symbol?", _     ])         = Bool False
  eval (List (Atom func : args )) = apply func $ map eval args

  apply :: String -> [LispVal] -> LispVal
  apply func args = maybe (Bool False) ($ args) $ lookup func primitives

  primitives :: [(String, [LispVal] -> LispVal)]
  primitives = [("+"         , numericBinop (+) ),
                ("-"         , numericBinop (-) ),
                ("*"         , numericBinop (*) ),
                ("/"         , numericBinop div ),
                ("mod"       , numericBinop mod ),
                ("quotient"  , numericBinop quot),
                ("remainder" , numericBinop rem )]

  numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
  numericBinop op params = Number $ foldl1 op $ map unpackNum params

  unpackNum :: LispVal -> Integer
  unpackNum (Number n) = n
  unpackNum _          = 0
