module Main where
  import System.Environment
  import Parser
  import Evaluator
  import Control.Monad.Except (throwError)
  import Text.ParserCombinators.Parsec (parse)
  import Control.Monad

  main :: IO ()
  main = do
    args <- getArgs
    evaled <- return $ liftM show $ readExpr (head args) >>= eval
    putStrLn $ extractValue $ trapError evaled

  -- | Reads an expression to a value
  readExpr :: String -> ThrowsError LispVal
  readExpr input = case parse parseExpr "lisp" input of
    Left  err -> throwError $ ParseErr err
    Right val -> return val
