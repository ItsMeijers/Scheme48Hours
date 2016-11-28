module Main where
  import System.Environment
  import Parser
  import Evaluator
  import Control.Monad.Except (throwError)
  import Text.ParserCombinators.Parsec (parse)
  import Control.Monad
  import System.IO

  main :: IO ()
  main = do args <- getArgs
            case length args of
              0 -> runRepl
              1 -> evalAndPrint $ args !! 0
              _ -> putStrLn "Program takes only 0 or 1 argument"

  -- | Reads an expression to a value
  readExpr :: String -> ThrowsError LispVal
  readExpr input = case parse parseExpr "lisp" input of
    Left  err -> throwError $ ParseErr err
    Right val -> return val

  flushStr :: String -> IO ()
  flushStr str = putStr str >> hFlush stdout

  readPrompt :: String -> IO String
  readPrompt prompt = flushStr prompt >> getLine

  evalString :: String -> IO String
  evalString expr = return $ extractValue $ trapError (liftM show $ readExpr expr >>= eval)

  evalAndPrint :: String -> IO ()
  evalAndPrint expr = evalString expr >>= putStrLn

  until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
  until_ pred' prompt action = do
    result <- prompt
    unless (pred' result) $ action result >> until_ pred' prompt action

  runRepl :: IO ()
  runRepl = until_ (== "quit") (readPrompt "Lisp>>> ") evalAndPrint
