import Control.Monad (void)
import Control.Monad.Trans.State.Lazy (StateT (runStateT), runState)
import Eval
import Parser
import Text.Megaparsec (MonadParsec (eof), runParser, runParserT)
import Typecheck

main :: IO ()
main = do
  void (putStrLn "enter term:")
  word <- getLine
  let p = runStateT (parseTerm <* eof) []
      result = runParser p "" word
  case result of
    Left e -> print e
    Right (term, state) -> do
      putStrLn $ "Term before evaluation: " ++ show term
      case typeOf [] term of
        Right ty -> do
          putStrLn $ "Succesfully typechecked: " ++ show ty
          putStrLn $ "Term after evaluation: " ++ show (eval term)
        Left err -> putStrLn $ "Expression does not typecheck: " ++ show err
