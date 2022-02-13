import Control.Monad (void)
import Control.Monad.Trans.State.Lazy (StateT (runStateT), runState)
import Eval
import Parser
import Text.Megaparsec (MonadParsec (eof), runParser, runParserT)

main :: IO ()
main = do
  void (putStrLn "enter term:")
  word <- getLine
  let p = runStateT parseTerm []
      result = runParser p "" word
  case result of
    Left e -> print e
    Right (term, state) -> do
      putStrLn $ "Term before evaluation: " ++ show term
      putStrLn $ "Term after evaluation: " ++ show (eval term)
