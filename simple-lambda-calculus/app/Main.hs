import Control.Monad (void)
import Eval
import Parser
import Text.Megaparsec (MonadParsec (eof), runParser)

main :: IO ()
main = do
  void (putStrLn "enter term:")
  word <- getLine
  let term = runParser (parseTerm <* eof) "" word
  case term of
    Left e -> print e
    Right t -> do
      print t
      print $ eval t
