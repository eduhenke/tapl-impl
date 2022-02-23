import Control.Monad (void)
import Control.Monad.Trans.State.Lazy (StateT (runStateT), runState)
import Data.Bifunctor
import Data.Functor
import Eval
import Parser
import System.IO (IOMode (ReadMode), hGetContents, openFile)
import Term
import Text.Megaparsec (MonadParsec (eof), runParser, runParserT)
import Typecheck

compile input = (parse input >>= (\term -> (\ty -> (term, ty)) <$> typeCheck term)) <&> first eval

main :: IO ()
main = do
  void (putStrLn "enter term:")
  code <- getLine
  -- handle <- openFile "test.lambda" ReadMode
  -- code <- hGetContents handle
  case compile code of
    Left e -> print e
    Right (term, ty) -> do
      putStrLn $ "Succesfully typechecked: " ++ show ty
      putStrLn $ "Term after evaluation: " ++ show (eval term)
