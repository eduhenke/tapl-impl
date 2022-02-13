module Parser where

import Control.Monad.Combinators.Expr (Operator (InfixL), makeExprParser)
import Control.Monad.Identity (Identity)
import Control.Monad.State
import Data.Functor (void)
import Data.List (elemIndex)
import Data.Text (Text, pack)
import Data.Void
import Term
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Context = [String]

type Parser = StateT Context (Parsec Void String)

getDeBruijnIndex :: String -> Context -> Parser Term
getDeBruijnIndex v ctx = case elemIndex v ctx of
  Nothing -> fail $ "The variable " ++ v ++ " has not been bound"
  Just n -> pure $ Var n (length ctx)

parseVarName :: Parser String
parseVarName = many letterChar

parseAbs :: Parser Term
parseAbs = do
  void (char 'λ' <|> char '\\')
  varName <- parseVarName
  modify (varName :)
  void $ char '.'
  term <- parseTerm
  modify tail
  return $ Abs varName term

parseVar :: Parser Term
parseVar = do
  varName <- parseVarName
  state <- get
  getDeBruijnIndex varName state

parens :: Parser a -> Parser a
parens = between (char '(') (char ')')

parseNonApp :: Parser Term
parseNonApp =
  parens parseTerm
    <|> parseAbs
    <|> parseVar

binary :: String -> (Term -> Term -> Term) -> Operator Parser Term
binary name f = InfixL (f <$ string name)

parseTerm :: Parser Term
parseTerm = makeExprParser parseNonApp [[binary " " App]]
