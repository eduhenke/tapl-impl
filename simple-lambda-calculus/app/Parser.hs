module Parser where

import Control.Monad.Combinators.Expr (Operator (InfixL), makeExprParser)
import Data.Functor (void)
import Data.Text (Text, pack)
import Data.Void
import Term
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

parseVarName :: Parser String
parseVarName = many letterChar

parseAbs :: Parser Term
parseAbs = do
  void $ char 'λ'
  varName <- parseVarName
  void $ char '.'
  Abs varName <$> parseTerm

parseVar :: Parser Term
parseVar = Var <$> parseVarName

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
