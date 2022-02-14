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
import Type
import Typecheck

type Parser = StateT Context (Parsec Void String)

-- space consumer, basis for lexing
sc :: Parser ()
sc =
  L.space
    space1 -- (2)
    (L.skipLineComment "//") -- (3)
    (L.skipBlockComment "/*" "*/") -- (4)

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

getDeBruijnIndex :: String -> Context -> Parser Term
getDeBruijnIndex v ctx = case elemIndex v (map fst ctx) of
  Nothing -> fail $ "The variable " ++ v ++ " has not been bound"
  Just n -> pure $ Var n (length ctx)

parseVarName :: Parser String
parseVarName = many letterChar

parseTypeArrow :: Parser Type
parseTypeArrow = do
  ty1 <- parseTypeBool
  void $ symbol "->"
  ty2 <- parseType
  return $ TyArrow ty1 ty2

parseTypeBool :: Parser Type
parseTypeBool = TyBool <$ symbol "Bool"

parseType :: Parser Type
parseType = lexeme $ try parseTypeArrow <|> parseTypeBool <|> parens parseType

parseBool :: Parser Term
parseBool = t <|> f <?> "bool"
  where
    t = TmTrue <$ string "true"
    f = TmFalse <$ string "false"

parseIf :: Parser Term
parseIf = do
  void $ symbol "if"
  cond <- lexeme parseNonApp
  void $ symbol "then"
  t <- lexeme parseNonApp
  void $ symbol "else"
  TmIf cond t <$> parseTerm

parseAbs :: Parser Term
parseAbs = do
  void (symbol "λ" <|> symbol "\\")
  varName <- lexeme parseVarName
  void $ symbol ":"
  ty <- parseType
  modify ((varName, VarBind ty) :)
  void $ symbol "."
  term <- parseTerm
  modify tail
  return $ Abs varName ty term

parseVar :: Parser Term
parseVar = do
  varName <- parseVarName
  state <- get
  getDeBruijnIndex varName state

parens :: Parser a -> Parser a
parens = between (char '(') (char ')')

parseNonApp :: Parser Term
parseNonApp =
  try parseBool
    <|> try parseIf
    <|> try parseAbs
    <|> try parseVar
    <|> try (parens parseTerm)

binary :: String -> (Term -> Term -> Term) -> Operator Parser Term
binary name f = InfixL (f <$ string name)

parseTerm :: Parser Term
parseTerm = makeExprParser parseNonApp [[binary " " App]]
