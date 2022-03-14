module Parser where

import Control.Monad.Combinators.Expr (Operator (InfixL, InfixR, Postfix), makeExprParser)
import Control.Monad.Identity (Identity)
import Control.Monad.State
import Data.Functor (void)
import Data.List (elemIndex)
import Data.Map (fromList)
import Data.Text (Text, pack)
import Data.Void
import Error
import Term
import Text.Megaparsec hiding (ParseError, State)
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
parseVarName = some (letterChar <|> char '_')

parseBaseType :: Parser Type
parseBaseType = bool <|> unit <|> nat
  where
    bool = TyBool <$ symbol "Bool"
    unit = TyUnit <$ symbol "Unit"
    nat = TyNat <$ symbol "Nat"

parseTypeAbbreviationUse :: Parser Type
parseTypeAbbreviationUse = do
  n <- parseTypeVarName
  state <- get
  let tyAbbrs = filter (\(_, bind) -> case bind of TyAbbreviation _ -> True; _ -> False) state
  (_, TyAbbreviation ty) <- case elemIndex n (map fst tyAbbrs) of
    Nothing -> fail $ "The type " ++ n ++ " has not been bound"
    Just i -> pure $ tyAbbrs !! i
  -- in case we are using an abbreviation inside of a recursive type
  let scopeLength = length $ filter (\(_, bind) -> case bind of TyVarBind -> True; _ -> False) state
  return $ shift scopeLength ty

parseTypeVarName :: Parser String
parseTypeVarName = parseVarName

parseType :: Parser Type
parseType =
  makeExprParser
    ( lexeme $
        try parseTypeAbbreviationUse
          <|> try parseBaseType
          <|> parens parseType
    )
    [ [binaryR "->" TyArrow]
    ]

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

parseLet :: Parser Term
parseLet = do
  void $ symbol "let"
  varName <- lexeme parseVarName
  void $ symbol "="
  t1 <- lexeme parseNonApp
  void $ symbol "in"
  modify ((varName, NameBind) :)
  t2 <- lexeme parseTerm
  modify tail
  return $ TmLet varName t1 t2

parseRecordLabel :: Parser String
parseRecordLabel = some (letterChar <|> numberChar)

parseVariantLabel :: Parser String
parseVariantLabel = some letterChar

parseSeq :: Parser Term
parseSeq = do
  t1 <- parseNonApp
  void (symbol ";")
  t2 <- parseNonApp
  return $ App (Abs "_" TyUnit t2) t1

parseVar :: Parser Term
parseVar = do
  varName <- parseVarName
  state <- get
  getDeBruijnIndex varName state

parens :: Parser a -> Parser a
parens = between (symbol "(") (char ')')

parseZero, parseNum, parseSucc, parsePred, parseIsZero :: Parser Term
parseZero = TmZero <$ symbol "0"
parseNum = do
  n <- read <$> some numberChar
  return $ iterate TmSucc TmZero !! n
parseSucc = TmSucc <$> (symbol "succ" *> parseTerm)
parsePred = TmPred <$> (symbol "pred" *> parseTerm)
parseIsZero = TmIsZero <$> ((symbol "zero?" <|> symbol "iszero") *> parseTerm)

parseTypeAbbreviation :: Parser Term
parseTypeAbbreviation = do
  void $ symbol "type"
  n <- lexeme parseTypeVarName
  void $ symbol "="
  ty <- lexeme parseType
  void $ symbol ";"
  modify ((n, TyAbbreviation ty) :)
  parseTerm

parseNonApp :: Parser Term
parseNonApp = makeExprParser parsers operatorTable
  where
    parsers =
      parseBool
        <|> parseIf
        <|> try parseVar
        <|> parseZero
        <|> parseNum
        <|> parseSucc
        <|> parsePred
        <|> parseIsZero
        <|> parseAbs
        <|> parseLet
        <|> parseTypeAbbreviation
        <|> parens parseTerm

binaryL, binaryR :: String -> (a -> a -> a) -> Operator Parser a
binaryL name f = InfixL (f <$ symbol name)
binaryR name f = InfixR (f <$ symbol name)

operatorTable :: [[Operator Parser Term]]
operatorTable =
  [ [ binaryL ";" $ \t1 t2 -> App (Abs "_" TyUnit t2) t1
    ]
  ]

parseTerm :: Parser Term
parseTerm = chainl1 parseNonApp (App <$ space1)

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = p >>= rest
  where
    rest x = ((($ x) <$> op <*> p) >>= rest) <|> return x

parse :: String -> Either CompilerError Term
parse input = case result of
  Left e -> Left $ ParserError e
  Right (term, state) -> pure term
  where
    p = runStateT (parseTerm <* eof) []
    result = runParser p "" input
