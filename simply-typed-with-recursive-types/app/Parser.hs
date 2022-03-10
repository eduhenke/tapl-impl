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

parseRecordType :: Parser Type
parseRecordType =
  between
    (symbol "{")
    (symbol "}")
    (TyRecord . fromList <$> (try parseRecordElements <|> parseTupleElements))
  where
    parseRecordElement = do
      label <- parseRecordLabel
      void $ symbol ":"
      t <- parseType
      return (label, t)
    parseTupleElements = zip (map show [1 ..]) <$> sepBy parseType (symbol ",")
    parseRecordElements = sepBy parseRecordElement (symbol ",")

parseVariantType :: Parser Type
parseVariantType =
  between
    (symbol "<")
    (symbol ">")
    (TyVariant . fromList <$> sepBy parseVariantElement (symbol ","))
  where
    parseVariantElement = do
      label <- parseVariantLabel
      void $ symbol ":"
      t <- parseType
      return (label, t)

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

parseTypeVar, parseRecursiveType :: Parser Type
parseTypeVar = do
  x <- parseTypeVarName
  state <- get
  let tyVars = filter (\(_, bind) -> case bind of TyVarBind -> True; _ -> False) state
  case elemIndex x (map fst tyVars) of
    Nothing -> fail $ "The type variable" ++ x ++ " has not been bound"
    Just i -> pure $ TyVar i (length tyVars)
parseRecursiveType = do
  void $ symbol "µ"
  x <- parseTypeVarName
  void $ symbol "."
  modify ((x, TyVarBind) :)
  body <- parseType
  modify tail
  pure $ TyRecursive x body

parseType :: Parser Type
parseType =
  makeExprParser
    ( lexeme $
        try parseTypeAbbreviationUse
          <|> try parseBaseType
          <|> parseRecordType
          <|> parseVariantType
          <|> parseRecursiveType
          <|> parseTypeVar
          <|> parens parseType
    )
    [ [binaryR "->" TyArrow]
    ]

parseBool :: Parser Term
parseBool = t <|> f <?> "bool"
  where
    t = TmTrue <$ string "true"
    f = TmFalse <$ string "false"

parseUnit :: Parser Term
parseUnit = TmUnit <$ string "unit"

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

parseRecord :: Parser Term
parseRecord =
  between
    (symbol "{")
    (symbol "}")
    (TmRecord . fromList <$> (try parseRecordElements <|> parseTupleElements))
  where
    parseRecordElement = do
      label <- parseRecordLabel
      void $ symbol "="
      t <- parseTerm
      return (label, t)
    parseTupleElements = zip (map show [1 ..]) <$> sepBy parseTerm (symbol ",")
    parseRecordElements = sepBy parseRecordElement (symbol ",")

parseVariant :: Parser Term
parseVariant = do
  (label, t) <- parseVariantNoAscription
  void $ symbol "as"
  ty <- parseType
  return $ TmVariant t label ty
  where
    parseVariantElement = do
      label <- parseVariantLabel
      void $ symbol "="
      t <- lexeme parseTerm
      return (label, t)
    parseVariantNoAscription = between (symbol "<") (symbol ">") parseVariantElement

parseCase :: Parser Term
parseCase = do
  void $ symbol "case"
  t <- lexeme parseNonApp
  void $ symbol "of"
  TmCase t . fromList <$> sepBy parseBranch (symbol ",")
  where
    parseBranch = do
      void $ symbol "<"
      label <- parseVariantLabel
      void $ symbol "="
      x <- parseVarName
      void $ symbol ">"
      void $ symbol "=>"
      modify ((x, NameBind) :)
      t <- parseTerm
      modify tail
      return (label, (x, t))

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

parseFold :: Parser Term
parseFold = do
  void $ symbol "fold"
  void $ symbol "["
  ty <- parseType
  void $ symbol "]"
  t <- parseTerm
  pure $ TmFold t ty

parseUnfold :: Parser Term
parseUnfold = do
  void $ symbol "unfold"
  void $ symbol "["
  ty <- parseType
  void $ symbol "]"
  t <- parseTerm
  pure $ TmUnfold t ty

parseNonApp :: Parser Term
parseNonApp = makeExprParser parsers operatorTable
  where
    parsers =
      parseUnit
        <|> parseBool
        <|> parseIf
        <|> try parseVar
        <|> parseZero
        <|> parseNum
        <|> parseSucc
        <|> parsePred
        <|> parseIsZero
        <|> parseAbs
        <|> parseLet
        <|> parseRecord
        <|> parseVariant
        <|> parseCase
        <|> parseTypeAbbreviation
        <|> parseFold
        <|> parseUnfold
        <|> parens parseTerm

binaryL, binaryR :: String -> (a -> a -> a) -> Operator Parser a
binaryL name f = InfixL (f <$ symbol name)
binaryR name f = InfixR (f <$ symbol name)

operatorTable :: [[Operator Parser Term]]
operatorTable =
  [ [ binaryL ";" $ \t1 t2 -> App (Abs "_" TyUnit t2) t1
    ],
    [ Postfix $ (\ty term -> TmAscription term ty) <$> (symbol " as" *> parseType),
      Postfix $ (\i t -> TmProj t i) <$> (symbol "." *> parseRecordLabel)
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
