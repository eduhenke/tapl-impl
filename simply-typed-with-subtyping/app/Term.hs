module Term where

import Data.List
import Data.Map (Map, toList)
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)
import Type

data Binding
  = NameBind
  | VarBind Type
  | TyAbbreviation Type
  deriving (Eq, Ord, Show)

type Context = [(String, Binding)]

data Term
  = TmTrue
  | TmFalse
  | TmIf Term Term Term
  | TmZero
  | TmSucc Term
  | TmPred Term
  | TmIsZero Term
  | Var Int Int
  | Abs String Type Term
  | App Term Term
  | TmUnit
  | TmAscription Term Type
  | TmLet String Term Term
  | TmRecord (Map String Term)
  | TmProj Term String
  | TmVariant Term String Type
  | TmCase Term (Map String (String, Term))
  | TmFix Term
  deriving (Eq, Ord)

instance Show Term where
  show t =
    let show' ctx t =
          let s = show' ctx
           in case t of
                TmTrue -> "true"
                TmFalse -> "false"
                TmIf cond t f -> "if " ++ s cond ++ " then " ++ s t ++ " else " ++ s f
                Var n _ -> if n < length ctx then ctx !! n else "<Var ???>"
                Abs x ty t -> "(λ" ++ x ++ ":" ++ show ty ++ "." ++ show' (x : ctx) t ++ ")"
                App t1 t2 -> s t1 ++ " " ++ s t2
                TmUnit -> "unit"
                TmAscription t ty -> s t ++ " as " ++ show ty
                TmLet x t1 t2 -> "let " ++ x ++ "=" ++ s t1 ++ " in " ++ show' (x : ctx) t2
                TmProj t i -> s t ++ "." ++ i
                TmRecord ts -> "{" ++ intercalate ", " (map (\(l, t) -> l ++ "=" ++ s t) $ toList ts) ++ "}"
                TmVariant t l ty -> "<" ++ l ++ "=" ++ s t ++ "> as " ++ show ty
                TmCase t cases -> "case " ++ s t ++ " of\n\t" ++ intercalate "\n\t" (map (\(l, (x, t)) -> "<" ++ l ++ "=" ++ x ++ "> => " ++ show' (x : ctx) t) $ toList cases)
                TmFix t -> "fix " ++ s t
                TmZero -> "0"
                TmSucc t -> (\x -> maybe ("succ " ++ x) (show . (1 +)) (readMaybe x :: Maybe Int)) (s t)
                TmPred t -> (\x -> maybe ("pred " ++ x) (show . (1 -)) (readMaybe x :: Maybe Int)) (s t)
                TmIsZero t -> "zero? " ++ s t
     in show' [] t