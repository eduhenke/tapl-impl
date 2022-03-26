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
  | Abs String (Maybe Type) Term
  | App Term Term
  | TmLet String Term Term
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
                Abs x ty t ->
                  case ty of
                    Just ty -> "(λ" ++ x ++ ":" ++ show ty ++ "." ++ show' (x : ctx) t ++ ")"
                    Nothing -> "(λ" ++ x ++ "." ++ show' (x : ctx) t ++ ")"
                App t1 t2@(App _ _) -> s t1 ++ " (" ++ s t2 ++ ")"
                App t1 t2 -> s t1 ++ " " ++ s t2
                TmLet x t1 t2 -> "let " ++ x ++ "=" ++ s t1 ++ " in " ++ show' (x : ctx) t2
                TmZero -> "0"
                TmSucc t -> (\x -> maybe ("succ " ++ x) (show . (1 +)) (readMaybe x :: Maybe Int)) (s t)
                TmPred t -> (\x -> maybe ("pred " ++ x) (show . (1 -)) (readMaybe x :: Maybe Int)) (s t)
                TmIsZero t -> "zero? " ++ s t
     in show' [] t