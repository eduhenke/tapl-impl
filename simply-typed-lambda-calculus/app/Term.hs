module Term where

import Type

data Term
  = TmTrue
  | TmFalse
  | TmIf Term Term Term
  | Var Int Int
  | Abs String Type Term
  | App Term Term
  deriving (Eq, Ord)

instance Show Term where
  show t =
    let show' ctx t =
          case t of
            TmTrue -> "true"
            TmFalse -> "false"
            TmIf cond t f -> "if " ++ show' ctx cond ++ " then " ++ show' ctx t ++ " else " ++ show' ctx f
            Var n _ -> fst $ ctx !! n
            Abs x ty t -> "(λ" ++ x ++ ":" ++ show ty ++ "." ++ show' ((x, ty) : ctx) t ++ ")"
            App t1 t2 -> show' ctx t1 ++ " " ++ show' ctx t2
     in show' [] t