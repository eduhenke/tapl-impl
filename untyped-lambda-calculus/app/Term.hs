module Term where

data Term
  = Var Int Int
  | Abs String Term
  | App Term Term
  deriving (Eq, Ord)

instance Show Term where
  show t =
    let show' ctx t =
          case t of
            Var n _ -> ctx !! n
            Abs x t -> "(λ" ++ x ++ "." ++ show' (x : ctx) t ++ ")"
            App t1 t2 -> "(" ++ show' ctx t1 ++ " " ++ show' ctx t2 ++ ")"
     in show' [] t