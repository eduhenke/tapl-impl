module Term where

import Data.List
import Data.Map (Map, toList)
import Data.Maybe (fromMaybe)
import Kind (Kind)
import Text.Read (readMaybe)
import Type

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
  | TmLet String Term Term
  | TmRecord (Map String Term)
  | TmProj Term String
  | TmTyAbs String Kind Term
  | TmTyApp Term Type
  deriving (Eq, Ord)

instance Show Term where
  show t =
    let show' ctx t =
          let s = show' ctx
           in case t of
                TmTrue -> "true"
                TmFalse -> "false"
                TmIf cond t f -> "if " ++ s cond ++ " then " ++ s t ++ " else " ++ s f
                Var n l -> if n < length ctx then fst $ ctx !! n else "<Var: " ++ show n ++ "/" ++ show l ++ ">"
                Abs x ty t -> "(λ" ++ x ++ ":" ++ showTy ctx (reduceTy ty) ++ "." ++ show' ((x, VarBind ty) : ctx) t ++ ")"
                App t1 t2@(App _ _) -> s t1 ++ " (" ++ s t2 ++ ")"
                App t1 t2 -> s t1 ++ " " ++ s t2
                TmLet x t1 t2 -> "let " ++ x ++ "=" ++ s t1 ++ " in " ++ show' ((x, NameBind) : ctx) t2
                TmProj t i -> show' ctx t ++ "." ++ i
                TmRecord ts -> "{" ++ intercalate ", " (map (\(l, t) -> l ++ "=" ++ show' ctx t) $ toList ts) ++ "}"
                TmZero -> "0"
                TmSucc t -> (\x -> maybe ("succ " ++ x) (show . (1 +)) (readMaybe x :: Maybe Int)) (s t)
                TmPred t -> (\x -> maybe ("pred " ++ x) (show . (1 -)) (readMaybe x :: Maybe Int)) (s t)
                TmIsZero t -> "zero? " ++ s t
                TmTyAbs x kind t -> "Λ" ++ x ++ "." ++ show' ((x, TyVarBind kind) : ctx) t
                TmTyApp t ty -> s t ++ " [" ++ showTy ctx (reduceTy ty) ++ "]"
     in show' [] t