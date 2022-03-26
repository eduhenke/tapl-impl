module Type where

import Data.List (intercalate)
import Data.Map (Map, keys, toList)

data Type
  = TyBool
  | TyNat
  | TyArrow Type Type
  | TyVar String
  deriving (Eq, Ord)

instance Show Type where
  show =
    let show' ctx ty = case ty of
          TyBool -> "Bool"
          TyNat -> "Nat"
          (TyArrow ty1@(TyArrow _ _) ty2) -> "(" ++ show' ctx ty1 ++ ")->" ++ show' ctx ty2
          (TyArrow ty1 ty2) -> show' ctx ty1 ++ "->" ++ show' ctx ty2
          (TyVar n) -> n
     in show' []