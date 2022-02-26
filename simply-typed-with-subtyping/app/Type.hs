module Type where

import Data.List (intercalate)
import Data.Map (Map, toList)

data Type
  = TyBool
  | TyNat
  | TyArrow Type Type
  | TyUnit
  | TyRecord (Map String Type)
  | TyVariant (Map String Type)
  deriving (Eq, Ord)

instance Show Type where
  show TyBool = "Bool"
  show TyNat = "Nat"
  show (TyArrow ty1 ty2) = show ty1 ++ "->" ++ show ty2
  show TyUnit = "Unit"
  show (TyRecord ts) = "{" ++ intercalate ", " (map (\(l, t) -> l ++ ":" ++ show t) $ toList ts) ++ "}"
  show (TyVariant ts) = "<" ++ intercalate ", " (map (\(l, t) -> l ++ ":" ++ show t) $ toList ts) ++ ">"
