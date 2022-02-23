module Type where

import Data.List (intercalate)

data Type
  = TyBool
  | TyArrow Type Type
  | TyUnit
  | TyTuple [Type]
  deriving (Eq, Ord)

instance Show Type where
  show TyBool = "Bool"
  show (TyArrow ty1 ty2) = show ty1 ++ "->" ++ show ty2
  show TyUnit = "Unit"
  show (TyTuple ts) = "{" ++ intercalate ", " (map show ts) ++ "}"
