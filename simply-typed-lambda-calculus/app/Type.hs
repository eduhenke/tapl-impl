module Type where

data Type
  = TyBool
  | TyArrow Type Type
  deriving (Eq, Ord)

instance Show Type where
  show TyBool = "Bool"
  show (TyArrow ty1 ty2) = show ty1 ++ "->" ++ show ty2
