module Type where

import Data.List (intercalate)
import Data.Map (Map, keys, toList)

data Type
  = TyBool
  | TyNat
  | TyArrow Type Type
  | TyRecord (Map String Type)
  | TyVar Int Int
  | TyAll String Type
  deriving (Eq, Ord)

data Binding
  = NameBind
  | VarBind Type
  | TyAbbreviation Type
  | TyVarBind
  deriving (Eq, Ord)

type Context = [(String, Binding)]

showTy :: Context -> Type -> String
showTy _ TyBool = "Bool"
showTy _ TyNat = "Nat"
showTy ctx (TyArrow ty1@(TyArrow _ _) ty2) = "(" ++ showTy ctx ty1 ++ ")->" ++ showTy ctx ty2
showTy ctx (TyArrow ty1 ty2) = showTy ctx ty1 ++ "->" ++ showTy ctx ty2
showTy ctx (TyRecord ts) = "{" ++ intercalate ", " (map (\(l, t) -> l ++ ":" ++ showTy ctx t) $ toList ts) ++ "}"
showTy ctx (TyVar n l) = if n < length ctx then fst $ ctx !! n else "<Ty: " ++ show n ++ "/" ++ show l ++ ">"
showTy ctx (TyAll x ty) = "(∀" ++ x ++ "." ++ showTy ((x, TyVarBind) : ctx) ty ++ ")"