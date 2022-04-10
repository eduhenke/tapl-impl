module Type where

import Data.List (intercalate)
import Data.Map (Map, keys, toList)
import qualified Data.Map

data Type
  = TyBool
  | TyNat
  | TyTop
  | TyArrow Type Type
  | TyRecord (Map String Type)
  | TyVar Int Int
  | TyAll String Type Type
  deriving (Eq, Ord)

data Binding
  = NameBind
  | VarBind Type
  | TyAbbreviation Type
  | TyVarBind Type
  deriving (Eq, Ord, Show)

type Context = [(String, Binding)]

getBinding :: Context -> Int -> Binding
getBinding ctx n =
  bindingShift (n + 1) bind
  where
    (_, bind) = ctx !! n

bindingShift :: Int -> Binding -> Binding
bindingShift d NameBind = NameBind
bindingShift d (VarBind ty) = VarBind $ tyShift d ty
bindingShift d (TyVarBind ty) = TyVarBind $ tyShift d ty
bindingShift d (TyAbbreviation ty) = TyAbbreviation $ tyShift d ty

tyWalk :: Type -> (Int -> Int -> Int -> Type) -> Int -> Type
tyWalk t onVar c =
  let walk c t = case t of
        TyArrow t1 t2 -> TyArrow (walk c t1) (walk c t2)
        TyVar x n -> onVar c x n
        TyAll x tyT1 tybody -> TyAll x (walk c tyT1) (walk (c + 1) tybody)
        TyRecord ts -> TyRecord (Data.Map.map (walk c) ts)
        t -> t
   in walk c t

tyShift :: Int -> Type -> Type
tyShift d t = tyWalk t onVar 0
  where
    onVar c x n
      | x >= c = TyVar (x + d) (n + d)
      | otherwise = TyVar x (n + d)

tySubst :: Type -> Int -> Type -> Type
tySubst s j t = tyWalk t onVar j
  where
    onVar c x n
      | c == x = s
      | otherwise = TyVar x n

tySubstTop :: Type -> Type -> Type
tySubstTop body newVal = tyShift (-1) (tySubst (tyShift 1 newVal) 0 body)

showTy :: Context -> Type -> String
showTy _ TyBool = "Bool"
showTy _ TyNat = "Nat"
showTy _ TyTop = "Top"
showTy ctx (TyArrow ty1@(TyArrow _ _) ty2) = "(" ++ showTy ctx ty1 ++ ")->" ++ showTy ctx ty2
showTy ctx (TyArrow ty1 ty2) = showTy ctx ty1 ++ "->" ++ showTy ctx ty2
showTy ctx (TyRecord ts) = "{" ++ intercalate ", " (map (\(l, t) -> l ++ ":" ++ showTy ctx t) $ toList ts) ++ "}"
showTy ctx (TyVar n l) = if n < length ctx then fst $ ctx !! n else "<Ty: " ++ show n ++ "/" ++ show l ++ ">"
showTy ctx (TyAll x tyT1 ty) = "(∀" ++ x ++ "<:(" ++ showTy ctx tyT1 ++ ")." ++ showTy ((x, TyVarBind tyT1) : ctx) ty ++ ")"

instance Show Type where
  show = showTy []