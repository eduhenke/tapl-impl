module Type where

import Data.List (intercalate)
import Data.Map (Map, keys, toList)
import qualified Data.Map
import Kind
import Data.Maybe (fromMaybe)
import Debug.Trace

data Type
  = TyBool
  | TyNat
  | TyTop
  | TyArrow Type Type
  | TyRecord (Map String Type)
  | TyVar Int Int
  | TyAll String Kind Type
  | TyAbs String Kind Type
  | TyApp Type Type
  deriving (Eq, Ord)

data Binding
  = NameBind
  | VarBind Type
  | TyAbbreviation Type
  | TyVarBind Kind
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
bindingShift d (TyVarBind kind) = TyVarBind kind
bindingShift d (TyAbbreviation ty) = TyAbbreviation $ tyShift d ty

tyWalk :: Type -> (Int -> Int -> Int -> Type) -> Int -> Type
tyWalk t onVar c =
  let walk c t = case t of
        TyArrow t1 t2 -> TyArrow (walk c t1) (walk c t2)
        TyVar x n -> onVar c x n
        TyAll x kind tybody -> TyAll x kind (walk (c + 1) tybody)
        TyRecord ts -> TyRecord (Data.Map.map (walk c) ts)
        TyAbs x kind tybody -> TyAbs x kind (walk (c + 1) tybody)
        TyApp ty1 ty2 -> TyApp (walk c ty1) (walk c ty2)
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
      | c == x = case s of
        -- i did not see this anywhere, but after much experimentation, i've seen that this line fixes some problems.
        -- and when comparing to the evaluation of terms on simply typed lambda calculus, we have rules that prevent
        -- variables from being applied to functions, but when we are reducing type expressions, we don't have any rule
        -- to prevent that. so a current workaround is to shift the type variable by the current scope length
        (TyVar x' n') -> tyShift (c + j) s
        _ -> tyShift j s
      | otherwise = TyVar x n

tySubstTop :: Type -> Type -> Type
tySubstTop body newVal = tyShift (-1) (tySubst (tyShift 1 newVal) 0 body)

-- single step
reduceTy' :: Type -> Type
-- reduceTy' ty | trace (showTy [] ty) False = undefined
reduceTy' (TyArrow s1 s2) = TyArrow (reduceTy' s1) (reduceTy' s2) -- QR-Arrow
reduceTy' (TyAll x k1 s2) = TyAll x k1 (reduceTy' s2) -- QR-All
reduceTy' (TyAbs x k1 s2) = TyAbs x k1 (reduceTy' s2) -- QR-Abs
reduceTy' (TyApp (TyAbs x k11 s12) s2) =
  let t2 = reduceTy' s2
      t12 = reduceTy' s12
   in tySubstTop t12 t2 -- QR-AppAbs
reduceTy' (TyApp ty1 ty2) = TyApp (reduceTy' ty1) (reduceTy' ty2) -- QR-App
reduceTy' ty = ty

-- -- transitive closure
reduceTy :: Type -> Type
reduceTy ty =
  if ty == ty'
    then ty
    else reduceTy ty'
  where ty' = reduceTy' ty

tyEq :: Type -> Type -> Bool
tyEq ty1' ty2' =
  let ty1 = reduceTy ty1'
      ty2 = reduceTy ty2'
  in case (ty1, ty2) of
    (TyVar x _, TyVar y _) -> x == y
    (TyArrow s1 s2, TyArrow t1 t2) -> s1 `tyEq` t1 && s2 `tyEq` t2
    (TyAll x k1 ty1, TyAll _ k2 ty2) -> k1 == k2 && ty1 `tyEq` ty2
    (TyAbs x k1 ty1, TyAbs _ k2 ty2) -> k1 == k2 && ty1 `tyEq` ty2
    _ -> ty1 == ty2

showTy :: Context -> Type -> String
showTy _ TyBool = "Bool"
showTy _ TyNat = "Nat"
showTy _ TyTop = "Top"
showTy ctx (TyArrow ty1@(TyArrow _ _) ty2) = "(" ++ showTy ctx ty1 ++ ")->" ++ showTy ctx ty2
showTy ctx (TyArrow ty1 ty2) = showTy ctx ty1 ++ "->" ++ showTy ctx ty2
showTy ctx (TyRecord ts) = "{" ++ intercalate ", " (map (\(l, t) -> l ++ ":" ++ showTy ctx t) $ toList ts) ++ "}"
showTy ctx (TyVar n l) = if n >= 0  && n < length ctx then fst $ ctx !! n else "<Ty: " ++ show n ++ "/" ++ show l ++ ">"
showTy ctx (TyAll x kind ty) = "(∀" ++ x ++ (if kind == KindProper then "" else "::" ++ show kind) ++ "." ++ showTy ((x, TyVarBind kind) : ctx) ty ++ ")"
showTy ctx (TyAbs x kind ty) = "(λ" ++ x ++ (if kind == KindProper then "" else "::" ++ show kind) ++ "." ++ showTy ((x, TyVarBind kind) : ctx) ty ++ ")"
showTy ctx (TyApp t1 t2@(TyApp _ _)) = showTy ctx t1 ++ " (" ++ showTy ctx t2 ++ ")"
showTy ctx (TyApp t1 t2) = showTy ctx t1 ++ " " ++ showTy ctx t2

instance Show Type where
  show ty = showTy [] $ reduceTy ty