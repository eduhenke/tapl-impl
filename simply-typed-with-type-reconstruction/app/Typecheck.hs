module Typecheck where

import qualified Control.Monad as Monad
import Control.Monad.State
import Data.Bifunctor (bimap)
import Data.List (find)
import Data.Map (assocs, elems, foldrWithKey, keys, keysSet, member, (!))
import qualified Data.Map as M
import Data.Set (isSubsetOf)
import qualified Data.Set as S
import Debug.Trace (trace)
import Error
import Eval
import GHC.IO (unsafePerformIO)
import Term
import Type

type Constraint = (Type, Type)

type GenNameState = Int

genName :: State GenNameState String
genName = do n <- get; put (n + 1); return $ "?X" ++ show n

typeReconstruct :: Context -> GenNameState -> Term -> (Type, GenNameState, [Constraint])
typeReconstruct _ g TmTrue = (TyBool, g, [])
typeReconstruct _ g TmFalse = (TyBool, g, [])
typeReconstruct _ g TmZero = (TyNat, g, [])
typeReconstruct ctx g (TmSucc t) =
  let (ty, g', constr) = typeReconstruct ctx g t
   in (TyNat, g', (ty, TyNat) : constr)
typeReconstruct ctx g (TmPred t) =
  let (ty, g', constr) = typeReconstruct ctx g t
   in (TyNat, g', (ty, TyNat) : constr)
typeReconstruct ctx g (TmIsZero t) =
  let (ty, g', constr) = typeReconstruct ctx g t
   in (TyBool, g', (ty, TyNat) : constr)
typeReconstruct ctx g (TmIf c t f) =
  let (ty1, g1, constr1) = typeReconstruct ctx g c
      (ty2, g2, constr2) = typeReconstruct ctx g1 t
      (ty3, g3, constr3) = typeReconstruct ctx g2 f
   in (ty2, g3, (ty1, TyBool) : (ty2, ty3) : (constr1 ++ constr2 ++ constr3))
typeReconstruct ctx g (Abs arg (Just tyArg) body) =
  let (tyBody, g', constr) = typeReconstruct ((arg, VarBind tyArg) : ctx) g body
   in (TyArrow tyArg tyBody, g', constr)
typeReconstruct ctx state (Abs arg Nothing body) =
  let (tyName, state') = runState genName state
      tyArg = TyVar tyName
      (tyBody, state'', constr) = typeReconstruct ((arg, VarBind tyArg) : ctx) state' body
   in (TyArrow tyArg tyBody, state'', constr)
typeReconstruct ctx g (Var n _) =
  let (_, VarBind ty) = (ctx !! n)
   in (ty, g, [])
typeReconstruct ctx g (App t1 t2) =
  let (ty1, g1, constr1) = typeReconstruct ctx g t1
      (ty2, g2, constr2) = typeReconstruct ctx g1 t2
      (tyName, g3) = runState genName g2
   in (TyVar tyName, g3, (ty1, TyArrow ty2 (TyVar tyName)) : (constr1 ++ constr2))
-- with let polymorphism
typeReconstruct ctx g (TmLet x t1 t2) =
  let (_, g', constr) = typeReconstruct ctx g t1
      (ty', g'', constr') = typeReconstruct ctx g' (betaReduction t2 t1)
   in (ty', g'', constr ++ constr')

type Substitution = [(String, Type)]

apply :: Substitution -> Type -> Type
apply σ (TyVar var) =
  case find ((var ==) . fst) σ of
    Nothing -> TyVar var
    Just (_, ty) -> ty
apply σ (TyArrow ty1 ty2) =
  TyArrow
    (apply σ ty1)
    (apply σ ty2)
apply _ ty = ty

domain :: Substitution -> [String]
domain = map fst

range :: Substitution -> [Type]
range = map snd

compose :: Substitution -> Substitution -> Substitution
compose σ γ =
  map (\(x, t) -> (x, apply σ t)) γ
    ++ filter (\(x, _) -> x `notElem` domain γ) σ

freeVars :: Type -> [String]
freeVars (TyVar var) = [var]
freeVars (TyArrow ty1 ty2) = freeVars ty1 ++ freeVars ty2
freeVars _ = []

-- generates a principal unifier for a set of constraints
unify :: [Constraint] -> Substitution
unify [] = []
unify ((s, t) : cs) | s == t = unify cs
unify ((TyVar s, t) : cs)
  | s `notElem` freeVars t =
    let subst = apply [(s, t)]
        cs' = map (bimap subst subst) cs
     in unify cs' `compose` [(s, t)]
unify ((s, TyVar t) : cs)
  | t `notElem` freeVars s =
    let subst = apply [(t, s)]
        cs' = map (bimap subst subst) cs
     in unify cs' `compose` [(t, s)]
unify ((TyArrow s1 s2, TyArrow t1 t2) : cs) =
  unify (cs ++ [(s1, t1), (s2, t2)])
unify _ = error "Unsolvable constraints"

typeCheck :: Term -> Either CompilerError Type
typeCheck term =
  let (ty, _, constraints) = typeReconstruct [] 0 term
      unifier = unify constraints
      principalTy = apply unifier ty
   in Right principalTy