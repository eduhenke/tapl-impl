module Typecheck where

import qualified Control.Monad as Monad
import Data.Bifunctor (bimap)
import Data.List (find)
import Data.Map (assocs, elems, foldrWithKey, keys, keysSet, map, member, (!))
import qualified Data.Map as M
import Data.Set (isSubsetOf)
import qualified Data.Set as S
import Error
import GHC.IO (unsafePerformIO)
import Kind
import Term
import Type

typeOf :: Context -> Term -> Either TypeError Type
typeOf _ TmFalse = Right TyBool
typeOf _ TmTrue = Right TyBool
typeOf _ TmZero = Right TyNat
typeOf ctx (TmIf c t f) = do
  tyCond <- typeOf ctx c
  if tyCond `tyEq` TyBool
    then do
      tyT <- typeOf ctx t
      tyF <- typeOf ctx f
      if tyT `tyEq` tyF
        then pure tyT
        else Left TypeOfArmsMustMatch
    else Left TypeOfConditionMustBeBool
typeOf ctx (TmSucc t) = do
  ty <- typeOf ctx t
  if ty `tyEq` TyNat 
    then pure TyNat
    else Left TypeMustBeNat
typeOf ctx (TmPred t) = do
  ty <- typeOf ctx t
  if ty `tyEq` TyNat 
    then pure TyNat
    else Left TypeMustBeNat
typeOf ctx (TmIsZero t) = do
  ty <- typeOf ctx t
  if ty `tyEq` TyNat 
    then pure TyBool
    else Left TypeMustBeNat
typeOf ctx (Var n _) =
  let VarBind ty = getBinding ctx n
   in Right ty
typeOf ctx (App t1 t2) = do
  ty1 <- typeOf ctx t1
  ty2 <- typeOf ctx t2
  case reduceTy ty1 of
    (TyArrow tyArg tyBody) ->
      if ty2 `tyEq` tyArg
        then Right tyBody
        else Left $ TypeAppArgumentMustMatch tyArg ty2
    _ -> Left TypeArrowExpected
typeOf ctx (Abs arg tyArg body) = do
  tyBody <- typeOf ((arg, VarBind tyArg) : ctx) body
  return $ TyArrow tyArg (tyShift (-1) tyBody)
typeOf ctx (TmLet x t1 t2) = do
  ty1 <- typeOf ctx t1
  ty' <- typeOf ((x, VarBind ty1) : ctx) t2
  pure $ tyShift (-1) ty'
typeOf ctx (TmRecord ts) = TyRecord <$> mapM (typeOf ctx) ts
typeOf ctx (TmProj t i) = do
  ty <- typeOf ctx t
  case reduceTy ty of
    (TyRecord ts) ->
      if member i ts
        then Right $ ts ! i
        else Left InvalidProjection
    _ -> Left ProjectionNotAppliedToATuple
typeOf ctx (TmTyAbs x tyT1 t2) =
  TyAll x tyT1 <$> typeOf ((x, TyVarBind tyT1) : ctx) t2
typeOf ctx (TmTyApp t argTy) = do
  termTy <- typeOf ctx t
  argKind <- kindOf ctx argTy
  case reduceTy termTy of
    (TyAll tyX kind allBody) ->
      if argKind == kind
        then return $ tySubstTop allBody argTy
        else Left $ TypeTyAppMustFollowKind argTy kind
    _ -> Left TypeTyAppMustApplyToForallType

kindOf :: Context -> Type -> Either TypeError Kind
-- K-TVar
kindOf ctx (TyVar n l) =
  let (TyVarBind kind) = getBinding ctx n
   in pure kind
-- K-Abs
kindOf ctx (TyAbs x k1 tybody) = do
  k2 <- kindOf ((x, TyVarBind k1) : ctx) tybody
  return $ KindOperator k1 k2
-- K-App
kindOf ctx (TyApp ty1 ty2) = do
  k1 <- kindOf ctx ty1
  k2 <- kindOf ctx ty2
  case k1 of
    (KindOperator argKind bodyKind) ->
      if argKind == k2
        then pure bodyKind
        else Left ParameterKindMismatch
    _ -> Left ArrowKindExpected
-- K-Arrow
kindOf ctx (TyArrow ty1 ty2) = do
  k1 <- kindOf ctx ty1
  k2 <- kindOf ctx ty2
  case (k1, k2) of
    (KindProper, KindProper) -> pure KindProper
    _ -> Left ProperTypeExpected
kindOf ctx (TyAll x k1 ty2) = do
  k2 <- kindOf ((x, TyVarBind k1) : ctx) ty2
  case (k1, k2) of
    (KindProper, KindProper) -> pure KindProper
    _ -> Left ProperTypeExpected
kindOf ctx _ = pure KindProper

typeCheck :: Term -> Either CompilerError Type
typeCheck term = case typeOf [] term of
  Left e -> Left $ TypecheckerError e
  Right ty -> Right ty