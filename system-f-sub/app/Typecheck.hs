module Typecheck where

import qualified Control.Monad as Monad
import Control.Monad.State
import Data.Bifunctor (bimap)
import Data.List (find)
import Data.Map (assocs, elems, foldrWithKey, keys, keysSet, map, member, (!))
import qualified Data.Map as M
import Data.Set (isSubsetOf)
import qualified Data.Set as S
import Debug.Trace (trace)
import Error
import GHC.IO (unsafePerformIO)
import Term
import Type

typeOf :: Context -> Term -> Either TypeError Type
typeOf _ TmFalse = Right TyBool
typeOf _ TmTrue = Right TyBool
typeOf _ TmZero = Right TyNat
typeOf ctx (TmIf c t f) =
  if typeOf ctx c == Right TyBool
    then
      let tyT = typeOf ctx t
          tyF = typeOf ctx f
       in if tyT == tyF
            then tyT
            else Left TypeOfArmsMustMatch
    else Left TypeOfConditionMustBeBool
typeOf ctx (TmSucc t)
  | typeOf ctx t == Right TyNat = Right TyNat
  | otherwise = Left TypeMustBeNat
typeOf ctx (TmPred t)
  | typeOf ctx t == Right TyNat = Right TyNat
  | otherwise = Left TypeMustBeNat
typeOf ctx (TmIsZero t)
  | typeOf ctx t == Right TyNat = Right TyBool
  | otherwise = Left TypeMustBeNat
typeOf ctx (Var n _) =
  let (_, VarBind ty) = (ctx !! n)
   in Right ty
typeOf ctx (App t1 t2) =
  case ty1 of
    Right (TyArrow tyArg tyBody) ->
      if Right tyArg == ty2
        then Right tyBody
        else Left TypeAppArgumentMustMatch
    _ -> Left TypeArrowExpected
  where
    ty1 = typeOf ctx t1
    ty2 = typeOf ctx t2
typeOf ctx (Abs arg tyArg body) = do
  tyBody <- typeOf ((arg, VarBind tyArg) : ctx) body
  return $ TyArrow tyArg tyBody
typeOf ctx (TmLet x t1 t2) = do
  ty1 <- typeOf ctx t1
  let ctx' = (x, VarBind ty1) : ctx
  typeOf ctx' t2
typeOf ctx (TmRecord ts) = TyRecord <$> mapM (typeOf ctx) ts
typeOf ctx (TmProj t i) = case typeOf ctx t of
  (Right (TyRecord ts)) ->
    if member i ts
      then Right $ ts ! i
      else Left InvalidProjection
  _ -> Left ProjectionNotAppliedToATuple
typeOf ctx (TmTyAbs x t) = TyAll x <$> typeOf ctx t
typeOf ctx (TmTyApp t argTy) = do
  termTy <- typeOf ctx t
  case termTy of
    (TyAll x allBody) -> return $ tySubstTop allBody argTy
    _ -> Left TypeTyAppMustApplyToForallType

tyWalk :: Type -> (Int -> Int -> Int -> Type) -> Int -> Type
tyWalk t onVar c =
  let walk c t = case t of
        TyArrow t1 t2 -> TyArrow (walk c t1) (walk c t2)
        TyVar x n -> onVar c x n
        TyAll x tybody -> TyAll x (walk (c + 1) tybody)
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

typeCheck :: Term -> Either CompilerError Type
typeCheck term = case typeOf [] term of
  Left e -> Left $ TypecheckerError e
  Right ty -> Right ty