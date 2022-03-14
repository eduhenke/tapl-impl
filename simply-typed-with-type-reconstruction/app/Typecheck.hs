module Typecheck where

import qualified Control.Monad as Monad
import Data.Map (assocs, elems, foldrWithKey, keys, keysSet, member, (!))
import qualified Data.Map as M
import Data.Set (isSubsetOf)
import qualified Data.Set as S
import Error
import Term
import Type

typeOf :: Context -> Term -> Either TypeError Type
typeOf _ TmFalse = Right TyBool
typeOf _ TmTrue = Right TyBool
typeOf _ TmZero = Right TyNat
typeOf ctx (TmIf c t f) =
  if typeOf ctx c == Right TyBool
    then do
      tyT <- typeOf ctx t
      tyF <- typeOf ctx f
      if tyT == tyF
        then Right tyT
        else Left TypeOfArmsMustMatch
    else Left TypeOfConditionMustBeBool
typeOf ctx (TmSucc t) = do
  ty <- typeOf ctx t
  if ty == TyNat
    then pure TyNat
    else Left TypeMustBeNat
typeOf ctx (TmPred t)
  | typeOf ctx t == Right TyNat = Right TyNat
  | otherwise = Left TypeMustBeNat
typeOf ctx (TmIsZero t)
  | typeOf ctx t == Right TyNat = Right TyBool
  | otherwise = Left TypeMustBeNat
typeOf ctx (Var n _) =
  let (_, VarBind ty) = (ctx !! n)
   in Right ty
typeOf ctx (App t1 t2) = do
  ty1 <- typeOf ctx t1
  ty2 <- typeOf ctx t2
  case ty1 of
    (TyArrow tyArg tyBody) ->
      if ty2 == tyArg
        then Right tyBody
        else Left $ TypeAppArgumentMustMatch ((show t2 ++ ": " ++ show ty2) ++ " vs. " ++ show tyArg)
    _ -> Left TypeArrowExpected
typeOf ctx (Abs arg tyArg body) = do
  tyBody <- typeOf ((arg, VarBind tyArg) : ctx) body
  return $ TyArrow tyArg tyBody
typeOf ctx (TmLet x t1 t2) = do
  ty1 <- typeOf ctx t1
  let ctx' = (x, VarBind ty1) : ctx
  typeOf ctx' t2

typeWalk :: Type -> (Int -> Int -> Int -> Type) -> Int -> Type
typeWalk t onVar c =
  let walk c t = case t of
        TyArrow t1 t2 -> TyArrow (walk c t1) (walk c t2)
        TyVar x n -> onVar c x n
        t -> t
   in walk c t

shift :: Int -> Type -> Type
shift d t = typeWalk t onVar 0
  where
    onVar c x n
      | x >= c = TyVar (x + d) (n + d)
      | otherwise = TyVar x (n + d)

subst :: Type -> Int -> Type -> Type
subst s j t = typeWalk t onVar j
  where
    onVar c x n
      | c == x = s
      | otherwise = TyVar x n

betaReduction :: Type -> Type -> Type
betaReduction body newVal = shift (-1) (subst (shift 1 newVal) 0 body)

typeCheck :: Term -> Either CompilerError Type
typeCheck term = case typeOf [] term of
  Left e -> Left $ TypecheckerError e
  Right ty -> Right ty