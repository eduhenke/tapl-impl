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
import Term
import Type

expose, promote :: Context -> Type -> Type
promote ctx (TyVar n _)
  | TyVarBind ty <- getBinding ctx n = ty
  | VarBind ty <- getBinding ctx n = ty
promote ctx ty = ty
-- "Exposure is defined by repeated promotion of variables"
expose ctx ty =
  if ty' == ty
    then ty'
    else expose ctx ty'
  where
    ty' = promote ctx ty

-- calculates the least "common" supertype of both types
join :: Context -> Type -> Type -> Type
join ctx t1 t2
  | subtype ctx t1 t2 = t2
  | subtype ctx t2 t1 = t1
join ctx t1@(TyVar {}) t2 = join ctx (expose ctx t1) t2
join ctx t1 t2@(TyVar {}) = join ctx t1 (expose ctx t2)
join ctx (TyArrow s1 s2) (TyArrow t1 t2)
  | Just m <- meet ctx s1 t1 =
    TyArrow m (join ctx s2 t2)
join ctx (TyRecord m1) (TyRecord m2) =
  TyRecord $ M.intersectionWith (join ctx) m1 m2
join ctx (TyAll x1 u1 s2) (TyAll x2 u2 t2)
  | x1 == x2 && u1 == u2 = join ((x1, TyVarBind u1) : ctx) s2 t2
join _ _ _ = TyTop

-- calculates the highest "common" subtype of both types
meet :: Context -> Type -> Type -> Maybe Type
meet ctx t1 t2
  | subtype ctx t1 t2 = pure t1
  | subtype ctx t2 t1 = pure t2
meet ctx s TyTop = pure s
meet ctx TyTop t = pure t
meet ctx (TyArrow s1 s2) (TyArrow t1 t2)
  | Just m <- meet ctx s2 t2 =
    pure $ TyArrow (join ctx s1 t1) m
meet ctx (TyRecord m1) (TyRecord m2) =
  if length mapOfMaybeMeets == length mapOfMeets
    then pure $ TyRecord mapOfMeets
    else Nothing
  where
    mapOfMaybeMeets = M.unionWith meet' (M.map Just m1) (M.map Just m2)
    meet' :: Maybe Type -> Maybe Type -> Maybe Type
    meet' k1 k2 = Monad.join $ meet ctx <$> k1 <*> k2
    mapOfMeets = M.mapMaybe id mapOfMaybeMeets
meet ctx (TyAll x1 u1 s2) (TyAll x2 u2 t2)
  | x1 == x2 && u1 == u2 = meet ((x1, TyVarBind u1) : ctx) s2 t2
meet _ _ _ = Nothing

subtype :: Context -> Type -> Type -> Bool
subtype ctx t1 t2 | t1 == t2 = True
subtype ctx t1@TyVar {} t2 | subtype ctx (expose ctx t1) (expose ctx t2) = True
subtype ctx t1 TyTop = True
subtype ctx (TyVar x _) t2
  | Just (_, TyVarBind u) <- ctx `nth` x = subtype ctx u t2
  where
    nth [] n = Nothing
    nth (x : xs) 0 = Just x
    nth (x : xs) n = nth xs (n - 1)
subtype ctx (TyArrow s1 s2) (TyArrow t1 t2) = subtype ctx t1 s1 && subtype ctx s2 t2
subtype ctx (TyRecord k) (TyRecord l) =
  if keysSet l `isSubsetOf` keysSet k
    then foldrWithKey f True l
    else False
  where
    f label tyT acc =
      Just True == do
        tyS <- M.lookup label k
        Just $ acc && subtype ctx tyS tyT
subtype ctx (TyAll x1 u1 s2) (TyAll x2 u2 t2)
  | x1 == x2 && u1 == u2 = subtype ((x1, TyVarBind u1) : ctx) s2 t2
subtype _ _ _ = False

typeOf :: Context -> Term -> Either TypeError Type
typeOf _ TmFalse = Right TyBool
typeOf _ TmTrue = Right TyBool
typeOf _ TmZero = Right TyNat
typeOf ctx (TmIf c t f) = do
  tyCond <- typeOf ctx c
  if subtype ctx tyCond TyBool
    then do
      tyT <- typeOf ctx t
      tyF <- typeOf ctx f
      pure $ join ctx tyT tyF
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
  let VarBind ty = getBinding ctx n
   in Right ty
typeOf ctx (App t1 t2) = do
  ty1 <- typeOf ctx t1
  ty2 <- typeOf ctx t2
  case expose ctx ty1 of
    (TyArrow tyArg tyBody) ->
      if subtype ctx ty2 tyArg
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
  case expose ctx ty of
    (TyRecord ts) ->
      if member i ts
        then Right $ ts ! i
        else Left InvalidProjection
    _ -> Left ProjectionNotAppliedToATuple
typeOf ctx (TmTyAbs x tyT1 t2) =
  TyAll x tyT1 <$> typeOf ((x, TyVarBind tyT1) : ctx) t2
typeOf ctx (TmTyApp t argTy) = do
  termTy <- typeOf ctx t
  case expose ctx termTy of
    (TyAll tyX tyT1 allBody) ->
      if subtype ctx argTy tyT1
        then return $ tySubstTop allBody argTy
        else Left $ TypeTyAppMustFollowTypeConstraints argTy tyT1
    _ -> Left TypeTyAppMustApplyToForallType

typeCheck :: Term -> Either CompilerError Type
typeCheck term = case typeOf [] term of
  Left e -> Left $ TypecheckerError e
  Right ty -> Right ty