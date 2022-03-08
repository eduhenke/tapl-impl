module Typecheck where

import qualified Control.Monad as Monad
import Data.Map (assocs, elems, foldrWithKey, keys, keysSet, member, (!))
import qualified Data.Map as M
import Data.Set (isSubsetOf)
import qualified Data.Set as S
import Error
import Term
import Type

-- calculates the least "common" supertype of both types
join :: Type -> Type -> Type
join t1 t2 | t1 == t2 = t1
join (TyArrow s1 s2) (TyArrow t1 t2)
  | Just m <- meet s1 t1 =
    TyArrow m (join s2 t2)
join (TyRecord m1) (TyRecord m2) =
  TyRecord $ M.intersectionWith join m1 m2

-- join _ _ = TyTop

-- calculates the highest "common" subtype of both types
meet :: Type -> Type -> Maybe Type
meet t1 t2 | t1 == t2 = pure t1
meet s TyTop = pure s
meet TyTop t = pure t
meet (TyArrow s1 s2) (TyArrow t1 t2)
  | Just m <- meet s2 t2 =
    pure $ TyArrow (join s1 t1) m
meet (TyRecord m1) (TyRecord m2) =
  if length mapOfMaybeMeets == length mapOfMeets
    then pure $ TyRecord mapOfMeets
    else Nothing
  where
    mapOfMaybeMeets = M.unionWith meet' (M.map Just m1) (M.map Just m2)
    meet' :: Maybe Type -> Maybe Type -> Maybe Type
    meet' k1 k2 = Monad.join $ meet <$> k1 <*> k2
    mapOfMeets = M.mapMaybe id mapOfMaybeMeets
meet _ _ = Nothing

subtype :: Type -> Type -> Bool
subtype t1 t2 | t1 == t2 = True
subtype t1 TyTop = True
subtype (TyArrow s1 s2) (TyArrow t1 t2) = subtype t1 s1 && subtype s2 t2
subtype (TyRecord k) (TyRecord l) =
  if keysSet l `isSubsetOf` keysSet k
    then foldrWithKey f True l
    else False
  where
    f label tyT acc =
      Just True == do
        tyS <- M.lookup label k
        Just $ acc && subtype tyS tyT
subtype _ _ = False

typeOf :: Context -> Term -> Either TypeError Type
typeOf _ TmFalse = Right TyBool
typeOf _ TmTrue = Right TyBool
typeOf _ TmZero = Right TyNat
typeOf _ TmUnit = Right TyUnit
typeOf ctx (TmIf c t f) =
  if typeOf ctx c == Right TyBool
    then do
      tyT <- typeOf ctx t
      tyF <- typeOf ctx f
      pure $ tyT `join` tyF
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
      if subtype ty2 tyArg
        then Right tyBody
        else Left TypeAppArgumentMustMatch
    _ -> Left TypeArrowExpected
typeOf ctx (Abs arg tyArg body) = do
  tyBody <- typeOf ((arg, VarBind tyArg) : ctx) body
  return $ TyArrow tyArg tyBody
typeOf ctx (TmAscription t ty) =
  if typeOf ctx t == Right ty
    then Right ty
    else Left TypeOfAscriptionMustMatch
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
  _ -> Left ProjectionNotAppliedToARecord
typeOf ctx (TmVariant t l varTy@(TyVariant tys)) = do
  termTy <- typeOf ctx t
  if termTy == tys ! l
    then Right varTy
    else Left TypeOfVariantMustMatch
typeOf ctx (TmVariant {}) = Left MustHaveAscriptionOfVariantType
typeOf ctx (TmCase t cases) = do
  termTy <- typeOf ctx t
  case termTy of
    TyVariant varTys ->
      if keys varTys /= keys cases
        then Left NonMatchingBranchesOfCaseWithVariantType
        else do
          caseTysMap <- sequence $ M.mapWithKey (\l (x, t') -> typeOf ((x, VarBind (varTys ! l)) : ctx) t') cases
          let caseTys = elems caseTysMap
              ty = head caseTys
          if all (== ty) caseTys
            then Right ty
            else Left CaseBranchesTypesMustMatchVariantType
    _ -> Left CaseMustBeAppliedToVariant
typeOf ctx (TmFix t) = do
  ty <- typeOf ctx t
  case ty of
    TyArrow x y -> if x == y then Right x else Left FixMustBeAppliedToAnArrowTypeWithSameDomainAndImage
    _ -> Left FixMustBeAppliedToAnArrowTypeWithSameDomainAndImage

typeCheck :: Term -> Either CompilerError Type
typeCheck term = case typeOf [] term of
  Left e -> Left $ TypecheckerError e
  Right ty -> Right ty