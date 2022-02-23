module Typecheck where

import Data.Map (elems, keys, member, (!))
import qualified Data.Map
import Error
import Term
import Type

typeOf :: Context -> Term -> Either TypeError Type
typeOf _ TmFalse = Right TyBool
typeOf _ TmTrue = Right TyBool
typeOf _ TmUnit = Right TyUnit
typeOf ctx (TmIf c t f) =
  if typeOf ctx c == Right TyBool
    then
      let tyT = typeOf ctx t
          tyF = typeOf ctx f
       in if tyT == tyF
            then tyT
            else Left TypeOfArmsMustMatch
    else Left TypeOfConditionMustBeBool
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
  _ -> Left ProjectionNotAppliedToATuple
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
          caseTysMap <- sequence $ Data.Map.mapWithKey (\l (x, t') -> typeOf ((x, VarBind (varTys ! l)) : ctx) t') cases
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