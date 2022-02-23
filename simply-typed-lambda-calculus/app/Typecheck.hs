module Typecheck where

import Data.Map (member, (!))
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

typeCheck :: Term -> Either CompilerError Type
typeCheck term = case typeOf [] term of
  Left e -> Left $ TypecheckerError e
  Right ty -> Right ty