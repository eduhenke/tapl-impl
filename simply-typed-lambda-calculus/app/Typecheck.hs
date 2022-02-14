module Typecheck where

import Term
import Type

data Binding = NameBind | VarBind Type deriving (Show)

type Context = [(String, Binding)]

data TypeError
  = TypeOfConditionMustBeBool
  | TypeOfArmsMustMatch
  | TypeOfVarNotSpecified
  | TypeArrowExpected
  | TypeAppArgumentMustMatch
  deriving (Eq, Show)

typeOf :: Context -> Term -> Either TypeError Type
typeOf _ TmFalse = Right TyBool
typeOf _ TmTrue = Right TyBool
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