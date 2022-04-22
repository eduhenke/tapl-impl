module Eval where

import Data.Bifunctor (second)
import Data.List (find)
import Data.Map (fromList, map, toList, (!))
import Term
import Type
import qualified Typecheck as T

termWalk :: Term -> (Int -> Int -> Int -> Term) -> (Int -> Type -> Type) -> Int -> Term
termWalk t onVar onType c =
  let walk c t = case t of
        Var x n -> onVar c x n
        TmIf cnd t f -> TmIf (walk c cnd) (walk c t) (walk c f)
        TmSucc t -> TmSucc (walk c t)
        TmPred t -> TmPred (walk c t)
        TmIsZero t -> TmIsZero (walk c t)
        Abs x ty t -> Abs x (onType c ty) (walk (c + 1) t)
        App t1 t2 -> App (walk c t1) (walk c t2)
        TmLet x t1 t2 -> TmLet x (walk c t1) (walk (c + 1) t2)
        TmRecord ts -> TmRecord (Data.Map.map (walk c) ts)
        TmProj t prop -> TmProj (walk c t) prop
        TmTyAbs x tyT1 t -> TmTyAbs x (onType c tyT1) (walk c t)
        TmTyApp t ty -> TmTyApp (walk c t) (onType c ty)
        t -> t
   in walk c t

shift :: Int -> Term -> Term
shift d t = termWalk t onVar onType 0
  where
    onVar c x n
      | x >= c = Var (x + d) (n + d)
      | otherwise = Var x (n + d)
    onType c = Type.tyShift d

subst :: Term -> Int -> Term -> Term
subst s j t = termWalk t onVar onType j
  where
    onVar c x n
      | c == x = s
      | otherwise = Var x n
    onType c = id

substTy :: String -> Term -> Type -> Term
substTy x t ty = termWalk t onVar onType 0
  where
    onVar c = Var
    onType c ty' = Type.tySubstTop ty' ty

isNumVal :: Term -> Bool
isNumVal TmZero = True
isNumVal (TmSucc t) = isNumVal t
isNumVal _ = False

isVal :: Term -> Bool
isVal TmTrue = True
isVal TmFalse = True
isVal TmZero = isNumVal TmZero
isVal (TmSucc t) = isNumVal t
isVal Abs {} = True
isVal (TmRecord ts) = all isVal ts
isVal (TmTyAbs {}) = True
isVal _ = False

betaReduction :: Term -> Term -> Term
betaReduction body newVal = shift (-1) (subst (shift 1 newVal) 0 body)

-- single-step
eval' :: Term -> Maybe Term
eval' (App (Abs _ _ t12) t2)
  | isVal t2 =
    -- t1 is Abs and t2 is value: E-AppAbs
    Just $ betaReduction t12 t2
eval' (App t1 t2)
  | isVal t1 = App t1 <$> eval' t2 -- E-App2
  | otherwise = (`App` t2) <$> eval' t1 -- E-App1
eval' (TmIf TmTrue t f) = Just t
eval' (TmIf TmFalse t f) = Just f
eval' (TmIf c t f) = (\c -> TmIf c t f) <$> eval' c
eval' (TmSucc t) = TmSucc <$> eval' t -- E-Succ
eval' (TmPred TmZero) = Just TmZero -- E-PredZero
eval' (TmPred (TmSucc t)) = Just t -- E-PredSucc
eval' (TmPred t) = TmPred <$> eval' t -- E-Pred
eval' (TmIsZero TmZero) = Just TmTrue -- E-IszeroZero
eval' (TmIsZero (TmSucc t)) | isNumVal t = Just TmFalse -- E-IszeroSucc
eval' (TmIsZero t) = TmIsZero <$> eval' t -- E-IsZero
eval' (TmLet x t1 t2)
  -- it's zero, because when parsing we add another entry to the context
  | isVal t1 = Just $ betaReduction t2 t1
  | otherwise = (\t1 -> TmLet x t1 t2) <$> eval' t1
eval' (TmProj t i) = case t of
  (TmRecord ts) ->
    Just $
      if all isVal ts
        then ts ! i -- E-ProjTuple
        else TmProj (eval t) i -- E-Proj
  _ -> Just $ TmProj (eval t) i -- E-Proj
eval' t@(TmRecord ts)
  | isVal t = Nothing
  -- E-Tuple
  | otherwise = Just $ TmRecord (fromList $ evalFirstNonVal (toList ts))
  where
    evalFirstNonVal :: [(String, Term)] -> [(String, Term)]
    evalFirstNonVal [] = []
    evalFirstNonVal ((label, t) : ts)
      | isVal t = (label, t) : evalFirstNonVal ts
      | otherwise = (label, eval t) : ts
eval' (TmTyApp (TmTyAbs x tyT1 body) argTy) = return $ substTy x body argTy
eval' (TmTyApp t ty) = (`TmTyApp` ty) <$> eval' t
eval' _ = Nothing

eval :: Term -> Term
eval t = case eval' t of
  Nothing -> t
  Just t' -> eval t'