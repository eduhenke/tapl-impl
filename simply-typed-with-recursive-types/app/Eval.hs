module Eval where

import Data.Bifunctor (second)
import Data.List (find)
import Data.Map (fromList, map, toList, (!))
import Term

termWalk :: Term -> (Int -> Int -> Int -> Term) -> Int -> Term
termWalk t onVar c =
  let walk c t = case t of
        Var x n -> onVar c x n
        TmIf cnd t f -> TmIf (walk c cnd) (walk c t) (walk c f)
        TmSucc t -> TmSucc (walk c t)
        TmPred t -> TmPred (walk c t)
        TmIsZero t -> TmIsZero (walk c t)
        Abs x ty t -> Abs x ty (walk (c + 1) t)
        App t1 t2 -> App (walk c t1) (walk c t2)
        TmLet x t1 t2 -> TmLet x (walk c t1) (walk (c + 1) t2)
        TmRecord ts -> TmRecord (Data.Map.map (walk c) ts)
        TmProj t prop -> TmProj (walk c t) prop
        TmCase t cases -> TmCase (walk c t) (Data.Map.map (second (walk (c + 1))) cases)
        TmVariant t l ty -> TmVariant (walk c t) l ty
        TmFold t ty -> TmFold (walk c t) ty
        TmUnfold t ty -> TmUnfold (walk c t) ty
        t -> t
   in walk c t

shift :: Int -> Term -> Term
shift d t = termWalk t onVar 0
  where
    onVar c x n
      | x >= c = Var (x + d) (n + d)
      | otherwise = Var x (n + d)

subst :: Term -> Int -> Term -> Term
subst s j t = termWalk t onVar j
  where
    onVar c x n
      | c == x = s
      | otherwise = Var x n

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
isVal TmUnit = True
isVal (TmRecord ts) = all isVal ts
-- i've added variant as a value. this is not in the TaPL book
isVal (TmVariant t _ _) = isVal t
isVal (TmFold t ty) = isVal t
isVal (App (TmFold t ty) v) = True
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
eval' (TmAscription t ty)
  | isVal t = Just t
  | otherwise = (`TmAscription` ty) <$> eval' t
eval' (TmLet x t1 t2)
  -- it's zero, because when parsing we add another entry to the context
  | isVal t1 = Just $ betaReduction t2 t1
  | otherwise = (\t1 -> TmLet x t1 t2) <$> eval' t1
eval' (TmProj t i) = case t of
  (TmRecord ts) ->
    if all isVal ts
      then Just $ ts ! i -- E-ProjTuple
      else (`TmProj` i) <$> eval' t -- E-Proj
  _ -> (`TmProj` i) <$> eval' t -- E-Proj
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
eval' (TmCase t@(TmVariant v l ty) cases)
  -- E-CaseVariant
  | isVal v =
    let (x, t') = cases ! l
     in Just $ betaReduction t' v
eval' (TmCase t cases) =
  -- E-Case
  (`TmCase` cases) <$> eval' t
eval' (TmVariant t l ty)
  -- i've added this rule, because i've treated a variant as a value. this is not in the TaPL book
  | isVal t = Nothing
  | otherwise =
    -- E-Variant
    (\t -> TmVariant t l ty) <$> eval' t
eval' (TmUnfold (TmFold v1 tyT) tyS) | isVal v1 = pure v1 -- E-UnfldFld
eval' (TmFold t ty) = do
  t' <- eval' t
  pure $ TmFold t' ty -- E-Fld
eval' (TmUnfold t ty) = do
  t' <- eval' t
  pure $ TmUnfold t' ty -- E-Unfld
eval' _ = Nothing

eval :: Term -> Term
eval t = case eval' t of
  Nothing -> t
  Just t' -> eval t'