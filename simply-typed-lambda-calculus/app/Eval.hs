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
        TmCase t cases -> TmCase (walk c t) (Data.Map.map (second (walk (c + 1))) cases)
        TmVariant t l ty -> TmVariant (walk c t) l ty
        TmFix t -> TmFix (walk c t)
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
isVal _ = False

betaReduction :: Term -> Term -> Term
betaReduction body newVal = shift (-1) (subst (shift 1 newVal) 0 body)

-- single-step
eval' :: Term -> Maybe Term
eval' (App t1 t2)
  -- t1 is not value: E-App1
  | not (isVal t1) = Just $ App (eval t1) t2
  -- t2 is not value: E-App2
  | not (isVal t2) = Just $ App t1 (eval t2)
  -- t1 and t2 are values
  | otherwise = case t1 of
    -- t1 is Abs and t2 is value: E-AppAbs
    (Abs _ _ t12) -> Just $ betaReduction t12 t2
    _ -> fail "Typecheck should catch this part: function application to a non-function"
eval' (TmIf TmTrue t f) = Just t
eval' (TmIf TmFalse t f) = Just f
eval' (TmIf c t f) = Just $ TmIf (eval c) t f
eval' (TmSucc t) = TmSucc <$> eval' t -- E-Succ
eval' (TmPred TmZero) = Just TmZero -- E-PredZero
eval' (TmPred (TmSucc t)) = Just t -- E-PredSucc
eval' (TmPred t) = TmPred <$> eval' t -- E-Pred
eval' (TmIsZero TmZero) = Just TmTrue -- E-IszeroZero
eval' (TmIsZero (TmSucc t)) | isNumVal t = Just TmFalse -- E-IszeroSucc
eval' (TmIsZero t) = Just $ TmIsZero (eval t) -- E-IsZero
eval' (TmAscription t ty)
  | isVal t = Just t
  | otherwise = Just $ TmAscription (eval t) ty
eval' (TmLet x t1 t2)
  -- it's zero, because when parsing we add another entry to the context
  | isVal t1 = Just $ betaReduction t2 t1
  | otherwise = Just $ TmLet x (eval t1) t2
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
eval' (TmCase t@(TmVariant v l ty) cases)
  -- E-CaseVariant
  | isVal v =
    let (x, t') = cases ! l
     in Just $ betaReduction t' v
  -- E-Case
  | otherwise = Just $ TmCase (eval t) cases
eval' (TmCase t cases) =
  -- E-Case
  Just $ TmCase (eval t) cases
eval' (TmVariant t l ty)
  -- i've added this rule, because i've treated a variant as a value. this is not in the TaPL book
  | isVal t = Nothing
  | otherwise =
    -- E-Variant
    Just $ TmVariant (eval t) l ty
eval' t@(TmFix (Abs x ty t2)) =
  return $ betaReduction t2 t
eval' (TmFix t) = do
  -- E-Fix
  t' <- eval' t
  return $ TmFix t'
eval' _ = Nothing

eval :: Term -> Term
eval t = case eval' t of
  Nothing -> t
  Just t' -> eval t'