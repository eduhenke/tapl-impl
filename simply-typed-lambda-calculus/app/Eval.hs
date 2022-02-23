module Eval where

import Term

shift :: Int -> Term -> Term
shift =
  let shift' c d (Var n l)
        | n >= c = Var (n + d) (l + d)
        | otherwise = Var n (l + d)
      shift' c d (Abs x ty t) = Abs x ty $ shift' (c + 1) d t
      shift' c d (App t1 t2) = App (shift' c d t1) (shift' c d t2)
      shift' c d (TmIf cnd t f) = TmIf (shift' c d cnd) (shift' c d t) (shift' c d f)
      shift' c d (TmLet x t1 t2) = TmLet x (shift' c d t1) (shift' (c + 1) d t2)
      shift' c d t = t
   in shift' 0

subst :: Term -> Int -> Term -> Term
subst s j k@(Var n l)
  | j == n = s
  | otherwise = k
subst s j (TmIf c t f) = TmIf (subst s j c) (subst s j t) (subst s j f)
subst s j (Abs x ty t) = Abs x ty (subst (shift 1 s) (j + 1) t)
subst s j (App t1 t2) = App (subst s j t1) (subst s j t2)
subst s j (TmLet x t1 t2) = TmLet x (subst s j t1) (subst (shift 1 s) (j + 1) t2)
subst s j t = t

isVal :: Term -> Bool
isVal TmTrue = True
isVal TmFalse = True
isVal Abs {} = True
isVal TmUnit = True
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
eval' (TmAscription t ty)
  | isVal t = Just t
  | otherwise = Just $ TmAscription (eval t) ty
eval' (TmLet x t1 t2)
  -- it's zero, because when parsing we add another entry to the context
  | isVal t1 = Just $ betaReduction t2 t1
  | otherwise = Just $ TmLet x (eval t1) t2
eval' _ = Nothing

eval :: Term -> Term
eval t = case eval' t of
  Nothing -> t
  Just t' -> eval t'