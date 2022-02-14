module Eval where

import Term

shift :: Int -> Term -> Term
shift =
  let shift' c d (Var n l)
        | n >= c = Var (n + d) (l + d)
        | otherwise = Var n (l + d)
      shift' c d (Abs x ty t) = Abs x ty $ shift' (c + 1) d t
      shift' c d (App t1 t2) = App (shift' c d t1) (shift' c d t2)
      shift' c d TmTrue = TmTrue
      shift' c d TmFalse = TmFalse
      shift' c d (TmIf cnd t f) = TmIf (shift' c d cnd) (shift' c d t) (shift' c d f)
   in shift' 0

subst :: Term -> Int -> Term -> Term
subst s j k@(Var n l)
  | j == n = s
  | otherwise = k
subst s j TmTrue = TmTrue
subst s j TmFalse = TmFalse
subst s j (TmIf c t f) = TmIf (subst s j c) (subst s j t) (subst s j f)
subst s j (Abs x ty t) = Abs x ty (subst (shift 1 s) (j + 1) t)
subst s j (App t1 t2) = App (subst s j t1) (subst s j t2)

isVal :: Term -> Bool
isVal TmTrue = True
isVal TmFalse = True
isVal Abs {} = True
isVal _ = False

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
    (Abs _ _ t12) -> Just $ shift (-1) (subst (shift 1 t2) 0 t12)
    _ -> fail "Typecheck should catch this part: function application to a non-function"
eval' (TmIf TmTrue t f) = Just t
eval' (TmIf TmFalse t f) = Just f
eval' (TmIf c t f) = Just $ TmIf (eval c) t f
eval' _ = Nothing

eval :: Term -> Term
eval t = case eval' t of
  Nothing -> t
  Just t' -> eval t'