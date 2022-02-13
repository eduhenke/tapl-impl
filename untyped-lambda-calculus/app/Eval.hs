module Eval where

import Term

shift' :: Int -> Int -> Term -> Term
shift' c d (Var n l)
  | n >= c = Var (n + d) (l + d)
  | otherwise = Var n (l + d)
shift' c d (Abs x t) = Abs x $ shift' (c + 1) d t
shift' c d (App t1 t2) = App (shift' c d t1) (shift' c d t2)

shift :: Int -> Term -> Term
shift = shift' 0

subst :: Term -> Int -> Term -> Term
subst s j k@(Var n l)
  | j == n = s
  | otherwise = k
subst s j (Abs x t) = Abs x (subst (shift 1 s) (j + 1) t)
subst s j (App t1 t2) = App (subst s j t1) (subst s j t2)

eval' :: Term -> Maybe Term
eval' (Var _ _) = Nothing
eval' (Abs _ _) = Nothing
eval' (App t1 t2) = case t1 of
  -- t1 is a value
  (Abs _ t12) -> case t2 of
    -- t2 is a value: E-AppAbs
    v1@(Abs _ _) -> Just $ shift (-1) (subst (shift 1 t2) 0 t12)
    -- t2 is not a value: E-App2
    _ -> Just $ App t1 (eval t2)
  -- t1 is not a value: E-App1
  _ -> Just $ App (eval t1) t2

eval :: Term -> Term
eval t = case eval' t of
  Nothing -> t
  Just t' -> eval t'