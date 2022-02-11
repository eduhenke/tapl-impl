module Eval where

import Term

freeVars :: Term -> [String]
freeVars (Var x) = [x]
freeVars (App t1 t2) = freeVars t1 ++ freeVars t2
freeVars (Abs x body) = filter (/= x) $ freeVars body

substitute :: Term -> Term -> Term -> Term
substitute x y (Var z)
  | y == Var z = x
  | otherwise = Var z
substitute x y (App a b) = App (substitute x y a) (substitute x y b)
substitute x y (Abs var body)
  | x == y = Abs var body
  | otherwise = Abs newVar $ substitute x y (substitute (Var var) (Var newVar) body)
  where
    genNewVar x = if x `elem` freeVars body then x ++ "'" else x
    newVar = genNewVar var

eval' :: Term -> Maybe Term
eval' (Var x) = Nothing
eval' (Abs var body) = Nothing
eval' (App t1 t2) = case t1 of
  (Abs var body) -> case eval' t2 of
    Just t2' -> Just $ App (Abs var body) t2'
    Nothing -> Just $ substitute t2 (Var var) body
  t -> eval' t

eval :: Term -> Term
eval t = case eval' t of
  Nothing -> t
  Just t' -> eval t'