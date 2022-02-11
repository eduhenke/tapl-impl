module Term where

data Term
  = Var String
  | Abs String Term
  | App Term Term
  deriving (Eq, Ord, Show)