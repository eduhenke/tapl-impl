module Kind where

data Kind
  = KindProper
  | KindOperator Kind Kind
  deriving (Eq, Ord)

instance Show Kind where
  show KindProper = "*"
  show (KindOperator k1@(KindOperator {}) k2) = "(" ++ show k1 ++ ")=>" ++ show k2
  show (KindOperator k1 k2) = show k1 ++ "=>" ++ show k2
