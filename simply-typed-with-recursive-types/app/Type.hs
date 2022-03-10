module Type where

import Data.List (intercalate)
import Data.Map (Map, keys, toList)

data Type
  = TyBool
  | TyNat
  | TyArrow Type Type
  | TyUnit
  | TyRecord (Map String Type)
  | TyVariant (Map String Type)
  | TyTop
  | TyVar Int Int
  | TyRecursive String Type
  deriving (Eq, Ord, Show)

-- instance Show Type where
--   show =
--     let show' ctx ty = case ty of
--           TyBool -> "Bool"
--           TyNat -> "Nat"
--           (TyArrow ty1 ty2) -> show' ctx ty1 ++ "->" ++ show' ctx ty2
--           TyUnit -> "Unit"
--           (TyRecord ts) ->
--             if keys ts == map show [1 .. (length $ keys ts)]
--               then "{" ++ intercalate ", " (map (\(l, t) -> show' ctx t) $ toList ts) ++ "}"
--               else "{" ++ intercalate ", " (map (\(l, t) -> l ++ ":" ++ show' ctx t) $ toList ts) ++ "}"
--           (TyVariant ts) -> "<" ++ intercalate ", " (map (\(l, t) -> l ++ ":" ++ show' ctx t) $ toList ts) ++ ">"
--           TyTop -> "Top"
--           (TyVar n _) -> if n < length ctx then ctx !! n else "<TyVar ???>"
--           (TyRecursive x body) -> "(µ" ++ x ++ "." ++ show' (x : ctx) body ++ ")"
--      in show' []