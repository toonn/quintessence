module Syntax where

type Name = String

data Expr
  = Var Name
  | Lit Literal
  | App Expr Expr
  | Lam Name Expr
  deriving (Eq, Show)

data Literal
  = LNat Integer
  | LBool Bool
  deriving (Eq, Show)

data Type
  = TNat
  | TBool
  | TArr Type Type
  deriving (Eq, Show)
