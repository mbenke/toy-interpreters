module Tiny1.Syntax where

data Stmt
  = SExp Exp
  | String := Exp
  | SBlock [Stmt]
  deriving (Eq,Show)

data Exp
    = EInt Integer
    | EVar String
    | EAdd Exp Exp
      deriving(Eq,Show)

type Val = Integer

instance Num Exp where
    fromInteger = EInt
    (+) = EAdd
    (*) = undefined
    (-) = undefined
    signum = undefined
    abs = undefined
