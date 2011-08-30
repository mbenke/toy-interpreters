module Tiny1.Syntax where

prog1 :: Stmt
prog1 = SBlock [ 
  "x" := 1,
  "y" := 2,
  "c" := (EVar "x"+ EVar "y")
  ]

-- undefined var
bad1 = SExp $ EVar "x"

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
