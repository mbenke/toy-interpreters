module Tiny4.Syntax where

prog1 :: Stmt
prog1 = SBlock [ 
  SVar "x",
  SVar "y",
  "x" := 1,
  SBlock [ SVar "x", "x" := 42 ], -- local variable
  "y" := 2,
  SPrint (EVar "x"+ EVar "y")
  ]

-- undefined var
bad1 = SExp $ EVar "x"

data Stmt
  = SExp Exp
  | String := Exp
  | SBlock [Stmt]
  | SPrint Exp
  | SVar String   -- local variable
  deriving (Eq,Show)

data Exp
    = EInt Integer
    | EVar String
    | EAdd Exp Exp
      deriving(Eq,Show)

instance Num Exp where
    fromInteger = EInt
    (+) = EAdd
    (*) = undefined
    (-) = undefined
    signum = undefined
    abs = undefined
