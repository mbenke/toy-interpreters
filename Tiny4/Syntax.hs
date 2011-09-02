module Tiny4.Syntax where

prog1 :: Stmt
prog1 = SBlock [ 
  SVar "o",
  LV "o" := EVar "object", 
  SPrint $ EDot (EVar "o") "__doc__",
  LhsDot (LV "o") "__doc__" :=  42,
  SPrint $ EDot (EVar "o") "__doc__",
  SVar "x",
  SVar "y",
  LV "x" := 1,
  SBlock [ SVar "x", LV "x" := 42 ], -- local variable
  LV "y" := 2,
  SPrint (EVar "x"+ EVar "y")
  ]

-- undefined var
bad1 = SExp $ EVar "x"

data Stmt
  = SExp Exp
  | Lhs := Exp
  | SBlock [Stmt]
  | SPrint Exp
  | SVar String   -- local variable
  deriving (Eq,Show)

data Lhs = LV String | LhsDot Lhs String deriving(Eq, Show)
data Exp
    = EInt Integer
    | EVar String
    | EAdd Exp Exp
    | EDot Exp String
      deriving(Eq,Show)

instance Num Exp where
    fromInteger = EInt
    (+) = EAdd
    (*) = undefined
    (-) = undefined
    signum = undefined
    abs = undefined
