module Simple2.Syntax where

type Name = String
data Stmt
  = SExp Exp
  | Name := Exp
  | SBlock [Stmt]
  | SPrint Exp
  | SVar Name   -- local variable
  deriving (Eq,Show)

data Exp
    = EInt Integer
    | EVar Name
    | EAdd Exp Exp
    | ELet Name Exp Exp
    | ELets [(Name,Exp)] Exp
    | EIf Exp Exp Exp
    | ENew Exp
    | EDeref Exp 
    | ENone
      deriving(Eq,Show)

instance Num Exp where
    fromInteger = EInt
    (+) = EAdd
    (*) = undefined
    (-) = undefined
    signum = undefined
    abs = undefined
