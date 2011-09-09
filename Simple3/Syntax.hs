module Simple3.Syntax where

type Name = String
type Defs = [Def]
type Def = (Name,Exp)

data Exp
    = EInt Integer
    | EVar Name
    | EAdd Exp Exp
    | ELet Name Exp Exp
    | ELets Defs Exp
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
