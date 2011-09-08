module Simple1.Syntax where

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
    | ELet String Exp Exp
    | ELets [(String,Exp)] Exp
    | EIf Exp Exp Exp
      deriving(Eq,Show)

instance Num Exp where
    fromInteger = EInt
    (+) = EAdd
    (*) = undefined
    (-) = undefined
    signum = undefined
    abs = undefined
