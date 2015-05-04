module Lambda1.Syntax where

type Name = String
data Exp = EInt Int | EVar Name 
         | ELam Name Exp | EApp Exp Exp
($$) = EApp
       
-- Przykładowe lambda-termy

mkI :: Exp
mkI = ELam "x" $ EVar "x"

mkK :: Exp
mkK = ELam "x" $ ELam "y" $ EVar "x"


mkS :: Exp
mkS = ELam "x" $ ELam "y" $ ELam "z"
          $ EApp 
             (EApp (EVar "x") (EVar "z")) 
             (EApp (EVar "y") (EVar "z")) 

k7 :: Exp
k7 = EApp mkK (EInt 7)

skk42 = mkS $$ mkK $$ mkK $$ EInt 42

-- kombinator omega nie typuje sie w prostym rachunku lambda
mkOmega :: Exp
mkOmega = ELam "x" $ EApp (EVar "x") (EVar "x")

kio = mkK $$ mkI $$ (mkOmega $$ mkOmega)

-- Show
showName :: Name -> ShowS
showName = showString

instance Show Exp where
  showsPrec d (EVar n) = showString n
  showsPrec d (EInt i) = showsPrec 10 i
  showsPrec d (EApp e1 e2) = showParen (d > ap_prec) $
             showsPrec (ap_prec) e1   .
             showString " "           .
             showsPrec (ap_prec+1) e2 
          where ap_prec = 10

  showsPrec d (ELam n e) = showParen (d > lam_prec) $
             showString ("\\"++n++"->") .
             showsPrec (lam_prec) e
          where lam_prec = 1
           
