-- Based on www.inf.ed.ac.uk/teaching/courses/lp/2009/slides/lp_theory6.pdf
-- which in turn bases on http://darcs.haskell.org/nofib/real/prolog/
module Prolog1.MarkJones where

data Id = Id String Int
    deriving Eq
             
instance Show Id where
  show (Id s 0) = s
  show (Id s n) = s ++ show n
  
{-
newtype Atom = Atom {unAtom :: String}
   deriving Eq
            
instance Show Atom where
  showsPrec _ = showString . unAtom
-}

type Atom = String
showsAtom :: Atom -> ShowS
showsAtom = showString

data Term = Var Id
          | Struct Atom [Term]
term0 :: Atom -> Term
term0 a = Struct a []

var0 :: String -> Term 
var0 s = Var $ Id s 0

instance Show Term where
  showsPrec _ (Var id) = shows id
  showsPrec _ (Struct a []) = showsAtom a
  showsPrec _ (Struct a ts) = showsAtom a . showParen True (showList ts) 
  showList [] = id
  showList ts = foldr1 (\s r -> s . showChar ',' . r) $ map shows ts

data Clause = Term :- [Term]
              deriving Show
                       
data Database = Db [(Atom,[Clause])]

-- # Substitutions 

type Subst = Id -> Term

apply :: Subst -> Term -> Term
apply s (Var i) = s i
apply s (Struct a ts) = Struct a (mapApply s ts)

mapApply :: Subst -> [Term] -> [Term]
mapApply = map . apply

nullSubst :: Subst
nullSubst i = Var i

(->>) :: Id -> Term -> Subst
(->>) i t j | j == i = t
            | otherwise = Var j

(@@) :: Subst -> Subst -> Subst
s1 @@ s2 = apply s1 . s2

unify :: Term -> Term -> [Subst]
unify (Var x) (Var y) = if x == y then [nullSubst] else [x->>Var y]
unify (Var x) t = [x ->> t] -- OBS no occur check
unify t (Var x) = [x ->> t]
unify (Struct a ts) (Struct b ss) = [u | a==b, u <- listUnify ts ss]

listUnify :: [Term] -> [Term] -> [Subst]
listUnify [] [] = [nullSubst]
listUnify (t:ts) (r:rs) = [u2 @@ u1 
                          | u1 <- unify t r
                          , u2 <- listUnify (mapApply u1 ts)
                                            (mapApply u1 rs)
                          ]                                                            
listUnify _  _ = []