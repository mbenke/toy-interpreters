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
          deriving Eq

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

renameVars :: Int -> Term -> Term
renameVars lev (Var (Id s n)) = Var (Id s lev)
renameVars lev (Struct s ts) = Struct s (map (renameVars lev) ts)

data Clause = Term :- [Term]
              deriving Show
                       
data Database = Db [(Atom,[Clause])]

clausesFor :: Atom -> Database  -> [Clause]
clausesFor a (Db rss) = concat [ cs | (a1,cs) <- rss, a == a1]

renClauses :: Database -> Int -> Term -> [Clause]
renClauses db n (Struct a _) = [ r tm :- map r tp | (tm :- tp) <- clausesFor a db ]
           where r = renameVars n

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

solution :: [Id] -> Subst -> [String]
solution ids s = [unwords[show id,"=",show t] | (id,t) <- [(i,s i) | i <- ids], t /=Var id] 
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

-- # Proof Tree

data Prooftree = Done Subst | Choice [Prooftree]

prooftree :: Database -> Int -> Subst -> [Term] -> Prooftree
-- for each clause with head unifiable against first goal,
-- get new goal list: add clause body at FRONT of goals
-- (to get depth first), and apply unifier; also
-- update accumulated substitution
prooftree db = pt where
      -- Depth, part res, goals
   pt :: Int -> Subst -> [Term] -> Prooftree
   pt n s [] = Done s
   pt n s (g:gs) = Choice [ pt (n+1) (u@@s) (mapApply u (tp++gs) )
                          | tm :- tp <- renClauses db n g
                          , u <- unify g tm
                          ]


search :: Prooftree -> [Subst]
search (Done s) = [s]
search (Choice pts) = [s | pt <- pts, s <- search pt]

prove :: Database -> [Term] -> [Subst]
prove db = search . prooftree db 1 nullSubst