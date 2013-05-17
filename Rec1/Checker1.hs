module Rec1.Checker1 where
import Rec1.Syntax

import Data.Map(Map)
import qualified Data.Map as Map

import Control.Monad.Error
import Control.Monad.State
import Control.Monad.Identity

import Control.Applicative


-- * Typing

data Typing  = Typing {tyPre :: Constraints,tyTy :: Type, tyPost :: Constraints}
--  deriving Show
instance Show Typing where
    show (Typing pre typ post) = unwords [showConstraints pre, "=>", show typ, ";", 
                                          showConstraints post]

pureType :: Type -> Typing
pureType t = Typing noConstraints t noConstraints

-- * Checker Monad

type Env = Map Name Type

emptyEnv :: Env
emptyEnv = Map.empty

extendEnv :: Name -> Type -> Env -> Env
extendEnv x t env = Map.insert x t env

data CheckState = CheckState { 
  cstFresh :: Int
}

initState :: CheckState
initState = CheckState 0

cstTick :: CheckState -> CheckState
cstTick (CheckState i) = CheckState $ i+1

freshInt :: CM Int
freshInt = do
  modify cstTick
  gets cstFresh
         
freshName :: String -> CM Name
freshName s = (s++) . show <$> freshInt
          
type CM a = StateT CheckState (ErrorT String Identity) a
runCM :: CM a -> CheckState ->  (Either String (a,CheckState))
runCM m st = runIdentity $ runErrorT (runStateT m st)

evalCM :: CM a -> CheckState -> (Either String a)
evalCM m st = runIdentity $ runErrorT (evalStateT m st)

lookupEnv :: Name -> Env ->  CM Type
lookupEnv n e = case Map.lookup n e of
          Just t -> return t
          Nothing -> throwError (n ++ " not found in env")

findType :: Exp -> Env -> CM Typing
findType (EVar v) env = pureType <$> lookupEnv v env
findType (EInt _) env = return $ pureType TInt
findType (ENew)   env = typ <$> freshName "X" where
         typ x = Typing noConstraints (TVar x) (oneConstraint x emptyRec)

findType (ELets [] e) env = findType e env
findType (ELets ((x,e1):ds) e0) env = findType (ELet x e1 (ELets ds e0)) env

findType (ELet x e1 e0) env = do
         ting1@(Typing psi11 t1 psi12) <- findType e1 env
         let env' = extendEnv x t1 env
         ting0 <- findType e0 env'
         return $ seqTypings ting1 ting0

findType e env = error $ "FindType unimplemented for " ++ show e

backPatchCons :: Constraints -> Constraints -> Constraints -> Constraints
backPatchCons psi01 psi11 psi12 = psi01 -- FIXME 

seqTypings :: Typing -> Typing -> Typing
seqTypings (Typing pre1 t1 post1) (Typing pre0 t0 post0) = 
   Typing pre2 t0 post2 where
           pre2 = pre0
           post2 = post1 `updateConstraints` post0
           