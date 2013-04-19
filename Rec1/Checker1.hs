module Rec1.Checker1 where
import Rec1.Syntax

import Data.Map(Map)
import qualified Data.Map as Map

import Control.Monad.Error
import Control.Monad.State
import Control.Monad.Identity

import Control.Applicative

type Env = Map Name Type

emptyEnv :: Env
emptyEnv = Map.empty

extendEnv :: Name -> Type -> Env -> Env
extendEnv x t env = Map.insert x t env

-- newtype Constraint = Constraint (Name, Type)
type Constraints = Map Name Type
noConstraints :: Constraints
noConstraints = Map.empty

oneConstraint :: Name -> Type -> Constraints
oneConstraint n t = Map.fromList [(n,t)]

data Typing  = Typing {tyPre :: Constraints,tyTy :: Type, tyPost :: Constraints}
  deriving Show

pureType :: Type -> Typing
pureType t = Typing noConstraints t noConstraints

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

test1 =  evalCM (findType (EInt 1) emptyEnv) initState 
test2 =  evalCM (findType (ENew) emptyEnv) initState 