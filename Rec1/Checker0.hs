module Rec1.Checker0 where
import Rec1.Syntax

import Data.Map(Map)
import qualified Data.Map as Map

import Control.Monad.Error
import Control.Monad.State

type Env = Map Name Type

emptyEnv :: Env
emptyEnv = Map.empty

extendEnv :: Name -> Type -> Env -> Env
extendEnv x t env = Map.insert x t env

data CheckState = CheckState { 
  cstCons :: [Constraint],
  cstFresh :: [Int]
}

instance Show CheckState where
  show cst = show $ cstCons cst
    
initState :: CheckState
initState = CheckState [] [1..]
  
type CM a = StateT CheckState (ErrorT String IO) a
runCM :: CM a -> CheckState -> IO (Either String (a,CheckState))
runCM m st = runErrorT (runStateT m st)


runCheck e = runCM (findType emptyEnv e) initState 
checkProg ds e = runCheck (ELets ds e)

freshInt :: CM Int
freshInt = do
  cst <- get
  let ints = cstFresh cst
  put $ cst { cstFresh = tail ints }
  return $ head ints
  
freshName :: String -> CM String
freshName s = do 
  i <- freshInt
  return $ s ++ show i

addConstraint :: Constraint -> CM ()
addConstraint c = do
  cst <- get
  put $ cst { cstCons = c:cstCons cst }
  
freshTVar = freshName "X"

emptyRecType :: Type
emptyRecType = TRec Map.empty

envType :: Env -> Name -> CM Type
envType env x = case Map.lookup x env of
  Nothing -> throwError $ "Unknown var "++x
  Just t -> return t
  
findType :: Env -> Exp -> CM Type
findType env (EVar x) = envType env x 
findType env ENew = do
  t <- freshTVar
  addConstraint $ Constraint (t,emptyRecType)
  return $ TVar t
findType env (ELet x e1 e0) = do  
  t1 <- findType env e1
  let env' = extendEnv x t1 env
  findType env' e0
findType env (ELets [] e0) = findType env e0
findType env (ELets ((x,e):ds) e0) = findType env (ELet x e (ELets ds e0))
findType env (ESet x a e) = do
  TVar tX <- envType env x
  t2 <- findType env e
  addConstraint $ Constraint (tX,oneFieldRec a t2)
  return (TVar tX)
