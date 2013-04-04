module Ref1.Syntax where
import Data.Map(Map)
import qualified Data.Map as Map

import Control.Monad.Error
import Control.Monad.State

type Name = String
type Defs = [Def]
type Def = (Name,Exp)
type Param = Name

data Exp 
  = EVar Name
  | ENew
  | ELet Name Exp Exp
  | ELets Defs Exp
  | EIf Exp Exp Exp
  | EGet Name Name
  | ESet Name Name Exp
  | EInt Integer
  deriving Show  
           
data Type = TInt | TVar Name | TRec RecType 
type RecType = (Map Name Type)
newtype Constraint = Constraint (Name, Type)
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
  
instance Show Constraint where
  show (Constraint  (x,t)) = concat [x,"<",show t]
  
showRec :: RecType -> String 
showRec r = concat ["{",showFields fields,"}"] where
  fields = Map.toList r 
  showFields  = concatMap showField
  showField (l,t) = concat [l,":",show t]
  
instance Show Type where
  show TInt = "int"
  show (TRec r) = showRec r
  show (TVar v) = v
  
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
  
oneFieldRec :: Name -> Type -> Type
oneFieldRec a t = TRec $ Map.fromList [(a,t)]
