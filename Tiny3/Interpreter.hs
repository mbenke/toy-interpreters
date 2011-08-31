{-# LANGUAGE MultiParamTypeClasses, NamedFieldPuns #-}
module Tiny3.Interpreter where
import Tiny3.Syntax

import qualified Data.Map as Map
import Control.Monad.Error
import Control.Monad.State
import Control.Monad.Trans
import Control.Monad.Identity

type Name = String

type Store = Map.Map Loc Val
initStore :: Store
initStore = Map.empty

type Loc = Int

type Env = [Map.Map Name Loc]  -- a stack of maps
initEnv :: Env
initEnv = [Map.empty]

data IntState = IntState { 
  store :: Store, 
  freeLocs :: [Loc], 
  env :: Env }

initState = IntState initStore [1..2^16] initEnv

type IM a = StateT IntState (ErrorT String IO) a
runIM :: IM a -> IntState -> IO (Either String (a,IntState))
runIM m st = runErrorT (runStateT m st)

runProg :: Stmt -> IO ()
runProg p = do
  res <- runIM (exec p) initState
  case res of
    Left e -> putStrLn e
    Right (a,state) -> print $ Map.toAscList (store state)

getStore :: IM Store  
getStore = gets store

getFreeLocs :: IM [Loc]
getFreeLocs = gets freeLocs

putFreeLocs :: [Loc] -> IM ()
putFreeLocs freeLocs = modify $ \r -> r { freeLocs }

getEnv :: IM Env
getEnv = gets env

putEnv :: Env -> IM ()
putEnv env = modify $ \r -> r { env }

modifyEnv :: (Env -> Env) -> IM ()
modifyEnv f = getEnv >>= putEnv . f

enterScope :: IM ()
enterScope = modifyEnv (Map.empty:)

leaveScope :: IM ()
leaveScope = do
  (current:olderEnv) <- getEnv
  -- mapM_ free (Map.keys current)
  putEnv olderEnv

mapHead :: (a->a) -> [a] -> [a]
mapHead f (x:xs) = f x:xs
mapHead f [] = []

whenNothing :: Maybe a -> Maybe a -> Maybe a
whenNothing Nothing y = y
whenNothing x y = x

lookupEnv :: Name -> Env -> Maybe Loc
lookupEnv n [] = Nothing
lookupEnv n (e:es) = Map.lookup n e `whenNothing` lookupEnv n es
  
updateEnv :: Name -> Loc -> Env -> Env
updateEnv n l = mapHead (Map.insert n l)

alloc :: IM Loc
alloc = do
  locs <- getFreeLocs
  case locs of
    [] -> throwError "alloc: no more free locs"
    (l:ls) -> putFreeLocs ls >> return l

free :: Loc -> IM ()
free l = do 
  ls <- getFreeLocs
  putFreeLocs (l:ls)


getNameLoc :: Name -> IM Loc
getNameLoc n =  do
  env <- getEnv
  let res  = lookupEnv n env
  maybe (throwError $ unwords["Undefined var",n,"env is",show env]) return res
  
getVar :: Name -> IM Val
getVar v =  do
  loc <- getNameLoc v
  store <- getStore 
  let res  = Map.lookup loc store
  maybe (throwError $ "Unallocated var"++ v) return res

updateStore :: Loc -> Val -> IM ()
updateStore v x = modify $ \state -> state { 
  store = Map.insert v x (store state)}

eval :: Exp -> IM Val
eval (EInt i) = return i
eval (EVar s) = getVar s
eval (EAdd e1 e2) = liftM2 (+) (eval e1) (eval e2)

exec :: Stmt -> IM ()
exec (SVar n) = do
  l <- alloc
  (current:olderEnv) <- getEnv
  modifyEnv (updateEnv n l)
  
  return ()
exec (SBlock ss)= enterScope >> mapM_ exec ss >> leaveScope

exec (v := e) = do
  x <- eval e
  loc <- getNameLoc v
  updateStore loc x
exec (SExp e) = eval e >>= liftIO . print 
exec (SPrint e) = eval e >>= liftIO . print 
-- exec x = error $ "exec unimplemented for " ++ show x

test :: IO()
test = runProg prog1
