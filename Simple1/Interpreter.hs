{-# LANGUAGE MultiParamTypeClasses, NamedFieldPuns #-}
-- Tiny3.Interpreter2 extended with if and let

module Simple1.Interpreter where
import Simple1.Syntax

import qualified Data.Map as Map
import Control.Monad.Error
import Control.Monad.State
import Control.Monad.Trans
import Control.Monad.Identity
import Data.Maybe(catMaybes)

data IntState = IntState { 
  store :: Store, 
  freeLocs :: [Loc], 
  env :: Env,
  scopes :: Scopes}

initState = IntState initStore initFreeLocs initEnv initScopes

type IM a = StateT IntState (ErrorT String IO) a
runIM :: IM a -> IntState -> IO (Either String (a,IntState))
runIM m st = runErrorT (runStateT m st)

runProg :: Stmt -> IO ()
runProg p = do
  res <- runIM (exec p) initState
  case res of
    Left e -> putStrLn e
    Right (a,state) -> printState state
    
printState :: IntState -> IO ()
printState state = do
  putStr "Env: "
  printEnv (env state)
  putStr "Store: "
  print $ Map.toAscList (store state)

-- * Values

type Val = Integer

isTrueVal :: Val -> Bool
isTrueVal 0 = False
isTrueVal _ = True

-- * Store    
    
type Loc = Int
type Store = Map.Map Loc Val
initStore :: Store
initStore = Map.empty

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

updateStore :: Loc -> Val -> IM ()
updateStore v x = modify $ \state -> state { 
  store = Map.insert v x (store state)}

getStore :: IM Store  
getStore = gets store

initFreeLocs :: [Loc]
initFreeLocs = [1..2^16]

getFreeLocs :: IM [Loc]
getFreeLocs = gets freeLocs

putFreeLocs :: [Loc] -> IM ()
putFreeLocs freeLocs = modify $ \r -> r { freeLocs }

-- * Names and Environment
type Name = String
type Env = Map.Map Name [Loc]  -- a map name -> loc stack
initEnv :: Env
initEnv = Map.empty

printEnv :: Env -> IO ()
printEnv env = print $ Map.toAscList env

type Scope = [Name]
type Scopes = [Scope]

emptyScope = []
initScopes = [emptyScope]

getEnv :: IM Env
getEnv = gets env

putEnv :: Env -> IM ()
putEnv env = modify $ \r -> r { env }

modifyEnv :: (Env -> Env) -> IM ()
modifyEnv f = getEnv >>= putEnv . f

getScopes ::  IM Scopes
getScopes = gets scopes

putScopes :: Scopes -> IM ()
putScopes scopes = modify $ \r -> r { scopes }

modifyScopes :: (Scopes -> Scopes) -> IM ()
modifyScopes f = getScopes >>= putScopes . f

popScope :: IM Scope
popScope = do
  (scope:scopes) <- getScopes
  putScopes scopes
  return scope

enterScope :: IM ()
enterScope = modifyScopes (emptyScope:)

leaveScope :: IM ()
leaveScope = do
  env <- getEnv
  scope <- popScope
  -- TODO: free scope vars
  let scopeLocs = catMaybes $ map (flip lookupEnv env) scope
  mapM_ free scopeLocs
  let env' = foldr (\n e -> Map.update pop n e)env scope
  putEnv env' where
    pop :: [Loc] -> Maybe [Loc]
    pop [] = Nothing
    pop (x:xs) = Just xs

createVar :: Name -> IM Loc
createVar n = do
     l <- alloc
     modifyEnv (updateEnv n l)
     modifyScopes (addLocal n)
     return l

addLocal :: Name -> Scopes -> Scopes
addLocal n (h:t) =(n:h):t 
addLocal n [] = []

lookupEnv :: Name -> Env -> Maybe Loc
lookupEnv n e = do
  stack <- Map.lookup n e 
  case stack of
    [] -> Nothing
    (l:_) -> return l
  
updateEnv :: Name -> Loc -> Env -> Env
updateEnv n l = Map.insertWith (++) n [l] 

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

-- | Evaluate expressions
eval :: Exp -> IM Val
eval (EInt i) = return i
eval (EVar s) = getVar s
eval (EAdd e1 e2) = liftM2 (+) (eval e1) (eval e2)
eval (EIf e1 e2 e3) = do
     v1 <- eval e1
     if isTrueVal v1 then eval e2 else eval e3
eval (ELet n e1 e0) = do
     enterScope
     execDecl n e1
     v0 <- eval e0
     leaveScope
     return v0
eval (ELets ds e0) = do
     enterScope
     execDecls ds
     v0 <- eval e0
     leaveScope
     return v0
     
execDecl :: Name -> Exp -> IM ()
execDecl n e = do
  v <- eval e
  l <- createVar n
  updateStore l v

execDecls :: [(Name,Exp)] -> IM ()
execDecls  [] = return ()
execDecls ((n,e):ds) = execDecl n e >> execDecls ds

-- | Execute statements
exec :: Stmt -> IM ()
exec (SVar n) = createVar n >> return ()
exec (SBlock ss)= enterScope >> mapM_ exec ss >> leaveScope

exec (v := e) = do
  x <- eval e
  loc <- getNameLoc v
  updateStore loc x
exec (SExp e) = eval e >>= liftIO . print 
exec (SPrint e) = eval e >>= liftIO . print 
-- exec x = error $ "exec unimplemented for " ++ show x
