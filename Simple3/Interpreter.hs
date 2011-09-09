{-# LANGUAGE MultiParamTypeClasses, NamedFieldPuns #-}
-- Tiny3.Interpreter2 extended with if and let

module Simple3.Interpreter where
import Simple3.Syntax

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

runProg :: Defs -> IO ()
runProg p = do
  res <- runIM (evalDefs p) initState
  case res of
    Left e -> putStrLn e
    Right (a,state) -> do
      print a
      printState state
    
printState :: IntState -> IO ()
printState state = do
  putStr "Env: "
  printEnv (env state)
  putStr "Store: "
  print $ Map.toAscList (store state)

-- * Values

data Val = VInt Integer | VLoc Loc | VNone deriving (Eq)

instance Show Val where
  show (VInt n) = show n
  show (VLoc l) = "loc:"++show l
  show VNone = "None"
  
getVInt :: Val -> IM Integer
getVInt (VInt i) = return i
getVInt v = throwError $ "Not an integer: "++show v

getVLoc :: Val -> IM Loc
getVLoc (VLoc l) = return l
getVLoc v = throwError $ "Not a loc: "++show v

isTrueVal :: Val -> Bool
isTrueVal (VInt 0) = False
isTrueVal (VInt _) = True
isTrueVal (VLoc _) = True
isTrueVal (VNone)  = False

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

getLocContents :: Loc -> IM Val
getLocContents loc = do
  store <- getStore 
  let res  = Map.lookup loc store
  maybe (throwError $ "Unknown loc: "++ show loc) return res

initFreeLocs :: [Loc]
initFreeLocs = [1..2^16]

getFreeLocs :: IM [Loc]
getFreeLocs = gets freeLocs

putFreeLocs :: [Loc] -> IM ()
putFreeLocs freeLocs = modify $ \r -> r { freeLocs }

-- * Names and Environment
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
  let scopeLocs = catMaybes $ map (flip lookupEnv env) scope
  mapM_ free scopeLocs
  let env' = foldr (\n e -> Map.update pop n e)env scope
  putEnv env' where
    pop :: [Loc] -> Maybe [Loc]
    pop [x] = Nothing
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
  l <- getNameLoc v
  getLocContents l

-- | Evaluate expressions
eval :: Exp -> IM Val
eval (EInt i) = return (VInt i)
eval (EVar s) = getVar s
eval (EAdd e1 e2) = do -- liftM2 (+) (eval e1) (eval e2)
  i1 <- getVInt =<< eval e1 
  i2 <- getVInt =<< eval e2
  return $ VInt (i1+i2)                  
eval (EIf e1 e2 e3) = do
     v1 <- eval e1
     if isTrueVal v1 then eval e2 else eval e3
eval (ELet "_" e1 e0) = eval e1 >> eval e0
eval (ELet n e1 e0) = do
     enterScope
     execDef n e1
     v0 <- eval e0
     leaveScope
     return v0
eval (ELets ds e0) = do
     enterScope
     execDefs ds
     v0 <- eval e0
     leaveScope
     return v0
eval (ENew e) = do     
     v <- eval e
     l <- alloc
     updateStore l v
     return $ VLoc l
eval (EDeref e) = do
  v <- eval e
  l <- getVLoc v
  getLocContents l
  
execDef :: Name -> Exp -> IM ()
execDef n e = do
  v <- eval e
  l <- createVar n
  updateStore l v

execDefs :: [(Name,Exp)] -> IM ()
execDefs  [] = return ()
execDefs ((n,e):ds) = execDef n e >> execDefs ds

evalDef :: Name -> Exp -> IM Val
evalDef n e = do
  v <- eval e
  l <- createVar n
  updateStore l v
  return v
  
evalDefs :: [(Name,Exp)] -> IM Val
evalDefs  [(n,e)] = evalDef n e
evalDefs ((n,e):ds) = evalDef n e >> evalDefs ds
