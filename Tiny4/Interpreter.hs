{-# LANGUAGE MultiParamTypeClasses, NamedFieldPuns #-}

module Tiny4.Interpreter where
import Tiny4.Syntax

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
  res <- runIM (prepareRuntime >> exec p) initState
  case res of
    Left e -> putStrLn e
    Right (a,state) -> print $ Map.toAscList (store state)

-- * Runtime
prepareRuntime :: IM ()
prepareRuntime = do
  l1 <- new $ VStr "The base type"
  loc <- new $ mkRec [("__doc__",l1)]
  modifyEnv (updateEnv "object" loc)
  modifyScopes (addLocal "object")
  
-- * Values
    
data Val = VInt Integer | VStr String | VRec Record
         deriving Show
type Record = (Map.Map Name Loc)

emptyRec::Val
emptyRec = VRec Map.empty

mkRec :: [(String,Loc)] -> Val
mkRec dict = VRec $ Map.fromList dict

getVInt :: Val -> IM Integer
getVInt (VInt i) = return i
getVInt v = throwError $ "Not an integer: "++show v

getVRec :: Val -> IM Record
getVRec (VRec r) = return r
getVRec v = throwError $ "Not a record: "++show v

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

new :: Val -> IM Loc
new val = do -- alloc >>= \l -> updateStore l val >> return l
  loc <- alloc
  updateStore loc val
  return loc

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
  readLoc loc
  
readLoc :: Loc -> IM Val
readLoc loc = do
  store <- getStore 
  let res  = Map.lookup loc store
  maybe (throwError $ "Unallocated loc"++ show loc) return res

getFieldLoc :: Record -> Name -> (Maybe Loc)
getFieldLoc r n =  (Map.lookup n r)

-- | Evaluate expressions
eval :: Exp -> IM Val
eval (EInt i) = return $ VInt i
eval (EVar s) = getVar s
eval (EAdd e1 e2) = do
  i1 <- getVInt =<< eval e1 
  i2 <- getVInt =<< eval e2
  return $ VInt (i1+i2)                  
eval (EDot e n) = do
   v <- eval e
   r <- getVRec v
   maybe (throwError $ unwords [show v,"has no field",n]) 
         readLoc
         (getFieldLoc r n)
-- | Execute statements
exec :: Stmt -> IM ()
exec (SVar n) = do
  l <- alloc
  modifyEnv (updateEnv n l)
  modifyScopes (addLocal n)
  return ()
exec (SBlock ss)= enterScope >> mapM_ exec ss >> leaveScope

exec (lhs := e) = do
  loc <- lhsLoc lhs
  v <- eval e
  updateStore loc v

exec (SExp e) = eval e >>= liftIO . print 
exec (SPrint e) = eval e >>= liftIO . print 
-- exec x = error $ "exec unimplemented for " ++ show x

lhsLoc :: Lhs -> IM Loc
lhsLoc (LV v) = getNameLoc v
lhsLoc (LhsDot lhs n) = do
  rloc <- lhsLoc lhs
  v <- readLoc rloc
  r <- getVRec v
  case getFieldLoc r n of
    Just floc -> return floc
    Nothing -> do -- create field if not exists
      floc <- alloc
      let r' = Map.insert n floc r
      updateStore rloc (VRec r')
      return floc
  
test :: IO()
test = runProg prog1
