{-# LANGUAGE MultiParamTypeClasses #-}
module Tiny3.Interpreter where
import Tiny3.Syntax

import qualified Data.Map as Map
import Control.Monad.Error
import Control.Monad.State
import Control.Monad.Trans
import Control.Monad.Identity

type Store = Map.Map String Val
initStore :: Store
initStore = Map.empty

type Loc = Int

data IntState = IntState { store :: Store, freeLocs :: [Loc] }
initState = IntState initStore [1..2^16]

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

getVar :: String -> IM Val
getVar v =  do
  store <- getStore 
  let res  = Map.lookup v store
  maybe (throwError $ "Undefined var "++v) return res

updateStore :: String -> Val -> IM ()
updateStore v x = modify $ \state -> state { 
  store = Map.insert v x (store state)}

eval :: Exp -> IM Val
eval (EInt i) = return i
eval (EVar s) = getVar s
eval (EAdd e1 e2) = liftM2 (+) (eval e1) (eval e2)

exec :: Stmt -> IM ()
-- exec (SVar n) = return () -- FIXME
exec (SBlock ss)= mapM_ exec ss
exec (v := e) = do
  x <- eval e
  updateStore v x
exec (SExp e) = eval e >>= liftIO . print 
exec (SPrint e) = eval e >>= liftIO . print 
exec x = error $ "exec unimplemented for " ++ show x

test :: IO()
test = runProg prog1
