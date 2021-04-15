{-# LANGUAGE MultiParamTypeClasses #-}
module Tiny2.Interpreter where
import Tiny2.Syntax

import qualified Data.Map as Map
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Trans
import Control.Monad.Identity

type Store = Map.Map String Val
initStore :: Store
initStore = Map.empty

type IntState = Store

type IM a = StateT IntState (ExceptT String IO) a
runIM :: IM a -> IntState -> IO (Either String (a,IntState))
runIM m st = runExceptT (runStateT m st)

runProg :: Stmt -> IO ()
runProg p = do
  res <- runIM (exec p) initStore 
  case res of
    Left e -> putStrLn e
    Right ((),store) -> print $ Map.toAscList store

  
getVar :: String -> IM Val
getVar v =  gets (Map.lookup v) >>= 
            maybe (throwError $ "Undefined var "++v) return
  
eval :: Exp -> IM Val
eval (EInt i) = return i
eval (EVar s) = getVar s
eval (EAdd e1 e2) = liftM2 (+) (eval e1) (eval e2)

exec :: Stmt -> IM ()
exec (SBlock ss)= mapM_ exec ss
exec (v := e) = do
  x <- eval e
  modify (Map.insert v x)
exec (SExp e) = eval e >> return ()
exec (SPrint e) = eval e >>= liftIO . print 
