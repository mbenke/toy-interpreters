{-# LANGUAGE MultiParamTypeClasses #-}
module Tiny1.Interpreter where
import Tiny1.Syntax

import qualified Data.Map as Map
import Control.Monad.Error
import Control.Monad.State
import Control.Monad.Trans

type Store = Map.Map String Val
initStore :: Store
initStore = Map.empty

type IntState = Store

type IM = StateT IntState (Either String)
runIM :: IM a -> IntState -> Either String (a,IntState)
runIM m st = runStateT m st

runProg :: Stmt -> String
runProg p = case runIM (exec p) initStore of
  Left e -> e
  Right ((),store) -> show $ Map.toAscList store
  
getVar :: String -> IM Val
getVar v =  do
  r <- gets (Map.lookup v)
  maybe (throwError $ "Undefined var "++v) return r
  
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

test :: IO()
test = putStrLn $ runProg prog1
