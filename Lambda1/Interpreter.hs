module Lambda1.Interpreter where
import Lambda1.Syntax
import qualified Data.Map as Map
import Data.Map(Map)
import Data.Maybe(fromMaybe)
import Control.Monad.Reader

data Value = Vint Int
           | Vclos Exp Env
             deriving Show

type Env = Map Name Value

eval :: Exp -> Env -> Value
eval (EInt i) env =  (Vint i)
eval (EVar v) env = fromMaybe (error "unknown variable") (Map.lookup v env) 
eval e@(ELam _ _) env = Vclos e env
eval (EApp e1 e2) env = apply (eval e1 env) (eval e2 env)

apply :: Value -> Value -> Value
apply (Vclos (ELam x e1) env) e2 = eval e1 (Map.insert x e2 env) 

