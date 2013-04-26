module Rec1.Checker1 where
import Rec1.Syntax

import Data.Map(Map)
import qualified Data.Map as Map

import Control.Monad.Error
import Control.Monad.State
import Control.Monad.Identity

import Control.Applicative

-- * Constraints

-- newtype Constraint = Constraint (Name, Type)
type Constraints = Map Name Type
noConstraints :: Constraints
noConstraints = Map.empty

oneConstraint :: Name -> Type -> Constraints
oneConstraint n t = Map.fromList [(n,t)]

updateConstraints :: Constraints -> Constraints -> Constraints
updateConstraints psi1 psi2 = Map.foldrWithKey updateStep psi1 psi2 where
  updateStep :: Name -> Type -> Constraints -> Constraints
  updateStep n t psi = Map.insertWith updateRecs n t psi
  -- t1 \updconstr t2
  updateRecs :: Type -> Type -> Type
  updateRecs (TRec r1) (TRec r2) = TRec $ updateRecMaps r1 r2
  updateRecs t1       t2 = t2
  -- tr1 \updconstr tr2

updateRecMaps :: RecType -> RecType -> RecType
updateRecMaps m1 m2 = foldrRec rtUpdateField m1 m2

-- | rtUpdateField n t r = r \updconstr \{ n : t \}
rtUpdateField :: Name -> Type -> RecType -> RecType
rtUpdateField = Map.insert 

-- foldrWithKey :: (k -> a -> b -> b) -> b -> Map k a -> b
foldrRec :: (Name -> Type -> RecType -> RecType) -> RecType -> RecType -> RecType
foldrRec = Map.foldrWithKey

-- * Typing

data Typing  = Typing {tyPre :: Constraints,tyTy :: Type, tyPost :: Constraints}
  deriving Show

pureType :: Type -> Typing
pureType t = Typing noConstraints t noConstraints

-- * Checker Monad

type Env = Map Name Type

emptyEnv :: Env
emptyEnv = Map.empty

extendEnv :: Name -> Type -> Env -> Env
extendEnv x t env = Map.insert x t env

data CheckState = CheckState { 
  cstFresh :: Int
}

initState :: CheckState
initState = CheckState 0

cstTick :: CheckState -> CheckState
cstTick (CheckState i) = CheckState $ i+1

freshInt :: CM Int
freshInt = do
  modify cstTick
  gets cstFresh
         
freshName :: String -> CM Name
freshName s = (s++) . show <$> freshInt
          
type CM a = StateT CheckState (ErrorT String Identity) a
runCM :: CM a -> CheckState ->  (Either String (a,CheckState))
runCM m st = runIdentity $ runErrorT (runStateT m st)

evalCM :: CM a -> CheckState -> (Either String a)
evalCM m st = runIdentity $ runErrorT (evalStateT m st)

lookupEnv :: Name -> Env ->  CM Type
lookupEnv n e = case Map.lookup n e of
          Just t -> return t
          Nothing -> throwError (n ++ " not found in env")

findType :: Exp -> Env -> CM Typing
findType (EVar v) env = pureType <$> lookupEnv v env
findType (EInt _) env = return $ pureType TInt
findType (ENew)   env = typ <$> freshName "X" where
         typ x = Typing noConstraints (TVar x) (oneConstraint x emptyRec)

