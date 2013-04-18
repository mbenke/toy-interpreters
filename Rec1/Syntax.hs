module Rec1.Syntax where
import Data.Map(Map)
import qualified Data.Map as Map

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
  
oneFieldRec :: Name -> Type -> Type
oneFieldRec a t = TRec $ Map.fromList [(a,t)]
