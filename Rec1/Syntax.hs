module Rec1.Syntax where
import Data.Map(Map)
import qualified Data.Map as Map
import Data.List(intersperse)

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

emptyRec :: Type
emptyRec = TRec Map.empty  
oneFieldRec :: Name -> Type -> Type
oneFieldRec a t = TRec $ Map.fromList [(a,t)]

instance Show Constraint where
  show (Constraint  (x,t)) = concat [x,"<",show t]
  
showRec :: RecType -> String 
showRec r = concat ["{",showFields fields,"}"] where
  fields = Map.toList r 
  showFields  = concat . intersperse ","  . map showField
  showField (l,t) = concat [l,":",show t]
  
instance Show Type where
  show TInt = "int"
  show (TRec r) = showRec r
  show (TVar v) = v
