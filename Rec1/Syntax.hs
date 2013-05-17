module Rec1.Syntax where
import Data.Map(Map)
import qualified Data.Map as Map
import Data.List(intersperse,intercalate)

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

emptyRec :: Type
emptyRec = TRec Map.empty  
oneFieldRec :: Name -> Type -> Type
oneFieldRec a t = TRec $ Map.fromList [(a,t)]

showRec :: RecType -> String 
showRec r = concat ["{",showFields fields,"}"] where
  fields = Map.toList r 
  showFields  = concat . intersperse ","  . map showField
  showField (l,t) = concat [l,":",show t]
  
instance Show Type where
  show TInt = "int"
  show (TRec r) = showRec r
  show (TVar v) = v

-- * Constraints

-- newtype Constraint = Constraint (Name, Type)
type Constraints = Map Name Type
noConstraints :: Constraints
noConstraints = Map.empty

oneConstraint :: Name -> Type -> Constraints
oneConstraint n t = Map.fromList [(n,t)]

showConstraints ::  Constraints  -> String
showConstraints = showConList . Map.toList

showConList :: [(Name,Type)] -> String
showConList = intercalate "," . map showCon

showCon :: (Name,Type) -> String
showCon (x,t) = concat [x,"<",show t] 

-- ** Updating Constraints

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

