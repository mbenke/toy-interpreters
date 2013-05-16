module Rec1.TestChecker1 where
import Rec1.Checker1
import Rec1.Syntax
import Rec1.Parser

import Control.Monad
import qualified Data.Map as Map

-- * Tests and examples
testExp1 :: Exp
testExp1 = parseExp "testExp1" "1"
testExp3 = parseExp "testExp3" "let x = new in x.a = 1"

allTests = [test1, test2]
test1 = testFT (EInt 1) 
test2 = testFT ENew
test3 = testFT testExp3

testFT exp = evalCM (findType exp emptyEnv) initState
recT1, recT2 :: RecType
recT1 = Map.fromList [("a",TVar "Xa"),("b", TVar "Xb")]
recT2 = Map.fromList [("c",TVar "Yc"),("b", TVar "Yb")]
{-
*Rec1.Checker1> updateRecMaps recT1 recT2
fromList [("a",Xa),("b",Yb),("c",Yc)]
*Rec1.Checker1> updateRecMaps recT2 recT1
fromList [("a",Xa),("b",Xb),("c",Yc)]
-}
psi1 = Map.fromList [("Z1",TRec recT1),("Z2",emptyRec)]
psi2 = Map.fromList [("Z1",TRec recT2)]
psi3 = Map.fromList [("Z2",TRec recT2)]

runAllTests = runTests allTests

runTests = mapM_ print