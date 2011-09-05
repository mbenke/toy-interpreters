module Tiny3.Test where
import Tiny3.Syntax
import qualified Tiny3.Interpreter as Interpreter1
import qualified Tiny3.Interpreter2 as Interpreter2

test :: IO ()
test = do
  testInterpreter1
  testInterpreter2

prog1 :: Stmt
prog1 = SBlock [ 
  SVar "x",
  SVar "y",
  "x" := 1,
  SBlock [ SVar "x", "x" := 42 ], -- local variable
  "y" := 2,
  SPrint (EVar "x"+ EVar "y")
  ]

-- undefined var
bad1 = SExp $ EVar "x"

testInterpreter :: (Stmt -> IO ()) -> IO ()
testInterpreter i = i prog1

testInterpreter1 = testInterpreter Interpreter1.runProg
testInterpreter2 = testInterpreter Interpreter2.runProg
